/* -------- */
/* Prologue */
/* -------- */
%{

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "command/commands.h"
#include "parser/parser.h"
#include "parser/tree.h"

/* Prototypes */
int CommandParser_lex(void);
void CommandParser_error(char *s);

/* Local Variables */
Dnchar tokenName;
List<Dnchar> stepNameStack;
VTypes::DataType declaredType, funcType;
TreeNode *tempNode;
int globalDeclarations;

%}

// Redeclare function names
%name-prefix="CommandParser_"

/* Type Definition */
%union {
	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *tree;			/* function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */
};

%token <intconst> INTCONST ELEMENTCONST
%token <doubleconst> DOUBLECONST
%token <name> NEWTOKEN CHARCONST STEPTOKEN
%token <variable> VAR LOCALVAR
%token <functionId> FUNCCALL
%token <tree> USERFUNCCALL
%token <vtype> VTYPE
%token ATEN_DO ATEN_WHILE ATEN_FOR ATEN_SWITCH ATEN_CASE ATEN_DEFAULT ATEN_IF ATEN_IIF ATEN_IN ATEN_GLOBAL ATEN_RETURN FILTERBLOCK HELP ATEN_VOID ATEN_CONTINUE ATEN_BREAK
%nonassoc ATEN_ELSE

/* Higher line number == Higher precedence */
/* Taken from cppreference.com */
%right '=' PEQ MEQ TEQ DEQ 
%right '?' ':'
%left OR
%left AND
%left EQ NEQ
%left '<' LEQ '>' GEQ
%left '+' '-'
%left '*' '/' '%' '^'
%right UPLUS UMINUS '!'
%left PLUSPLUS MINUSMINUS
 
/* %left AND OR */
/* %left '=' PEQ MEQ TEQ DEQ  */
/* %left GEQ LEQ EQ NEQ '>' '<' */
/* %left '+' '-' */
/* %left '*' '/' '%'  */
/* %right UMINUS '?' ':' */
/* %left PLUSPLUS MINUSMINUS */
/* %right '!' */
/* %right '^' */

%type <node> constant expression expressionlist variable statement flowstatement statementlist block blockment assignment
%type <node> declaration functiondeclaration caselabel caselist
%type <node> ARRAYCONST function userfunction assignedvariablename variablelistitem variablelist typedvariablelistitem typedvariablelist
%type <name> variablename
%type <node> pushscope
%type <tree> pushfunc

%%

/* ------------------------- */
/* Main Program Construction */
/* ------------------------- */

/* Program List */
programlist:
	program						{ }
	| programlist program				{ }
	;

/* Single Program 'Statement' */
program:
	statementlist					{
		if (($1 != NULL) && (!cmdparser.addStatement($1))) YYABORT;
		}
	| block						{
		if (($1 != NULL) && (!cmdparser.addStatement($1))) YYABORT;
		}
	;

/* --------- */
/* Constants */
/* --------- */

constant:
	INTCONST					{ $$ = cmdparser.addConstant($1); }
	| DOUBLECONST					{ $$ = cmdparser.addConstant($1); }
	| CHARCONST					{ $$ = cmdparser.addConstant($1->get()); }
	| ELEMENTCONST					{ $$ = cmdparser.addElementConstant($1); }
	;

/* ----------------- */
/* Variables & Paths */
/* ----------------- */

/* Single Path Step */
step:
	STEPTOKEN pushstepname '[' expression ']' 	{
		if (!cmdparser.expandPath(stepNameStack.last(), $4)) YYABORT;
		stepNameStack.removeLast();
		}
	| STEPTOKEN pushstepname '(' expressionlist ')' {
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, $4)) YYABORT;
		stepNameStack.removeLast();
		}
	| STEPTOKEN pushstepname '(' ')' 		{
		if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT;
		stepNameStack.removeLast();
		}
	| STEPTOKEN pushstepname 			{
		if (!cmdparser.expandPath($1)) YYABORT;
		stepNameStack.removeLast();
		}
	;

/* Multiple Step Path */
steplist:
	step 						{ }
	| steplist '.' step				{ }
	| steplist error				{ msg.print("Error formulating path.\n"); YYABORT; }
	;

/* Pre-Existing Variable */
variable:
	VAR '[' expression ']'				{
		$$ = cmdparser.wrapVariable($1,$3);
		if ($$ == NULL) { msg.print("Error in variable expression (code 1)\n"); YYABORT; }
		}
	| VAR						{
		$$ = cmdparser.wrapVariable($1);
		if ($$ == NULL) { msg.print("Error in variable expression (code 2)\n"); YYABORT; }
		}
	| LOCALVAR '[' expression ']'			{
		$$ = cmdparser.wrapVariable($1,$3);
		if ($$ == NULL) { msg.print("Error in variable expression (code 3)\n"); YYABORT; }
		}
	| LOCALVAR					{
		$$ = cmdparser.wrapVariable($1);
		if ($$ == NULL) { msg.print("Error in variable expression (code 4)\n"); YYABORT; }
		}
	| variable '.' 					{
		cmdparser.createPath($1);
		} steplist {
		$$ = cmdparser.finalisePath();
		}
	| variable '('					{
		msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); $$ = NULL;
		}
	;

/* -------------- */
/* Function Calls */
/* -------------- */

/* Built-In Functions */
function:
	FUNCCALL '(' ')'				{
		$$ = cmdparser.addFunction( (Command::Function) $1);
		if ($$ == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : function : function '%s'\n", commands.data[(Command::Function) $1].keyword);
		}
	| FUNCCALL '(' expressionlist ')'		{
		$$ = cmdparser.addFunctionWithArglist( (Command::Function) $1,$3);
		if ($$ == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : function : function '%s' with exprlist\n", commands.data[(Command::Function) $1].keyword);
		}
	| FUNCCALL error				{
		msg.print("Error: Missing brackets after function call?\n");
		YYABORT;
		}
	;

/* User-Defined Functions */
userfunction:
	USERFUNCCALL '(' ')'				{
		$$ = cmdparser.addUserFunction($1);
		if ($$ == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : userfunction : function '%s'\n", $1->name());
		}
	| USERFUNCCALL '(' expressionlist ')'		{
		$$ = cmdparser.addUserFunction($1,$3);
		if ($$ == NULL) YYABORT;
		msg.print(Messenger::Parse,"PARSER : userfunction : function '%s' with expressionlist\n", $1->name());
		}
	| USERFUNCCALL error				{
		msg.print("Error: Missing brackets after function call?\n");
		YYABORT;
		}
	;

/* ------------ */
/* Misc Objects */
/* ------------ */

/* Array Vector Constant / Assignment Group */
ARRAYCONST:
	'{' expressionlist '}'				{
		$$ = cmdparser.addArrayConstant($2);
		if ($$ == NULL) YYABORT;
		}
	;

/* ----------- */
/* Expressions */
/* ----------- */

assignment:
	variable '=' expression				{ $$ = cmdparser.addOperator(Command::OperatorAssignment,$1,$3); if ($$ == NULL) YYABORT; }
	| variable '=' ARRAYCONST			{ $$ = cmdparser.addOperator(Command::OperatorAssignment,$1,$3); if ($$ == NULL) YYABORT; }
	| variable '=' error				{ msg.print("Mangled expression used in assignment.\n"); YYABORT; }
	;

/* Expression */
expression:
	constant					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| function					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| userfunction					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| variable PEQ expression			{ $$ = cmdparser.addOperator(Command::OperatorAssignmentPlus,$1,$3); if ($$ == NULL) YYABORT; }
	| variable MEQ expression			{ $$ = cmdparser.addOperator(Command::OperatorAssignmentSubtract,$1,$3); if ($$ == NULL) YYABORT; }
	| variable TEQ expression			{ $$ = cmdparser.addOperator(Command::OperatorAssignmentMultiply,$1,$3); if ($$ == NULL) YYABORT; }
	| variable DEQ expression			{ $$ = cmdparser.addOperator(Command::OperatorAssignmentDivide,$1,$3); if ($$ == NULL) YYABORT; }
	| '-' expression %prec UMINUS			{ $$ = cmdparser.addOperator(Command::OperatorNegate, $2); if ($$ == NULL) YYABORT; }
	| variable PLUSPLUS				{ $$ = cmdparser.addOperator(Command::OperatorPostfixIncrease, $1);  if ($$ == NULL) YYABORT; }
	| variable MINUSMINUS				{ $$ = cmdparser.addOperator(Command::OperatorPostfixDecrease, $1); if ($$ == NULL) YYABORT; }
	| PLUSPLUS variable				{ $$ = cmdparser.addOperator(Command::OperatorPrefixIncrease, $2); if ($$ == NULL) YYABORT; }
	| MINUSMINUS variable				{ $$ = cmdparser.addOperator(Command::OperatorPrefixDecrease, $2); if ($$ == NULL) YYABORT; }
	| variable					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| expression '+' expression			{ $$ = cmdparser.addOperator(Command::OperatorAdd, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '-' expression			{ $$ = cmdparser.addOperator(Command::OperatorSubtract, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '*' expression			{ $$ = cmdparser.addOperator(Command::OperatorMultiply, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '/' expression			{ $$ = cmdparser.addOperator(Command::OperatorDivide, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '^' expression			{ $$ = cmdparser.addOperator(Command::OperatorPower, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '%' expression			{ $$ = cmdparser.addOperator(Command::OperatorModulus, $1, $3); if ($$ == NULL) YYABORT; }
	| expression EQ expression			{ $$ = cmdparser.addOperator(Command::OperatorEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression NEQ expression			{ $$ = cmdparser.addOperator(Command::OperatorNotEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '>' expression			{ $$ = cmdparser.addOperator(Command::OperatorGreaterThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expression GEQ expression			{ $$ = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '<' expression			{ $$ = cmdparser.addOperator(Command::OperatorLessThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expression LEQ expression			{ $$ = cmdparser.addOperator(Command::OperatorLessThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression AND expression			{ $$ = cmdparser.addOperator(Command::OperatorAnd, $1, $3); if ($$ == NULL) YYABORT; }
	| expression OR expression			{ $$ = cmdparser.addOperator(Command::OperatorOr, $1, $3); if ($$ == NULL) YYABORT; }
	| '(' expression ')'				{ $$ = $2; if ($$ == NULL) YYABORT; }
	| '!' expression				{ $$ = cmdparser.addOperator(Command::OperatorNot, $2); if ($$ == NULL) YYABORT; }
	| expression '?' expression ':' expression	{ $$ = cmdparser.addOperator(Command::OperatorInlineIf, $1, $3, $5); if ($$ == NULL) YYABORT; }
	| NEWTOKEN					{ msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
	;

/* Expression LIst */
expressionlist:
	expression					{
		$$ = $1;
		if ($$ == NULL) YYABORT;
		}
	| expressionlist ',' expression			{
		$$ = Tree::joinArguments($3,$1);
		}
	| expressionlist expression			{
		msg.print("Error: Missing comma between items.\n");
		YYABORT;
		}
	;

/* ----------------------------- */
/* New Variables and Declaration */
/* ----------------------------- */

/* Conversion of allowable names to single token type */
variablename:
	VAR 						{
		msg.print(Messenger::Parse,"PARSER : variablename : existing var '%s'\n", tokenName.get());
		tokenName = yylval.variable->name();
		$$ = &tokenName;
		}
	| FUNCCALL					{
		msg.print(Messenger::Parse,"PARSER : variablename : existing built-in function '%s'\n", tokenName.get());
		tokenName = Command::data[yylval.functionId].keyword;
		$$ = &tokenName;
		}
	| LOCALVAR					{
		msg.print(Messenger::Parse,"PARSER : variablename : existing local var '%s'\n", tokenName.get());
		msg.print("Error: Existing variable in local scope cannot be redeclared.\n");
		YYABORT;
		}
	| constant					{
		msg.print(Messenger::Parse,"PARSER : variablename : constant '%s'\n", tokenName.get());
		msg.print("Error: Constant value found in declaration.\n");
		YYABORT;
		}
	| USERFUNCCALL					{
		msg.print(Messenger::Parse,"PARSER : variablename : existing user function '%s'\n", tokenName.get());
		msg.print("Error: Existing user-defined function name cannot be redeclared.\n");
		YYABORT;
		}
	| VTYPE						{
		msg.print(Messenger::Parse,"PARSER : variablename : variable type-name '%s'\n", VTypes::dataType( yylval.vtype));
		msg.print("Error: Type-name used in variable declaration.\n");
		YYABORT;
		}
	| NEWTOKEN savetokenname			{
		msg.print(Messenger::Parse,"PARSER : variablename : new token '%s'\n", tokenName.get());
		if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; }
		$$ = $1;
		}
	;

/* Variable name with assigned value */
assignedvariablename:
	variablename '=' ARRAYCONST			{
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with array assignment\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName, $3, globalDeclarations);
		}
	| variablename '[' expression ']' '=' expression {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with expr assignment\n", tokenName.get());
		$$ = cmdparser.addArrayVariable(declaredType, &tokenName, $3, $6, globalDeclarations);
		}
	| variablename '[' expression ']' '=' ARRAYCONST {
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with array assignment\n", tokenName.get());
		$$ = cmdparser.addArrayVariable(declaredType, &tokenName, $3, $6, globalDeclarations);
		}
	| variablename '=' expression 			{
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with expr assignment\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName, $3, globalDeclarations);
		}
	;

/* Variable List Item */
variablelistitem:
	variablename					{
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : var '%s'\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName, NULL, globalDeclarations);
		}
	| variablename '[' expression ']' 		{
		msg.print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s'\n", tokenName.get());
		$$ = cmdparser.addArrayVariable(declaredType, &tokenName, $3, NULL, globalDeclarations);
		}
	| assignedvariablename 				{
		$$ = $1;
		}
	;

/* Variable List Single */
variablelist:
	variablelistitem				{
		$$ = $1;
		}
	| variablelist ',' variablelistitem		{
		$$ = Tree::joinArguments($3,$1);
		}
	| variablelist variablelistitem			{
		msg.print("Error: Missing comma between declarations?\n");
		YYABORT;
		}
	;

/* Typed Variable List Single */
typedvariablelistitem:
	VTYPE savetype variablename			{
		msg.print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s'\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName);
		}
	| VTYPE savetype variablename '=' expression 	{
		msg.print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s' with expr assignment\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName, $5);
		}
	;

/* Typed Variable List */
typedvariablelist:
	typedvariablelistitem				{
		$$ = $1;
		}
	| typedvariablelist ',' typedvariablelistitem	{
		$$ = Tree::joinArguments($3,$1);
		}
	| typedvariablelist typedvariablelistitem	{
		msg.print("Error: Missing comma between declarations?\n");
		YYABORT;
		}
	;

/* Variable Declaration Statement */
declaration:
	ATEN_GLOBAL setglobal VTYPE savetype variablelist unsetglobal	{
		msg.print(Messenger::Parse,"PARSER : global declaration : standard variable declaration list\n");
		$$ = cmdparser.addDeclarations($5);
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype variablelist			{
		msg.print(Messenger::Parse,"PARSER : declaration : standard variable declaration list\n");
		$$ = cmdparser.addDeclarations($3);
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype error				{
		msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType));
		YYABORT;
		}
	;

/* -------------------- */
/* Function Declaration */
/* -------------------- */

/* User-Defined Function Declaration */
functiondeclaration:
	ATEN_VOID cleartype NEWTOKEN pushfunc '(' ')' block {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, no arguments)\n");
		if (!cmdparser.addStatement($7)) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
	| ATEN_VOID cleartype NEWTOKEN pushfunc '(' typedvariablelist ')' {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, arguments)\n");
		if (!$4->addLocalFunctionArguments($6)) YYABORT;
		} block {
		if (!cmdparser.addStatement($9)) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype NEWTOKEN pushfunc '(' ')' block {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, no arguments)\n", VTypes::dataType($4->returnType()));
		if (!cmdparser.addStatement($7)) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype NEWTOKEN pushfunc '(' typedvariablelist ')' {
		msg.print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, arguments)\n", VTypes::dataType($4->returnType()));
		if (!$4->addLocalFunctionArguments($6)) YYABORT;
		} block {
		if (!cmdparser.addStatement($9)) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
	;

/* ---------- */
/* Statements */
/* ---------- */

/* Single Statement */
statement:
	assignment ';'					{
		$$ = $1;
		}
	| declaration ';'				{
		$$ = $1;
		}
	| expression ';'				{
		$$ = $1;
		}
	| function ';'					{
		$$ = $1;
		}
	| userfunction ';'				{
		$$ = $1;
		}
	| flowstatement					{
		$$ = $1;
		}
	| functiondeclaration				{
		$$ = NULL;
		}
	| filter					{
		$$ = NULL;
		}
	| HELP FUNCCALL					{
		$$ = cmdparser.addFunction(Command::Help, cmdparser.addConstant($2));
		}
	| ATEN_RETURN expression ';'			{
		$$ = cmdparser.addFunction(Command::Return,$2);
		}
	| ATEN_RETURN ';'				{
		$$ = cmdparser.addFunction(Command::Return);
		}
	| ATEN_CONTINUE ';'				{
		$$ = cmdparser.addFunction(Command::Continue);
		}
	| ATEN_BREAK ';'				{
		$$ = cmdparser.addFunction(Command::Break);
		}
	;

/* Statement List */
statementlist:
	statement					{
		$$ = $1;
		}
	| statementlist statement			{
		if ($2 == NULL) $$ = $1;
		else $$ = cmdparser.joinCommands($1, $2);
		}
	;

/* Block Statement */
block:
	'{' pushscope statementlist '}' popscope	{
		$$ = $3;
		}
	| '{' '}'					{
		$$ = cmdparser.addFunction(Command::NoFunction);
		}
	;

/* Block or Statement, but not Statement List */
blockment:
	statement					{
		$$ = $1;
		}
	| block						{
		$$ = $1;
		}
	;

/* Flow-Control Statement */
flowstatement:
	ATEN_IF '(' expression ')' blockment ATEN_ELSE blockment 	{
		$$ = cmdparser.addFunction(Command::If,$3,$5,$7);
		}
	| ATEN_IF '(' expression ')' blockment 			{
		$$ = cmdparser.addFunction(Command::If,$3,$5);
		}
	| ATEN_FOR pushscope '(' assignment ';' expression ';' expression ')' blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::For, $4,$6,$8,$10)); cmdparser.popScope();
		}
	| ATEN_FOR pushscope '(' declaration ';' expression ';' expression ')' blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::For, $4,$6,$8,$10)); cmdparser.popScope();
		}
	| ATEN_FOR pushscope '(' variable ATEN_IN expression ')'	{
		if ($4->returnType() <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		if ($4->returnType() != $6->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		} blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::ForIn,$4,$6,$9));
		cmdparser.popScope();
		}
	| ATEN_FOR pushscope '(' VTYPE savetype variablename ATEN_IN expression ')' { 
		if (declaredType <= VTypes::VectorData)
		{
			msg.print("Error: For/In loop variable must be of pointer type.\n");
			YYABORT;
		}
		tempNode = cmdparser.addVariable(declaredType, &tokenName);
		if (declaredType != $8->returnType())
		{
			msg.print("Error: For/In loop variable is not being assigned the correct type.\n");
			YYABORT;
		}
		} blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::ForIn,tempNode,$8,$11));
		cmdparser.popScope();
		}
	| ATEN_WHILE pushscope '(' expression ')' blockment	{
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::While, $4,$6));
		cmdparser.popScope();
		}
	| ATEN_DO pushscope block ATEN_WHILE '(' expression ')' ';' {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::DoWhile, $3,$6));
		cmdparser.popScope();
		}
	| ATEN_SWITCH '(' expression ')' 		{
		if (($3->returnType() != VTypes::IntegerData) && ($3->returnType() != VTypes::StringData))
		{
			msg.print("Error: Switch value must be of integer or string type.\n");
			YYABORT;
		}
		} '{' caselist '}' {
		$$ = cmdparser.addFunction(Command::Switch, $3);
		$$->addJoinedArguments($7);
		}
	;

/* Switch Statement Case/Default Label */
caselabel:
	ATEN_CASE '(' expression ')' ':'		{
		if (($3->returnType() != VTypes::IntegerData) && ($3->returnType() != VTypes::StringData))
		{
			msg.print("Error: Case value must be of integer or string type.\n");
			YYABORT;
		}
		$$ = cmdparser.addFunction(Command::Case, $3);
		if ($$ == NULL) { msg.print("Error: Invalid case expression.\n"); YYABORT; }
		}
	| ATEN_DEFAULT ':'				{
		$$ = cmdparser.addFunction(Command::Default);
		}
	;

/* Switch Statement Case List */
caselist:
	caselabel 					{
		$$ = $1;
		}
	| caselist statementlist			{
		$$ = Tree::joinArguments($2,$1);
		}
	| caselist caselabel				{
		$$ = Tree::joinArguments($2,$1);
		}
	;

/* ------- */
/* Filters */
/* ------- */

/* Filter Options */
filteroptions:
	NEWTOKEN savetokenname '=' constant		{
		if (!cmdparser.setFilterOption(&tokenName, $4)) YYABORT;
		msg.print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'\n", tokenName.get());
		}
	| filteroptions ',' NEWTOKEN savetokenname '=' constant {
		if (!cmdparser.setFilterOption(&tokenName, $6)) YYABORT;
		msg.print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'\n", tokenName.get());
		}
	;

/* Filter Definition */
filter:
	FILTERBLOCK pushfilter '(' filteroptions ')' block 	{
		if (($6 != NULL) && (!cmdparser.addStatement($6))) YYABORT;
		cmdparser.popTree();
		msg.print(Messenger::Parse,"PARSER : completed filter definition\n");
		}
	;

/* -------------------------- */
/* Semantic Value Subroutines */
/* -------------------------- */

savetokenname:
	/* empty */					{ tokenName = *yylval.name; }
	;

savetype:
	/* empty */					{ declaredType = yylval.vtype; }
	;

cleartype:
	/* empty */					{ declaredType = VTypes::NoData; }
	;

pushscope:
	/* empty */					{ $$ = cmdparser.pushScope(); if ($$ == NULL) YYABORT; }
	;

popscope:
	/* empty */					{ if (!cmdparser.popScope()) YYABORT; }
	;

pushstepname:
	/* empty */					{ stepNameStack.add()->set(yylval.name->get()); }
	;

pushfunc:
	/* empty */					{
		msg.print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'\n", yylval.name->get());
		$$ = cmdparser.pushFunction(yylval.name->get(), declaredType);
		/*cmdparser.pushScope();*/
		}
	;

pushfilter:
	/* empty */					{
		msg.print(Messenger::Parse,"PARSER : pushfilter : new filter definition\n");
		cmdparser.pushFilter();
		}
	;

setglobal:
	/* empty */					{ globalDeclarations = TRUE; }
	;

unsetglobal:
	/* empty */					{ globalDeclarations = FALSE; }
	;

%%

void yyerror(char *s)
{
}
