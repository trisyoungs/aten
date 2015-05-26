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
#include <QtCore/QStringList>

/* Prototypes */
int CommandParser_lex(void);
void CommandParser_error(char *s);

ATEN_USING_NAMESPACE

/* Local Variables */
QString tokenName;
QStringList stepNameStack;
VTypes::DataType declaredType, funcType;
TreeNode* tempNode;
int globalDeclarations;
QString variableName;

%}

// Redeclare function names
%name-prefix="CommandParser_"

/* Type Definition */
%union {
	int functionId;			/* Function enum id */
	TreeNode* node;			/* node pointer */
	Variable* variable;		/* variable pointer */
	Tree* tree;			/* function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intConst;			/* integer constant value */
	double doubleConst;		/* double constant value */
};

%token <intConst> INTCONST ELEMENTCONST
%token <doubleConst> DOUBLECONST
%token <name> NEWTOKEN CHARCONST STEPTOKEN
%token <variable> VAR VARSAMESCOPE
%token <functionId> FUNCCALL
%token <tree> USERFUNCCALL
%token <vtype> VTYPE
%token ATEN_DO ATEN_WHILE ATEN_FOR ATEN_SWITCH ATEN_CASE ATEN_DEFAULT ATEN_IF ATEN_IIF ATEN_IN ATEN_GLOBAL ATEN_RETURN FILTERBLOCK HELP ATEN_VOID ATEN_CONTINUE ATEN_BREAK ATEN_NEW
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
%left '*' '/' '%'
%right UPLUS UMINUS '!'
%left '^'
%left PLUSPLUS MINUSMINUS
 
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
	| CHARCONST					{ $$ = cmdparser.addConstant(cmdparser.lexedName()); }
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
		if (!cmdparser.expandPath(cmdparser.lexedName())) YYABORT;
		stepNameStack.removeLast();
		}
	;

/* Multiple Step Path */
steplist:
	step 						{ }
	| steplist '.' step				{ }
	| steplist error				{ Messenger::print("Error formulating path."); YYABORT; }
	;

/* Pre-Existing Variable */
variable:
	VAR '[' expression ']'				{
		$$ = cmdparser.wrapVariable($1,$3);
		if ($$ == NULL) { Messenger::print("Error in variable expression (code 1)"); YYABORT; }
		}
	| VAR						{
		$$ = cmdparser.wrapVariable($1);
		if ($$ == NULL) { Messenger::print("Error in variable expression (code 2)"); YYABORT; }
		}
	| VARSAMESCOPE '[' expression ']'			{
		$$ = cmdparser.wrapVariable($1,$3);
		if ($$ == NULL) { Messenger::print("Error in variable expression (code 3)"); YYABORT; }
		}
	| VARSAMESCOPE					{
		$$ = cmdparser.wrapVariable($1);
		if ($$ == NULL) { Messenger::print("Error in variable expression (code 4)"); YYABORT; }
		}
	| variable '.' 					{
		cmdparser.createPath($1);
		} steplist {
		$$ = cmdparser.finalisePath();
		}
	| variable '('					{
		Messenger::print("Can't use a variable as a function. Did you mean '[' instead?"); $$ = NULL;
		}
	;

/* -------------- */
/* Function Calls */
/* -------------- */

/* Built-In Functions */
function:
	FUNCCALL '(' ')'				{
		$$ = cmdparser.addFunction( (Commands::Function) $1);
		if ($$ == NULL) YYABORT;
		Messenger::print(Messenger::Parse, "PARSER : function : function '%s'", Commands::command((Commands::Function) $1));
		}
	| FUNCCALL '(' expressionlist ')'		{
		$$ = cmdparser.addFunctionWithArglist( (Commands::Function) $1,$3);
		if ($$ == NULL) YYABORT;
		Messenger::print(Messenger::Parse, "PARSER : function : function '%s' with exprlist", Commands::command((Commands::Function) $1));
		}
	| FUNCCALL error				{
		Messenger::print("Error: Missing brackets after function call?");
		YYABORT;
		}
	;

/* User-Defined Functions */
userfunction:
	USERFUNCCALL '(' ')'				{
		$$ = cmdparser.addUserFunction($1);
		if ($$ == NULL) YYABORT;
		Messenger::print(Messenger::Parse,"PARSER : userfunction : function '%s'", qPrintable($1->name()));
		}
	| USERFUNCCALL '(' expressionlist ')'		{
		$$ = cmdparser.addUserFunction($1,$3);
		if ($$ == NULL) YYABORT;
		Messenger::print(Messenger::Parse,"PARSER : userfunction : function '%s' with expressionlist", qPrintable($1->name()));
		}
	| USERFUNCCALL error				{
		Messenger::print("Error: Missing brackets after function call?");
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
	variable '=' expression				{ $$ = cmdparser.addOperator(Commands::OperatorAssignment,$1,$3); if ($$ == NULL) YYABORT; }
	| variable '=' ARRAYCONST			{ $$ = cmdparser.addOperator(Commands::OperatorAssignment,$1,$3); if ($$ == NULL) YYABORT; }
	| variable '=' error				{ Messenger::print("Mangled expression used in assignment."); YYABORT; }
	;

/* Expression */
expression:
	constant					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| function					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| userfunction					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| variable PEQ expression			{ $$ = cmdparser.addOperator(Commands::OperatorAssignmentPlus,$1,$3); if ($$ == NULL) YYABORT; }
	| variable MEQ expression			{ $$ = cmdparser.addOperator(Commands::OperatorAssignmentSubtract,$1,$3); if ($$ == NULL) YYABORT; }
	| variable TEQ expression			{ $$ = cmdparser.addOperator(Commands::OperatorAssignmentMultiply,$1,$3); if ($$ == NULL) YYABORT; }
	| variable DEQ expression			{ $$ = cmdparser.addOperator(Commands::OperatorAssignmentDivide,$1,$3); if ($$ == NULL) YYABORT; }
	| '-' expression %prec UMINUS			{ $$ = cmdparser.addOperator(Commands::OperatorNegate, $2); if ($$ == NULL) YYABORT; }
	| variable PLUSPLUS				{ $$ = cmdparser.addOperator(Commands::OperatorPostfixIncrease, $1);  if ($$ == NULL) YYABORT; }
	| variable MINUSMINUS				{ $$ = cmdparser.addOperator(Commands::OperatorPostfixDecrease, $1); if ($$ == NULL) YYABORT; }
	| PLUSPLUS variable				{ $$ = cmdparser.addOperator(Commands::OperatorPrefixIncrease, $2); if ($$ == NULL) YYABORT; }
	| MINUSMINUS variable				{ $$ = cmdparser.addOperator(Commands::OperatorPrefixDecrease, $2); if ($$ == NULL) YYABORT; }
	| variable					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| expression '+' expression			{ $$ = cmdparser.addOperator(Commands::OperatorAdd, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '-' expression			{ $$ = cmdparser.addOperator(Commands::OperatorSubtract, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '*' expression			{ $$ = cmdparser.addOperator(Commands::OperatorMultiply, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '/' expression			{ $$ = cmdparser.addOperator(Commands::OperatorDivide, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '^' expression			{ $$ = cmdparser.addOperator(Commands::OperatorPower, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '%' expression			{ $$ = cmdparser.addOperator(Commands::OperatorModulus, $1, $3); if ($$ == NULL) YYABORT; }
	| expression EQ expression			{ $$ = cmdparser.addOperator(Commands::OperatorEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression NEQ expression			{ $$ = cmdparser.addOperator(Commands::OperatorNotEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '>' expression			{ $$ = cmdparser.addOperator(Commands::OperatorGreaterThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expression GEQ expression			{ $$ = cmdparser.addOperator(Commands::OperatorGreaterThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '<' expression			{ $$ = cmdparser.addOperator(Commands::OperatorLessThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expression LEQ expression			{ $$ = cmdparser.addOperator(Commands::OperatorLessThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression AND expression			{ $$ = cmdparser.addOperator(Commands::OperatorAnd, $1, $3); if ($$ == NULL) YYABORT; }
	| expression OR expression			{ $$ = cmdparser.addOperator(Commands::OperatorOr, $1, $3); if ($$ == NULL) YYABORT; }
	| '(' expression ')'				{ $$ = $2; if ($$ == NULL) YYABORT; }
	| '!' expression				{ $$ = cmdparser.addOperator(Commands::OperatorNot, $2); if ($$ == NULL) YYABORT; }
	| expression '?' expression ':' expression	{ $$ = cmdparser.addOperator(Commands::OperatorInlineIf, $1, $3, $5); if ($$ == NULL) YYABORT; }
	| ATEN_NEW VTYPE				{ $$ = cmdparser.addNew(yylval.vtype); if ($$ == NULL) YYABORT; }
	| NEWTOKEN					{ Messenger::print("Error: '%s' has not been declared as a function or a variable.", qPrintable(cmdparser.lexedName())); YYABORT; }
	;

/* Expression List */
expressionlist:
	expression					{
		$$ = $1;
		if ($$ == NULL) YYABORT;
		}
	| expressionlist ',' expression			{
		$$ = Tree::joinArguments($3,$1);
		}
	| expressionlist expression			{
		Messenger::print("Error: Missing comma between items.");
		YYABORT;
		}
	;

/* ----------------------------- */
/* New Variables and Declaration */
/* ----------------------------- */

/* Conversion of allowable names to single token type */
variablename:
	VAR 						{
		tokenName = yylval.variable->name();
		Messenger::print(Messenger::Parse,"PARSER : variablename : existing var '%s'", qPrintable(tokenName));
		Messenger::print("Warning - declaration of variable '%s' in %s hides a previous declaration.", qPrintable(tokenName), qPrintable(cmdparser.sourceInfo()));
		variableName = tokenName;
/* 		$$ = &tokenName; */ // ATEN2 TODO TOCHECK
		}
	| FUNCCALL					{
		tokenName = Commands::command((Commands::Function) yylval.functionId);
		Messenger::print(Messenger::Parse,"PARSER : variablename : existing built-in function '%s'", qPrintable(tokenName));
		variableName = tokenName;
/* 		$$ = &tokenName; */ // ATEN2 TODO TOCHECK
		}
	| VARSAMESCOPE					{
		tokenName = yylval.variable->name();
		Messenger::print(Messenger::Parse,"PARSER : variablename : existing var '%s' in same scope", qPrintable(tokenName));
		Messenger::print("Error: Declaration of variable '%s' in %s conflicts with a previous declaration.", qPrintable(tokenName), qPrintable(cmdparser.sourceInfo()));
		YYABORT;
		}
	| constant					{
		tokenName = yylval.variable->name();
		Messenger::print(Messenger::Parse,"PARSER : variablename : constant '%s'", qPrintable(tokenName));
		Messenger::print("Error: Constant value found in declaration.");
		YYABORT;
		}
	| USERFUNCCALL					{
		tokenName = yylval.tree->name();
		Messenger::print(Messenger::Parse,"PARSER : variablename : existing user function '%s'", qPrintable(tokenName));
		Messenger::print("Error: Existing user-defined function '%s' in %s cannot be redeclared.", qPrintable(tokenName), qPrintable(cmdparser.sourceInfo()));
		YYABORT;
		}
	| VTYPE						{
		Messenger::print(Messenger::Parse,"PARSER : variablename : variable type-name '%s'", VTypes::dataType(yylval.vtype));
		Messenger::print("Error: Type-name used in variable declaration.");
		YYABORT;
		}
	| NEWTOKEN savetokenname			{
		Messenger::print(Messenger::Parse,"PARSER : variablename : new token, name is '%s'", qPrintable(tokenName));
		if (declaredType == VTypes::NoData) { Messenger::print("Token '%s' is undeclared.", qPrintable(tokenName)); YYABORT; }
		variableName = cmdparser.lexedName();
		}
	;

/* Variable name with assigned value */
assignedvariablename:
	variablename '=' ARRAYCONST			{
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with array assignment", qPrintable(tokenName));
		$$ = cmdparser.addVariable(declaredType, tokenName, $3, globalDeclarations);
		}
	| variablename '[' expression ']' '=' expression {
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with expr assignment", qPrintable(tokenName));
		$$ = cmdparser.addArrayVariable(declaredType, tokenName, $3, $6, globalDeclarations);
		}
	| variablename '[' expression ']' '=' ARRAYCONST {
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with array assignment", qPrintable(tokenName));
		$$ = cmdparser.addArrayVariable(declaredType, tokenName, $3, $6, globalDeclarations);
		}
	| variablename '=' expression 			{
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with expr assignment", qPrintable(variableName));
		$$ = cmdparser.addVariable(declaredType, tokenName, $3, globalDeclarations);
		}
	;

/* Variable List Item */
variablelistitem:
	variablename					{
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s'", qPrintable(tokenName));
		$$ = cmdparser.addVariable(declaredType, tokenName, NULL, globalDeclarations);
		}
	| variablename '[' expression ']' 		{
		Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s'", qPrintable(tokenName));
		$$ = cmdparser.addArrayVariable(declaredType, tokenName, $3, NULL, globalDeclarations);
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
		Messenger::print("Error: Missing comma between declarations?");
		YYABORT;
		}
	;

/* Typed Variable List Single */
typedvariablelistitem:
	VTYPE savetype variablename			{
		Messenger::print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s'", qPrintable(tokenName));
		$$ = cmdparser.addVariable(declaredType, tokenName);
		}
	| VTYPE savetype variablename '=' expression 	{
		Messenger::print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s' with expr assignment", qPrintable(tokenName));
		$$ = cmdparser.addVariable(declaredType, tokenName, $5);
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
		Messenger::print("Error: Missing comma between declarations?");
		YYABORT;
		}
	;

/* Variable Declaration Statement */
declaration:
	ATEN_GLOBAL setglobal VTYPE savetype variablelist unsetglobal	{
		Messenger::print(Messenger::Parse,"PARSER : global declaration : standard variable declaration list");
		$$ = cmdparser.addDeclarations($5);
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype variablelist			{
		Messenger::print(Messenger::Parse,"PARSER : declaration : standard variable declaration list");
		$$ = cmdparser.addDeclarations($3);
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype error				{
		Messenger::print("Illegal use of reserved word '%s'.", VTypes::dataType(declaredType));
		YYABORT;
		}
	;

/* -------------------- */
/* Function Declaration */
/* -------------------- */

/* User-Defined Function Declaration */
functiondeclaration:
	ATEN_VOID cleartype NEWTOKEN pushfunc '(' ')' block {
		Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, no arguments)");
		if (!cmdparser.addStatement($7)) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
	| ATEN_VOID cleartype NEWTOKEN pushfunc '(' typedvariablelist ')' {
		Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, arguments)");
		if (!$4->addLocalFunctionArguments($6)) YYABORT;
		} block {
		if (!cmdparser.addStatement($9)) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype NEWTOKEN pushfunc '(' ')' block {
		Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, no arguments)", VTypes::dataType($4->returnType()));
		if (!cmdparser.addStatement($7)) YYABORT;
		cmdparser.popTree();
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype NEWTOKEN pushfunc '(' typedvariablelist ')' {
		Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, arguments)", VTypes::dataType($4->returnType()));
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
		$$ = cmdparser.addFunction(Commands::Help, cmdparser.addConstant($2));
		}
	| ATEN_RETURN expression ';'			{
		$$ = cmdparser.addFunction(Commands::Return,$2);
		}
	| ATEN_RETURN ';'				{
		$$ = cmdparser.addFunction(Commands::Return);
		}
	| ATEN_CONTINUE ';'				{
		$$ = cmdparser.addFunction(Commands::Continue);
		}
	| ATEN_BREAK ';'				{
		$$ = cmdparser.addFunction(Commands::Break);
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
		$$ = cmdparser.addFunction(Commands::NoFunction);
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
		$$ = cmdparser.addFunction(Commands::If,$3,$5,$7);
		}
	| ATEN_IF '(' expression ')' blockment 			{
		$$ = cmdparser.addFunction(Commands::If,$3,$5);
		}
	| ATEN_FOR pushscope '(' assignment ';' expression ';' expression ')' blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Commands::For, $4,$6,$8,$10)); cmdparser.popScope();
		}
	| ATEN_FOR pushscope '(' declaration ';' expression ';' expression ')' blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Commands::For, $4,$6,$8,$10)); cmdparser.popScope();
		}
	| ATEN_FOR pushscope '(' variable ATEN_IN expression ')'	{
		if ($4->returnType() <= VTypes::VectorData) { Messenger::print("Error: For/In loop variable must be of pointer type."); YYABORT; }
		if ($4->returnType() != $6->returnType()) { Messenger::print("Error: For/In loop variable is not being assigned the correct type."); YYABORT; }
		} blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Commands::ForIn,$4,$6,$9));
		cmdparser.popScope();
		}
	| ATEN_FOR pushscope '(' VTYPE savetype variablename ATEN_IN expression ')' { 
		if (declaredType <= VTypes::VectorData)
		{
			Messenger::print("Error: For/In loop variable must be of pointer type.");
			YYABORT;
		}
		tempNode = cmdparser.addVariable(declaredType, tokenName);
		if (declaredType != $8->returnType())
		{
			Messenger::print("Error: For/In loop variable is not being assigned the correct type.");
			YYABORT;
		}
		} blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Commands::ForIn,tempNode,$8,$11));
		cmdparser.popScope();
		}
	| ATEN_WHILE pushscope '(' expression ')' blockment	{
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Commands::While, $4,$6));
		cmdparser.popScope();
		}
	| ATEN_DO pushscope block ATEN_WHILE '(' expression ')' ';' {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Commands::DoWhile, $3,$6));
		cmdparser.popScope();
		}
	| ATEN_SWITCH '(' expression ')' 		{
		if (($3->returnType() != VTypes::IntegerData) && ($3->returnType() != VTypes::StringData))
		{
			Messenger::print("Error: Switch value must be of integer or string type.");
			YYABORT;
		}
		} '{' caselist '}' {
		$$ = cmdparser.addFunction(Commands::Switch, $3);
		$$->addJoinedArguments($7);
		}
	;

/* Switch Statement Case/Default Label */
caselabel:
	ATEN_CASE '(' expression ')' ':'		{
		if (($3->returnType() != VTypes::IntegerData) && ($3->returnType() != VTypes::StringData))
		{
			Messenger::print("Error: Case value must be of integer or string type.");
			YYABORT;
		}
		$$ = cmdparser.addFunction(Commands::Case, $3);
		if ($$ == NULL) { Messenger::print("Error: Invalid case expression."); YYABORT; }
		}
	| ATEN_DEFAULT ':'				{
		$$ = cmdparser.addFunction(Commands::Default);
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
		if (!cmdparser.setFilterOption(tokenName, $4)) YYABORT;
		Messenger::print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'", qPrintable(tokenName));
		}
	| filteroptions ',' NEWTOKEN savetokenname '=' constant {
		if (!cmdparser.setFilterOption(tokenName, $6)) YYABORT;
		Messenger::print(Messenger::Parse,"PARSER : filteroptions : filter option '%s'", qPrintable(tokenName));
		}
	;

/* Filter Definition */
filter:
	FILTERBLOCK pushfilter '(' filteroptions ')' block 	{
		if (($6 != NULL) && (!cmdparser.addStatement($6))) YYABORT;
		cmdparser.popTree();
		Messenger::print(Messenger::Parse,"PARSER : completed filter definition");
		}
	;

/* -------------------------- */
/* Semantic Value Subroutines */
/* -------------------------- */

savetokenname:
	/* empty */					{ tokenName = cmdparser.lexedName(); }
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
	/* empty */					{ stepNameStack << cmdparser.lexedName(); }
	;

pushfunc:
	/* empty */					{
		Messenger::print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'", qPrintable(cmdparser.lexedName()));
		$$ = cmdparser.pushFunction(qPrintable(cmdparser.lexedName()), declaredType);
		/*cmdparser.pushScope();*/
		}
	;

pushfilter:
	/* empty */					{
		Messenger::print(Messenger::Parse,"PARSER : pushfilter : new filter definition");
		cmdparser.pushFilter();
		}
	;

setglobal:
	/* empty */					{ globalDeclarations = true; }
	;

unsetglobal:
	/* empty */					{ globalDeclarations = false; }
	;

%%

void yyerror(char *s)
{
}
