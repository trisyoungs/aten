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
void CommandParser_error(char const *s);

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
%name-prefix "CommandParser_"

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
%token ATEN_DO ATEN_WHILE ATEN_FOR ATEN_SWITCH ATEN_CASE ATEN_DEFAULT ATEN_IF ATEN_IIF ATEN_IN ATEN_GLOBAL ATEN_RETURN HELP ATEN_VOID ATEN_CONTINUE ATEN_BREAK ATEN_NEW
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
		if (($1 != NULL) && (!CommandParser::tree()->addStatement($1))) YYABORT;
		}
	| block						{
		if (($1 != NULL) && (!CommandParser::tree()->addStatement($1))) YYABORT;
		}
	;

/* --------- */
/* Constants */
/* --------- */

constant:
	INTCONST					{ $$ = CommandParser::tree()->addConstant($1); }
	| DOUBLECONST					{ $$ = CommandParser::tree()->addConstant($1); }
	| CHARCONST					{ $$ = CommandParser::tree()->addConstant(CommandParser::lexedName()); }
	| ELEMENTCONST					{ $$ = CommandParser::tree()->addElementConstant($1); }
	;

/* ----------------- */
/* Variables & Paths */
/* ----------------- */

/* Single Path Step */
step:
	STEPTOKEN pushstepname '[' expression ']' 	{
		if (!CommandParser::tree()->expandPath(stepNameStack.last(), $4)) YYABORT;
		stepNameStack.removeLast();
		}
	| STEPTOKEN pushstepname '(' expressionlist ')' {
		if (!CommandParser::tree()->expandPath(stepNameStack.last(), NULL, $4)) YYABORT;
		stepNameStack.removeLast();
		}
	| STEPTOKEN pushstepname '(' ')' 		{
		if (!CommandParser::tree()->expandPath(stepNameStack.last(), NULL, NULL)) YYABORT;
		stepNameStack.removeLast();
		}
	| STEPTOKEN pushstepname 			{
		if (!CommandParser::tree()->expandPath(CommandParser::lexedName())) YYABORT;
		stepNameStack.removeLast();
		}
	;

/* Multiple Step Path */
steplist:
	step 						{ }
	| steplist '.' step				{ }
	| steplist error				{ if (!CommandParser::quiet()) Messenger::print("Error formulating path."); YYABORT; }
	;

/* Pre-Existing Variable */
variable:
	VAR '[' expression ']'				{
		$$ = CommandParser::tree()->wrapVariable($1,$3);
		if ($$ == NULL) { if (!CommandParser::quiet()) Messenger::print("Error in variable expression (code 1)"); YYABORT; }
		}
	| VAR						{
		$$ = CommandParser::tree()->wrapVariable($1);
		if ($$ == NULL) { if (!CommandParser::quiet()) Messenger::print("Error in variable expression (code 2)"); YYABORT; }
		}
	| VARSAMESCOPE '[' expression ']'			{
		$$ = CommandParser::tree()->wrapVariable($1,$3);
		if ($$ == NULL) { if (!CommandParser::quiet()) Messenger::print("Error in variable expression (code 3)"); YYABORT; }
		}
	| VARSAMESCOPE					{
		$$ = CommandParser::tree()->wrapVariable($1);
		if ($$ == NULL) { if (!CommandParser::quiet()) Messenger::print("Error in variable expression (code 4)"); YYABORT; }
		}
	| variable '.' 					{
		CommandParser::tree()->createPath($1);
		} steplist {
		$$ = CommandParser::tree()->finalisePath();
		}
	| variable '('					{
		if (!CommandParser::quiet()) Messenger::print("Can't use a variable as a function. Did you mean '[' instead?"); $$ = NULL;
		}
	;

/* -------------- */
/* Function Calls */
/* -------------- */

/* Built-In Functions */
function:
	FUNCCALL '(' ')'				{
		$$ = CommandParser::tree()->addFunction( (Commands::Function) $1);
		if ($$ == NULL) YYABORT;
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse, "PARSER : function : function '%s'", Commands::command((Commands::Function) $1));
		}
	| FUNCCALL '(' expressionlist ')'		{
		$$ = CommandParser::tree()->addFunctionWithArglist( (Commands::Function) $1,$3);
		if ($$ == NULL) YYABORT;
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse, "PARSER : function : function '%s' with exprlist", Commands::command((Commands::Function) $1));
		}
	| FUNCCALL error				{
		if (!CommandParser::quiet()) Messenger::print("Error: Missing brackets after function call?");
		YYABORT;
		}
	;

/* User-Defined Functions */
userfunction:
	USERFUNCCALL '(' ')'				{
		$$ = CommandParser::tree()->addUserFunction($1);
		if ($$ == NULL) YYABORT;
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : userfunction : function '%s'", qPrintable($1->name()));
		}
	| USERFUNCCALL '(' expressionlist ')'		{
		$$ = CommandParser::tree()->addUserFunction($1,$3);
		if ($$ == NULL) YYABORT;
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : userfunction : function '%s' with expressionlist", qPrintable($1->name()));
		}
	| USERFUNCCALL error				{
		if (!CommandParser::quiet()) Messenger::print("Error: Missing brackets after function call?");
		YYABORT;
		}
	;

/* ------------ */
/* Misc Objects */
/* ------------ */

/* Array Vector Constant / Assignment Group */
ARRAYCONST:
	'{' expressionlist '}'				{
		$$ = CommandParser::tree()->addArrayConstant($2);
		if ($$ == NULL) YYABORT;
		}
	;

/* ----------- */
/* Expressions */
/* ----------- */

assignment:
	variable '=' expression				{ $$ = CommandParser::tree()->addOperator(Commands::OperatorAssignment,$1,$3); if ($$ == NULL) YYABORT; }
	| variable '=' ARRAYCONST			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorAssignment,$1,$3); if ($$ == NULL) YYABORT; }
	| variable '=' error				{ if (!CommandParser::quiet()) Messenger::print("Mangled expression used in assignment."); YYABORT; }
	;

/* Expression */
expression:
	constant					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| function					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| userfunction					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| variable PEQ expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorAssignmentPlus,$1,$3); if ($$ == NULL) YYABORT; }
	| variable MEQ expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorAssignmentSubtract,$1,$3); if ($$ == NULL) YYABORT; }
	| variable TEQ expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorAssignmentMultiply,$1,$3); if ($$ == NULL) YYABORT; }
	| variable DEQ expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorAssignmentDivide,$1,$3); if ($$ == NULL) YYABORT; }
	| '-' expression %prec UMINUS			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorNegate, $2); if ($$ == NULL) YYABORT; }
	| variable PLUSPLUS				{ $$ = CommandParser::tree()->addOperator(Commands::OperatorPostfixIncrease, $1);  if ($$ == NULL) YYABORT; }
	| variable MINUSMINUS				{ $$ = CommandParser::tree()->addOperator(Commands::OperatorPostfixDecrease, $1); if ($$ == NULL) YYABORT; }
	| PLUSPLUS variable				{ $$ = CommandParser::tree()->addOperator(Commands::OperatorPrefixIncrease, $2); if ($$ == NULL) YYABORT; }
	| MINUSMINUS variable				{ $$ = CommandParser::tree()->addOperator(Commands::OperatorPrefixDecrease, $2); if ($$ == NULL) YYABORT; }
	| variable					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| expression '+' expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorAdd, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '-' expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorSubtract, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '*' expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorMultiply, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '/' expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorDivide, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '^' expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorPower, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '%' expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorModulus, $1, $3); if ($$ == NULL) YYABORT; }
	| expression EQ expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression NEQ expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorNotEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '>' expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorGreaterThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expression GEQ expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorGreaterThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression '<' expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorLessThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expression LEQ expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorLessThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expression AND expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorAnd, $1, $3); if ($$ == NULL) YYABORT; }
	| expression OR expression			{ $$ = CommandParser::tree()->addOperator(Commands::OperatorOr, $1, $3); if ($$ == NULL) YYABORT; }
	| '(' expression ')'				{ $$ = $2; if ($$ == NULL) YYABORT; }
	| '!' expression				{ $$ = CommandParser::tree()->addOperator(Commands::OperatorNot, $2); if ($$ == NULL) YYABORT; }
	| expression '?' expression ':' expression	{ $$ = CommandParser::tree()->addOperator(Commands::OperatorInlineIf, $1, $3, $5); if ($$ == NULL) YYABORT; }
	| ATEN_NEW VTYPE				{ $$ = CommandParser::tree()->addNew(yylval.vtype); if ($$ == NULL) YYABORT; }
	| NEWTOKEN					{ if (!CommandParser::quiet()) Messenger::print("Error: '%s' has not been declared as a function or a variable.", qPrintable(CommandParser::lexedName())); YYABORT; }
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
		if (!CommandParser::quiet()) Messenger::print("Error: Missing comma between items.");
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
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : variablename : existing var '%s'", qPrintable(tokenName));
		if (!CommandParser::quiet()) Messenger::print("Warning - declaration of variable '%s' in %s hides a previous declaration.", qPrintable(tokenName), qPrintable(CommandParser::sourceInfo()));
		variableName = tokenName;
/* 		$$ = &tokenName; */ // ATEN2 TODO TOCHECK
		}
	| FUNCCALL					{
		tokenName = Commands::command((Commands::Function) yylval.functionId);
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : variablename : existing built-in function '%s'", qPrintable(tokenName));
		variableName = tokenName;
/* 		$$ = &tokenName; */ // ATEN2 TODO TOCHECK
		}
	| VARSAMESCOPE					{
		tokenName = yylval.variable->name();
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : variablename : existing var '%s' in same scope", qPrintable(tokenName));
		if (!CommandParser::quiet()) Messenger::print("Error: Declaration of variable '%s' in %s conflicts with a previous declaration.", qPrintable(tokenName), qPrintable(CommandParser::sourceInfo()));
		YYABORT;
		}
	| constant					{
		tokenName = yylval.variable->name();
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : variablename : constant '%s'", qPrintable(tokenName));
		if (!CommandParser::quiet()) Messenger::print("Error: Constant value found in declaration.");
		YYABORT;
		}
	| USERFUNCCALL					{
		tokenName = yylval.tree->name();
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : variablename : existing user function '%s'", qPrintable(tokenName));
		if (!CommandParser::quiet()) Messenger::print("Error: Existing user-defined function '%s' in %s cannot be redeclared.", qPrintable(tokenName), qPrintable(CommandParser::sourceInfo()));
		YYABORT;
		}
	| VTYPE						{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : variablename : variable type-name '%s'", VTypes::dataType(yylval.vtype));
		if (!CommandParser::quiet()) Messenger::print("Error: Type-name used in variable declaration.");
		YYABORT;
		}
	| NEWTOKEN savetokenname			{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : variablename : new token, name is '%s'", qPrintable(tokenName));
		if (declaredType == VTypes::NoData) { if (!CommandParser::quiet()) Messenger::print("Token '%s' is undeclared.", qPrintable(tokenName)); YYABORT; }
		variableName = CommandParser::lexedName();
		}
	;

/* Variable name with assigned value */
assignedvariablename:
	variablename '=' ARRAYCONST			{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with array assignment", qPrintable(tokenName));
		$$ = CommandParser::tree()->addVariable(declaredType, tokenName, $3, globalDeclarations);
		}
	| variablename '[' expression ']' '=' expression {
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with expr assignment", qPrintable(tokenName));
		$$ = CommandParser::tree()->addArrayVariable(declaredType, tokenName, $3, $6, globalDeclarations);
		}
	| variablename '[' expression ']' '=' ARRAYCONST {
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s' with array assignment", qPrintable(tokenName));
		$$ = CommandParser::tree()->addArrayVariable(declaredType, tokenName, $3, $6, globalDeclarations);
		}
	| variablename '=' expression 			{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s' with expr assignment", qPrintable(variableName));
		$$ = CommandParser::tree()->addVariable(declaredType, tokenName, $3, globalDeclarations);
		}
	;

/* Variable List Item */
variablelistitem:
	variablename					{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : var '%s'", qPrintable(tokenName));
		$$ = CommandParser::tree()->addVariable(declaredType, tokenName, NULL, globalDeclarations);
		}
	| variablename '[' expression ']' 		{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : assignedvariablename : array var '%s'", qPrintable(tokenName));
		$$ = CommandParser::tree()->addArrayVariable(declaredType, tokenName, $3, NULL, globalDeclarations);
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
		if (!CommandParser::quiet()) Messenger::print("Error: Missing comma between declarations?");
		YYABORT;
		}
	;

/* Typed Variable List Single */
typedvariablelistitem:
	VTYPE savetype variablename			{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s'", qPrintable(tokenName));
		$$ = CommandParser::tree()->addVariable(declaredType, tokenName);
		}
	| VTYPE savetype variablename '=' expression 	{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : typedvariablelistitem : var '%s' with expr assignment", qPrintable(tokenName));
		$$ = CommandParser::tree()->addVariable(declaredType, tokenName, $5);
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
		if (!CommandParser::quiet()) Messenger::print("Error: Missing comma between declarations?");
		YYABORT;
		}
	;

/* Variable Declaration Statement */
declaration:
	ATEN_GLOBAL setglobal VTYPE savetype variablelist unsetglobal	{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : global declaration : standard variable declaration list");
		$$ = CommandParser::tree()->addDeclarations($5);
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype variablelist			{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : declaration : standard variable declaration list");
		$$ = CommandParser::tree()->addDeclarations($3);
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype error				{
		if (!CommandParser::quiet()) Messenger::print("Illegal use of reserved word '%s'.", VTypes::dataType(declaredType));
		YYABORT;
		}
	;

/* -------------------- */
/* Function Declaration */
/* -------------------- */

/* User-Defined Function Declaration */
functiondeclaration:
	ATEN_VOID cleartype NEWTOKEN pushfunc '(' ')' block {
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, no arguments)");
		if (!CommandParser::tree()->addStatement($7)) YYABORT;
		CommandParser::popTree();
		declaredType = VTypes::NoData;
		}
	| ATEN_VOID cleartype NEWTOKEN pushfunc '(' typedvariablelist ')' {
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined subroutine (VOID return value, arguments)");
		if (!$4->addLocalFunctionArguments($6)) YYABORT;
		} block {
		if (!CommandParser::tree()->addStatement($9)) YYABORT;
		CommandParser::popTree();
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype NEWTOKEN pushfunc '(' ')' block {
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, no arguments)", VTypes::dataType($4->returnType()));
		if (!CommandParser::tree()->addStatement($7)) YYABORT;
		CommandParser::popTree();
		declaredType = VTypes::NoData;
		}
	| VTYPE savetype NEWTOKEN pushfunc '(' typedvariablelist ')' {
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : functiondeclaration : user-defined function (%s return value, arguments)", VTypes::dataType($4->returnType()));
		if (!$4->addLocalFunctionArguments($6)) YYABORT;
		} block {
		if (!CommandParser::tree()->addStatement($9)) YYABORT;
		CommandParser::popTree();
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
	| HELP FUNCCALL					{
		$$ = CommandParser::tree()->addFunction(Commands::Help, CommandParser::tree()->addConstant($2));
		}
	| ATEN_RETURN expression ';'			{
		$$ = CommandParser::tree()->addFunction(Commands::Return,$2);
		}
	| ATEN_RETURN ';'				{
		$$ = CommandParser::tree()->addFunction(Commands::Return);
		}
	| ATEN_CONTINUE ';'				{
		$$ = CommandParser::tree()->addFunction(Commands::Continue);
		}
	| ATEN_BREAK ';'				{
		$$ = CommandParser::tree()->addFunction(Commands::Break);
		}
	;

/* Statement List */
statementlist:
	statement					{
		$$ = $1;
		}
	| statementlist statement			{
		if ($2 == NULL) $$ = $1;
		else $$ = CommandParser::tree()->joinCommands($1, $2);
		}
	;

/* Block Statement */
block:
	'{' pushscope statementlist '}' popscope	{
		$$ = $3;
		}
	| '{' '}'					{
		$$ = CommandParser::tree()->addFunction(Commands::NoFunction);
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
		$$ = CommandParser::tree()->addFunction(Commands::If,$3,$5,$7);
		}
	| ATEN_IF '(' expression ')' blockment 			{
		$$ = CommandParser::tree()->addFunction(Commands::If,$3,$5);
		}
	| ATEN_FOR pushscope '(' assignment ';' expression ';' expression ')' blockment {
		$$ = CommandParser::tree()->joinCommands($2, CommandParser::tree()->addFunction(Commands::For, $4,$6,$8,$10)); CommandParser::tree()->popScope();
		}
	| ATEN_FOR pushscope '(' declaration ';' expression ';' expression ')' blockment {
		$$ = CommandParser::tree()->joinCommands($2, CommandParser::tree()->addFunction(Commands::For, $4,$6,$8,$10)); CommandParser::tree()->popScope();
		}
	| ATEN_FOR pushscope '(' variable ATEN_IN expression ')'	{
		if ($4->returnType() <= VTypes::VectorData) { if (!CommandParser::quiet()) Messenger::print("Error: For/In loop variable must be of pointer type."); YYABORT; }
		if ($4->returnType() != $6->returnType()) { if (!CommandParser::quiet()) Messenger::print("Error: For/In loop variable is not being assigned the correct type."); YYABORT; }
		} blockment {
		$$ = CommandParser::tree()->joinCommands($2, CommandParser::tree()->addFunction(Commands::ForIn,$4,$6,$9));
		CommandParser::tree()->popScope();
		}
	| ATEN_FOR pushscope '(' VTYPE savetype variablename ATEN_IN expression ')' { 
		if (declaredType <= VTypes::VectorData)
		{
			if (!CommandParser::quiet()) Messenger::print("Error: For/In loop variable must be of pointer type.");
			YYABORT;
		}
		tempNode = CommandParser::tree()->addVariable(declaredType, tokenName);
		if (declaredType != $8->returnType())
		{
			if (!CommandParser::quiet()) Messenger::print("Error: For/In loop variable is not being assigned the correct type.");
			YYABORT;
		}
		} blockment {
		$$ = CommandParser::tree()->joinCommands($2, CommandParser::tree()->addFunction(Commands::ForIn,tempNode,$8,$11));
		CommandParser::tree()->popScope();
		}
	| ATEN_WHILE pushscope '(' expression ')' blockment	{
		$$ = CommandParser::tree()->joinCommands($2, CommandParser::tree()->addFunction(Commands::While, $4,$6));
		CommandParser::tree()->popScope();
		}
	| ATEN_DO pushscope block ATEN_WHILE '(' expression ')' ';' {
		$$ = CommandParser::tree()->joinCommands($2, CommandParser::tree()->addFunction(Commands::DoWhile, $3,$6));
		CommandParser::tree()->popScope();
		}
	| ATEN_SWITCH '(' expression ')' 		{
		if (($3->returnType() != VTypes::IntegerData) && ($3->returnType() != VTypes::StringData))
		{
			if (!CommandParser::quiet()) Messenger::print("Error: Switch value must be of integer or string type.");
			YYABORT;
		}
		} '{' caselist '}' {
		$$ = CommandParser::tree()->addFunction(Commands::Switch, $3);
		$$->addJoinedArguments($7);
		}
	;

/* Switch Statement Case/Default Label */
caselabel:
	ATEN_CASE '(' expression ')' ':'		{
		if (($3->returnType() != VTypes::IntegerData) && ($3->returnType() != VTypes::StringData))
		{
			if (!CommandParser::quiet()) Messenger::print("Error: Case value must be of integer or string type.");
			YYABORT;
		}
		$$ = CommandParser::tree()->addFunction(Commands::Case, $3);
		if ($$ == NULL) { if (!CommandParser::quiet()) Messenger::print("Error: Invalid case expression."); YYABORT; }
		}
	| ATEN_DEFAULT ':'				{
		$$ = CommandParser::tree()->addFunction(Commands::Default);
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

/* -------------------------- */
/* Semantic Value Subroutines */
/* -------------------------- */

savetokenname:
	/* empty */					{ tokenName = CommandParser::lexedName(); }
	;

savetype:
	/* empty */					{ declaredType = yylval.vtype; }
	;

cleartype:
	/* empty */					{ declaredType = VTypes::NoData; }
	;

pushscope:
	/* empty */					{ $$ = CommandParser::tree()->pushScope(); if ($$ == NULL) YYABORT; }
	;

popscope:
	/* empty */					{ if (!CommandParser::tree()->popScope()) YYABORT; }
	;

pushstepname:
	/* empty */					{ stepNameStack << CommandParser::lexedName(); }
	;

pushfunc:
	/* empty */					{
		if (!CommandParser::quiet()) Messenger::print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'", qPrintable(CommandParser::lexedName()));
		$$ = CommandParser::pushFunction(qPrintable(CommandParser::lexedName()), declaredType);
		/*CommandParser::pushScope();*/
		}
	;

setglobal:
	/* empty */					{ globalDeclarations = true; }
	;

unsetglobal:
	/* empty */					{ globalDeclarations = false; }
	;

%%

void yyerror(char const *s)
{
}
