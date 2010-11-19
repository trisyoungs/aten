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

%}

// Redeclare function names
%name-prefix="CommandParser_"

/* Type Definition */
%union {
	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *functree;			/* user-defined function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */
};

%token <intconst> INTCONST ELEMENTCONST
%token <doubleconst> DOUBLECONST
%token <name> NEWTOKEN NEWFUNCTOKEN CHARCONST STEPTOKEN
%token <variable> VAR LOCALVAR
%token <functionId> FUNCCALL
%token <functree> USERFUNCCALL
%token <vtype> VTYPE
%token ATEN_DO WHILE FOR ATEN_IF ATEN_IN ATEN_RETURN FILTERBLOCK HELP ATEN_VOID DUMMY OPTION
%nonassoc ELSE

%left AND OR
%left '=' PEQ MEQ TEQ DEQ 
%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/' '%'
%right UMINUS
%left PLUSPLUS MINUSMINUS
%right '!'
%right '^'

%type <node> constant expr rawexpr func var rawvar vardec
%type <node> flowstatement decexpr statement block blockment statementlist exprlist ARRAYCONST
%type <node> declist arglist args
%type <name> newname
%type <node> filter pushscope argdeclaration vardeclaration funcdeclaration userfunc widget

%%

programlist:
	/* empty */					{ }
	| program					{ }
	| programlist program				{ }
	;

program:
	statementlist					{ if (($1 != NULL) && (!cmdparser.addStatement($1))) YYABORT; }
	| filter					{ }
	;

/* Compound Statement */

block:
	'{' pushscope statementlist '}' popscope	{ $$ = cmdparser.joinCommands($2,$3); }
	| '{' '}'					{ $$ = cmdparser.addFunction(Command::NoFunction); }
        ;

pushscope:
	/* empty */					{ $$ = cmdparser.pushScope(); if ($$ == NULL) YYABORT; }
	;

popscope:
	/* empty */					{ if (!cmdparser.popScope()) YYABORT; }
	;

statementlist:
	blockment					{ $$ = $1; }
	| funcdeclaration				{ $$ = NULL; }
	| statementlist blockment 			{ $$ = cmdparser.joinCommands($1, $2); }
	| statementlist funcdeclaration			{ $$ = $1; }
	;

blockment:
	statement ';'					{ $$ = $1; }
	| block						{ $$ = $1; }
	| flowstatement					{ $$ = $1; if ($$ == NULL) YYABORT; }
	;

/* Filter Definitions */

optlist:
	NEWTOKEN savetokenname '=' constant		{
		if (!cmdparser.setFilterOption(&tokenName, $4)) YYABORT;
		msg.print(Messenger::Parse,"PARSER : optlist : filter option '%s'\n", tokenName.get());
		}
	| optlist ',' NEWTOKEN savetokenname '=' constant {
		if (!cmdparser.setFilterOption(&tokenName, $6)) YYABORT;
		msg.print(Messenger::Parse,"PARSER : optlist : filter option '%s'\n", tokenName.get());
		}
	;

filter:
	FILTERBLOCK pushfilter '(' optlist ')' block 	{ if (!cmdparser.addStatement($6)) YYABORT; cmdparser.popTree(); }
	;

pushfilter:
	/* empty */					{ cmdparser.pushTree(TRUE); }
	;

/* Single Statement / Flow Control */

statement:
	decexpr						{ $$ = $1; }
	| widget					{ $$ = $1; }
	| HELP FUNCCALL					{ $$ = cmdparser.addFunction(Command::Help, cmdparser.addConstant($2)); }
	| ATEN_RETURN expr					{ $$ = cmdparser.addFunction(Command::Return,$2); }
	| ATEN_RETURN 					{ $$ = cmdparser.addFunction(Command::Return); }
	| statement decexpr				{ msg.print("Error: Expected ';' before current expression.\n"); YYABORT; }
	;

/* Variable declaration (possibly with assignment) or normal expression, neither terminated with a ';' */
decexpr:
	vardeclaration					{ $$ = $1; }
	| expr						{ $$ = $1; }
	;

flowstatement:
	ATEN_IF '(' expr ')' blockment ELSE blockment {
		$$ = cmdparser.addFunction(Command::If,$3,$5,$7); }
	| ATEN_IF '(' expr ')' blockment {
		$$ = cmdparser.addFunction(Command::If,$3,$5); }
	| FOR pushscope '(' decexpr ';' expr ';' expr ')' blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::For, $4,$6,$8,$10)); cmdparser.popScope(); }
	| FOR pushscope '(' var ATEN_IN expr ')' {
		if ($4->returnType() <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		if ($4->returnType() != $6->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
		blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::ForIn,$4,$6,$9)); cmdparser.popScope(); }
	| FOR pushscope '(' VTYPE savetype newname ATEN_IN expr ')' { 
		if (declaredType <= VTypes::VectorData) { msg.print("Error: For/In loop variable must be of pointer type.\n"); YYABORT; }
		tempNode = cmdparser.addVariable(declaredType, &tokenName);
		if (declaredType != $8->returnType()) { msg.print("Error: For/In loop variable is not being assigned the correct type.\n"); YYABORT; }
		}
		blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::ForIn,tempNode,$8,$11)); cmdparser.popScope(); }
	| WHILE pushscope '(' expr ')' blockment {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::While, $4,$6)); cmdparser.popScope(); }
	| ATEN_DO pushscope blockment WHILE '(' expr ')' ';' {
		$$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::DoWhile, $3,$6)); cmdparser.popScope(); }
	;

/* Constants */

constant:
	INTCONST					{ $$ = cmdparser.addConstant($1); }
	| DOUBLECONST					{ $$ = cmdparser.addConstant($1); }
	| CHARCONST					{ $$ = cmdparser.addConstant($1->get()); }
	| ELEMENTCONST					{ $$ = cmdparser.addElementConstant($1); }
	;

/* Variable and user-defined function declaration and name / assignment lists */

funcdeclaration:
	ATEN_VOID cleartype NEWFUNCTOKEN pushfunc arglist block	{
		msg.print(Messenger::Parse,"PARSER : funcdeclaration : user-defined statement\n");
		if (!cmdparser.addStatement($6)) YYABORT; cmdparser.popTree(); declaredType = VTypes::NoData; }
	| VTYPE savetype NEWFUNCTOKEN pushfunc arglist block	{
		msg.print(Messenger::Parse,"PARSER : funcdeclaration : user-defined function\n");
		if (!cmdparser.addStatement($6)) YYABORT; cmdparser.popTree(); declaredType = VTypes::NoData; }
	| NEWFUNCTOKEN						{
		msg.print("Error: '%s' is not a recognised built-in or user-defined function.\n", $1->get()); YYABORT; }
	;

vardeclaration:
	VTYPE savetype declist			{
		msg.print(Messenger::Parse,"PARSER : vardeclaration : standard variable declaration list\n");
		$$ = cmdparser.addDeclarations($3); declaredType = VTypes::NoData; }
	| VTYPE savetype error				{
		msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType)); YYABORT; }
	;

arglist:
	'(' args ')'					{
		msg.print(Messenger::Parse,"PARSER : arglist : bracketed argument list\n");
		$$ = $2; }
	;

args:
	/* empty */					{
		msg.print(Messenger::Parse,"PARSER : args : empty\n");
		$$ = NULL; }
	| argdeclaration				{
		msg.print(Messenger::Parse,"PARSER : args : adding item\n");
		$$ = $1; }
	| args ',' argdeclaration			{
		msg.print(Messenger::Parse,"PARSER : args : joining items\n");
		if ($3 == NULL) YYABORT; $$ = Tree::joinArguments($3,$1); }
	;

argdeclaration:
	VTYPE savetype newname '=' expr 		{
		msg.print(Messenger::Parse,"PARSER : argdeclaration : function argument '%s' with expr assignment\n", tokenName.get());
		$$ = cmdparser.addVariableAsArgument(declaredType, &tokenName, $5); }
	| VTYPE savetype newname			{
		msg.print(Messenger::Parse,"PARSER : argdeclaration : function argument '%s'\n", tokenName.get());
		$$ = cmdparser.addVariableAsArgument(declaredType, &tokenName); }
	;

declist:
	vardec						{ $$ = $1; if ($1 == NULL) YYABORT; }
	| declist ',' vardec				{ if ($3 == NULL) YYABORT; $$ = Tree::joinArguments($3,$1); }
	| declist vardec					{ msg.print("Error: Missing comma between declarations?\n"); YYABORT; }
	;

vardec:
	newname '[' expr ']' 				{
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s'\n", tokenName.get());
		$$ = cmdparser.addArrayVariable(declaredType, &tokenName, $3); }
	| newname '=' ARRAYCONST			{
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with array assignment\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName, $3); }
	| newname '=' widget	 			{
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with widget assignment\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName, $3); }
	| newname '[' expr ']' '=' expr			{
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s' with expr assignment\n", tokenName.get());
		$$ = cmdparser.addArrayVariable(declaredType, &tokenName,$3,$6); }
	| newname '[' expr ']' '=' ARRAYCONST		{
		msg.print(Messenger::Parse,"PARSER : vardec : array var '%s' with array assignment\n", tokenName.get());
		$$ = cmdparser.addArrayVariable(declaredType, &tokenName,$3,$6); }
	| newname '=' expr 				{
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s' with expr assignment\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName, $3); }
	| newname					{
		msg.print(Messenger::Parse,"PARSER : vardec : var '%s'\n", tokenName.get());
		$$ = cmdparser.addVariable(declaredType, &tokenName); }
	;

newname:
	VAR 						{
		msg.print(Messenger::Parse,"PARSER : newname : existing var '%s'\n", tokenName.get());
		tokenName = yylval.variable->name(); $$ = &tokenName; }
	| FUNCCALL					{
		msg.print(Messenger::Parse,"PARSER : newname : existing built-in function '%s'\n", tokenName.get());
		tokenName = Command::data[yylval.functionId].keyword; $$ = &tokenName; }
	| LOCALVAR					{
		msg.print(Messenger::Parse,"PARSER : newname : existing local var '%s'\n", tokenName.get());
		msg.print("Error: Existing variable in local scope cannot be redeclared.\n"); YYABORT; }
	| constant					{
		msg.print(Messenger::Parse,"PARSER : newname : constant '%s'\n", tokenName.get());
		msg.print("Error: Constant value found in declaration.\n"); YYABORT; }
	| USERFUNCCALL					{
		msg.print(Messenger::Parse,"PARSER : newname : existing user function '%s'\n", tokenName.get());
		msg.print("Error: Existing user-defined function name cannot be redeclared.\n"); YYABORT; }
	| VTYPE						{
		msg.print(Messenger::Parse,"PARSER : newname : variable type-name '%s'\n", VTypes::dataType( yylval.vtype));
		msg.print("Error: Type-name used in variable declaration.\n"); YYABORT; }
	| NEWTOKEN savetokenname			{
		msg.print(Messenger::Parse,"PARSER : newname : new token '%s'\n", tokenName.get());
		if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; } $$ = $1; }
	;

pushfunc:
	/* empty */					{
		msg.print(Messenger::Parse,"PARSER : pushfunc : function/statement '%s'\n", yylval.name->get());
		cmdparser.pushFunction(yylval.name->get(), declaredType); }
	;


/* Filter Option Definition */

widget:
	OPTION '(' exprlist ')'				{ $$ = cmdparser.addWidget($3); }
	;

/* Variables / Paths */

step:
	STEPTOKEN pushstepname '[' expr ']' 		{ if (!cmdparser.expandPath(stepNameStack.last(), $4)) YYABORT; stepNameStack.removeLast(); }
	| STEPTOKEN pushstepname '(' exprlist ')' 	{ if (!cmdparser.expandPath(stepNameStack.last(), NULL, $4)) YYABORT; stepNameStack.removeLast(); }
	| STEPTOKEN pushstepname '(' ')' 		{ if (!cmdparser.expandPath(stepNameStack.last(), NULL, NULL)) YYABORT; stepNameStack.removeLast(); }
	| STEPTOKEN pushstepname 			{ if (!cmdparser.expandPath($1)) YYABORT; stepNameStack.removeLast(); }
	;

steplist:
	step 						{ }
	| steplist '.' step				{ }
	| steplist error				{ msg.print("Error formulating path.\n"); YYABORT; }
	;

var:
	rawvar						{ $$ = $1; if ($$ == NULL) YYABORT; }
	;

rawvar:
	VAR '[' expr ']'				{ $$ = cmdparser.wrapVariable($1,$3); }
	| VAR						{ $$ = cmdparser.wrapVariable($1); }
	| LOCALVAR '[' expr ']'				{ $$ = cmdparser.wrapVariable($1,$3); }
	| LOCALVAR					{ $$ = cmdparser.wrapVariable($1); }
	| rawvar '.' 					{ cmdparser.createPath($1); }
		steplist				{ $$ = cmdparser.finalisePath(); }
	| rawvar '('					{ msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); $$ = NULL; }
	;

/* Expressions */

exprlist:
	expr						{ $$ = $1; if ($$ == NULL) YYABORT; }
	| exprlist ',' expr				{ $$ = Tree::joinArguments($3,$1); }
	| exprlist expr					{ msg.print("Error: Missing comma between items.\n"); YYABORT; }
	;

expr:
	rawexpr						{ $$ = $1; if ($$ == NULL) YYABORT; }
	;

rawexpr:
	constant					{ $$ = $1; }
	| func						{ $$ = $1; }
	| userfunc					{ $$ = $1; }
	| var '=' expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignment,$1,$3); }
	| var '=' ARRAYCONST				{ $$ = cmdparser.addOperator(Command::OperatorAssignment,$1,$3); }
	| var '=' error					{ msg.print("Mangled expression used in assignment.\n"); YYABORT; }
	| var PEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentPlus,$1,$3); }
	| var MEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentSubtract,$1,$3); }
	| var TEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentMultiply,$1,$3); }
	| var DEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentDivide,$1,$3); }
	| '-' expr %prec UMINUS				{ $$ = cmdparser.addOperator(Command::OperatorNegate, $2); }
	| var PLUSPLUS					{ $$ = cmdparser.addOperator(Command::OperatorPostfixIncrease, $1);  }
	| var MINUSMINUS				{ $$ = cmdparser.addOperator(Command::OperatorPostfixDecrease, $1); }
	| PLUSPLUS var					{ $$ = cmdparser.addOperator(Command::OperatorPrefixIncrease, $2); }
	| MINUSMINUS var				{ $$ = cmdparser.addOperator(Command::OperatorPrefixDecrease, $2); }
	| var						{ $$ = $1; }
	| expr '+' expr					{ $$ = cmdparser.addOperator(Command::OperatorAdd, $1, $3); }
	| expr '-' expr					{ $$ = cmdparser.addOperator(Command::OperatorSubtract, $1, $3); }
	| expr '*' expr					{ $$ = cmdparser.addOperator(Command::OperatorMultiply, $1, $3); }
	| expr '/' expr					{ $$ = cmdparser.addOperator(Command::OperatorDivide, $1, $3); }
	| expr '^' expr					{ $$ = cmdparser.addOperator(Command::OperatorPower, $1, $3); }
	| expr '%' expr					{ $$ = cmdparser.addOperator(Command::OperatorModulus, $1, $3); }
	| expr EQ expr					{ $$ = cmdparser.addOperator(Command::OperatorEqualTo, $1, $3); }
	| expr NEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorNotEqualTo, $1, $3); }
	| expr '>' expr					{ $$ = cmdparser.addOperator(Command::OperatorGreaterThan, $1, $3); }
	| expr GEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, $1, $3); }
	| expr '<' expr					{ $$ = cmdparser.addOperator(Command::OperatorLessThan, $1, $3); }
	| expr LEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorLessThanEqualTo, $1, $3); }
	| expr AND expr					{ $$ = cmdparser.addOperator(Command::OperatorAnd, $1, $3); }
	| expr OR expr					{ $$ = cmdparser.addOperator(Command::OperatorOr, $1, $3); }
	| '(' expr ')'					{ $$ = $2; }
	| '!' expr					{ $$ = cmdparser.addOperator(Command::OperatorNot, $2); }
	| NEWTOKEN					{ msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
	;


/* Array Vector Constant / Assignment Group */
ARRAYCONST:
	'{' exprlist '}'				{ $$ = cmdparser.addArrayConstant($2); }
	;

/* Functions */

func:
	FUNCCALL '(' ')'				{ $$ = cmdparser.addFunction( (Command::Function) $1); }
	| FUNCCALL '(' exprlist ')' 			{ $$ = cmdparser.addFunctionWithArglist( (Command::Function) $1,$3); }
	| FUNCCALL					{ $$ = cmdparser.addFunction( (Command::Function) $1); }
	;

userfunc:
	USERFUNCCALL '(' exprlist ')' 			{ $$ = cmdparser.addUserFunction($1,$3); }
	| USERFUNCCALL '(' ')'				{ $$ = cmdparser.addUserFunction($1); }
	| USERFUNCCALL					{ $$ = cmdparser.addUserFunction($1); }
	;

/* Semantic Value Subroutines */

savetokenname:
	/* empty */					{ tokenName = *yylval.name; }
	;

savetype:
	/* empty */					{ declaredType = yylval.vtype; }
	;

cleartype:
	/* empty */					{ declaredType = VTypes::NoData; }
	;

pushstepname:
	/* empty */					{ stepNameStack.add()->set(yylval.name->get()); }
	;

%%

void yyerror(char *s)
{
}
