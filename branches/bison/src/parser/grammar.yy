/* -------- */
/* Prologue */
/* -------- */
%{

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "nucommand/commands.h"
#include "parser/parser.h"
#include "parser/tree.h"

/* Prototypes */
int yylex(void);
void yyerror(char *s);

/* Local Variables */
Dnchar newVarName;
Dnchar newStepName;

%}

/* Type Definition */
%union {
	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
};

%token <name> NEWTOKEN INTCONST REALCONST CHARCONST STEPTOKEN
%token <variable> VARNAME
%token <functionId> FUNCCALL
%token DECLARATION DO WHILE FOR IF FILTERBLOCK
%nonassoc ELSE

%nonassoc AND OR
%left '=' PEQ MEQ TEQ DEQ 
%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/'
%right UMINUS
%left PP MM
%right '!'
%right '^'

%type <node> constant expr func var
%type <node> flowstatement stexpr statement block blockment statementlist exprlist VECCONST
%type <node> namelist newname
%type <node> filter scope

%%

programlist:
	program					{ }
	| programlist program			{ }
	;

program:
	statementlist				{ if (!cmdparser.addStatement($1)) YYABORT; }
	| filter				{ if (!cmdparser.addStatement($1)) YYABORT; cmdparser.finishTree(); }
	;

/* Compound Statement */

block:
	'{' scope statementlist '}' descope	{ $$ = cmdparser.joinCommands($2,$3); }
        ;

scope:
	/* empty */				{ $$ = cmdparser.pushScope(); if ($$ == NULL) YYABORT; }
	;

descope:
	/* empty */				{ if (!cmdparser.popScope()) YYABORT; }
	;

statementlist:
	statement				{ $$ = $1; }
        | statementlist statement 		{ $$ = cmdparser.joinCommands($1, $2); }
        | statementlist block	 		{ $$ = cmdparser.joinCommands($1, $2); }
        ;

blockment:
	statement				{ $$ = $1; }
	| block					{ $$ = $1; }
	;

/* Filter Definitions */

optlist:
	NEWTOKEN assign '=' constant noassign	{ if (!cmdparser.setFilterOption(&newVarName, $4)) YYABORT; }
	| optlist ',' NEWTOKEN assign '=' constant noassign	{ if (!cmdparser.setFilterOption(&newVarName, $6)) YYABORT; }
	;

filter:
	FILTERBLOCK '(' optlist ')' block	{ $$ = $5; }
	| FILTERBLOCK error			{ msg.print("Error reading filter block definition.\n"); YYABORT; }
	;

/* Single Statement / Flow Control */

statement:
	';'					{ $$ = cmdparser.addFunction(Command::NoFunction); }
	| stexpr ';'				{ $$ = $1; }
	| flowstatement				{ $$ = $1; }
	;

stexpr:
	DECLARATION namelist 			{ $$ = cmdparser.addFunction(Command::NoFunction); cmdparser.setDeclarationType(VTypes::NoData); }
	| expr					{ $$ = $1; }
	| DECLARATION '.' steplist		{ msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(cmdparser.declarationType())); YYABORT; }
	;

flowstatement:
	IF '(' expr ')' blockment ELSE blockment
		{ $$ = cmdparser.addFunction(Command::If,$3,$5,$7); }
	| IF '(' expr ')' blockment
		{ $$ = cmdparser.addFunction(Command::If,$3,$5); }
	| FOR scope '(' stexpr ';' stexpr ';' stexpr ')' blockment
		{ $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::For, $4,$6,$8,$10)); cmdparser.popScope(); }
	| WHILE scope '(' expr ')' blockment
		{ $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::While, $4,$6)); cmdparser.popScope(); }
	| DO scope blockment WHILE '(' expr ')'
		{ $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::DoWhile, $3,$6)); cmdparser.popScope(); }
	;

/* Range (X~Y) */
range:
	expr '~' expr				{ printf("GENERATE RANGE. TGAY\n"); }
	;

/* Constants */

constant:
	INTCONST				{ $$ = cmdparser.addConstant(VTypes::IntegerData, $1); }
	| REALCONST				{ $$ = cmdparser.addConstant(VTypes::DoubleData, $1); }
	| CHARCONST				{ $$ = cmdparser.addConstant(VTypes::StringData, $1); }
	;

/* Variable declaration  name / assignment list */

assign:
	/* empty */				{ newVarName = *yylval.name; cmdparser.setDeclarationAssignment(TRUE); }
	;

noassign:
	/* empty */				{ cmdparser.setDeclarationAssignment(FALSE); }
	;

newname:
	NEWTOKEN assign '[' expr ']' noassign	{ $$ = cmdparser.addArrayVariable(&newVarName,$4); }
	| NEWTOKEN assign '=' expr noassign	{ $$ = cmdparser.addVariable(&newVarName,$4); }
	| NEWTOKEN assign '[' expr ']' '=' expr noassign	{ $$ = cmdparser.addArrayVariable(&newVarName,$4,$7); }
	| NEWTOKEN				{ $$ = cmdparser.addVariable($1); }
	;

namelist:
	newname					{ $$ = $1; }
	| namelist ',' newname			{ $$ = Tree::joinArguments($3,$1); }
	| namelist ',' constant			{ msg.print("Error: Constant value found in declaration.\n"); YYABORT; }
	| namelist newname			{ msg.print("Error: Missing comma between declarations?\n"); YYABORT; }
	| namelist error			{ YYABORT; }
	;

/* Variables / Paths */

step:
	STEPTOKEN savename '[' expr ']'		{ if (!cmdparser.expandPath(&newStepName, $4)) YYABORT; }
	| STEPTOKEN savename '[' constant ']'	{ if (!cmdparser.expandPath(&newStepName, $4)) YYABORT; }
	| STEPTOKEN				{ if (!cmdparser.expandPath($1)) YYABORT; }
	;

savename:
	/* empty */				{ newStepName = *yylval.name; }
	;

steplist:
	step 					{  }
	| steplist '.' step			{  }
	| error	'.'				{ printf("Arse..\n"); }
	;

var:
	VARNAME '[' expr ']'			{ $$ = cmdparser.wrapVariable($1,$3); if ($$ == NULL) YYABORT; }
	| VARNAME				{ $$ = cmdparser.wrapVariable($1); if ($$ == NULL) YYABORT; }
	| var '.' 				{ $$ = cmdparser.createPath($1); }
		steplist			{ $$ = cmdparser.finalisePath(); }
	;

/* Expressions */

exprlist:
	expr					{ $$ = $1; }
	| exprlist ',' expr			{ $$ = Tree::joinArguments($3,$1); }
	| exprlist expr				{ msg.print("Error: Missing comma between items?\n"); YYABORT; }
	;

expr:
	constant				{ $$ = $1; }
	| func					{ $$ = $1; }
	| var '=' expr				{ $$ = cmdparser.addOperator(Command::OperatorAssignment,$1,$3); if ($$ == NULL) YYABORT; }
	| var '=' error				{ msg.print("Mangled expression used in assignment.\n"); YYABORT; }
	| var PEQ expr				{ $$ = cmdparser.addOperator(Command::OperatorAssignmentPlus,$1,$3); if ($$ == NULL) YYABORT; }
	| var MEQ expr				{ $$ = cmdparser.addOperator(Command::OperatorAssignmentMinus,$1,$3); if ($$ == NULL) YYABORT; }
	| var TEQ expr				{ $$ = cmdparser.addOperator(Command::OperatorAssignmentMultiply,$1,$3); if ($$ == NULL) YYABORT; }
	| var DEQ expr				{ $$ = cmdparser.addOperator(Command::OperatorAssignmentDivide,$1,$3); if ($$ == NULL) YYABORT; }
	| var					{ $$ = $1; }
	| '-' expr %prec UMINUS			{ $$ = cmdparser.addOperator(Command::OperatorNegate, $2); if ($$ == NULL) YYABORT; }
	| var PP				{ $$ = cmdparser.addOperator(Command::OperatorPostfixIncrease, $1); if ($$ == NULL) YYABORT; }
	| var MM				{ $$ = cmdparser.addOperator(Command::OperatorPostfixDecrease, $1); if ($$ == NULL) YYABORT; }
	| PP var				{ $$ = cmdparser.addOperator(Command::OperatorPrefixIncrease, $2); if ($$ == NULL) YYABORT; }
	| MM var				{ $$ = cmdparser.addOperator(Command::OperatorPrefixDecrease, $2); if ($$ == NULL) YYABORT; }
	| PP error				{ msg.print("Prefix increment can only act on a variable value.\n"); YYABORT; }
	| MM error				{ msg.print("Prefix decrement can only act on a variable value.\n"); YYABORT; }
	| error PP				{ msg.print("Postfix increment can only act on a variable value.\n"); YYABORT; }
	| error MM				{ msg.print("Postfix decrement can only act on a variable value.\n"); YYABORT; }
	| expr '+' expr				{ $$ = cmdparser.addOperator(Command::OperatorAdd, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '-' expr				{ $$ = cmdparser.addOperator(Command::OperatorSubtract, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '*' expr				{ $$ = cmdparser.addOperator(Command::OperatorMultiply, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '/' expr				{ $$ = cmdparser.addOperator(Command::OperatorDivide, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '^' expr				{ $$ = cmdparser.addOperator(Command::OperatorPower, $1, $3); if ($$ == NULL) YYABORT; }
	| expr EQ expr				{ $$ = cmdparser.addOperator(Command::OperatorEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expr NEQ expr				{ $$ = cmdparser.addOperator(Command::OperatorNotEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '>' expr				{ $$ = cmdparser.addOperator(Command::OperatorGreaterThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expr GEQ expr				{ $$ = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '<' expr				{ $$ = cmdparser.addOperator(Command::OperatorLessThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expr LEQ expr				{ $$ = cmdparser.addOperator(Command::OperatorLessThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expr AND expr				{ $$ = cmdparser.addOperator(Command::OperatorAnd, $1, $3); if ($$ == NULL) YYABORT; }
	| expr OR expr				{ $$ = cmdparser.addOperator(Command::OperatorOr, $1, $3); if ($$ == NULL) YYABORT; }
	| '(' expr ')'				{ $$ = $2; }
	| '!' expr				{ $$ = cmdparser.addOperator(Command::OperatorNot, $2); if ($$ == NULL) YYABORT; }
	| NEWTOKEN				{ msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
	;


/* 3-Vector Constant / Assignment Group */
VECCONST:
	'#' expr ',' expr ',' expr '#'		{ $$ = cmdparser.addVecConstant(VTypes::VectorData, $2, $4, $6); }
	;

/* Function */

func:
	FUNCCALL '(' ')'			{ $$ = cmdparser.addFunction( (Command::Function) $1); if ($$ == NULL) YYABORT; }
	| FUNCCALL '(' exprlist ')' 		{ $$ = cmdparser.addFunctionWithArglist( (Command::Function) $1,$3); if ($$ == NULL) YYABORT; }
	| FUNCCALL				{ $$ = cmdparser.addFunction( (Command::Function) $1); if ($$ == NULL) YYABORT; }
	;

%%

void yyerror(char *s)
{
	cmdparser.deleteCurrentTree();
//    fprintf(stdout, "%s\n", s);
}
