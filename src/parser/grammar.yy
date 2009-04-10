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
	NuVariable *variable;		/* variable pointer */
};

%token <name> NEWTOKEN INTCONST REALCONST CHARCONST STEPTOKEN
%token <variable> VARNAME
%token <functionId> FUNCCALL
%token DECLARATION DO WHILE FOR IF FILTERBLOCK
%nonassoc ELSE

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
	statementlist				{ if (!nuparser.addStatement($1)) YYABORT; }
	| filter				{ if (!nuparser.addStatement($1)) YYABORT; nuparser.finishTree(); }
	;

/* Compound Statement */

block:
	'{' scope statementlist '}' descope	{ $$ = nuparser.joinCommands($2,$3); }
        ;

scope:
	/* empty */				{ $$ = nuparser.pushScope(); if ($$ == NULL) YYABORT; }
	;

descope:
	/* empty */				{ if (!nuparser.popScope()) YYABORT; }
	;

statementlist:
	statement				{ $$ = $1; }
        | statementlist statement 		{ $$ = nuparser.joinCommands($1, $2); }
        | statementlist block	 		{ $$ = nuparser.joinCommands($1, $2); }
        ;

blockment:
	statement				{ $$ = $1; }
	| block					{ $$ = $1; }
	;

/* Filter Definitions */

optlist:
	NEWTOKEN assign '=' constant noassign	{ if (!nuparser.setFilterOption(&newVarName, $4)) YYABORT; }
	| optlist ',' NEWTOKEN assign '=' constant noassign	{ if (!nuparser.setFilterOption(&newVarName, $6)) YYABORT; }
	;

filter:
	FILTERBLOCK '(' optlist ')' block	{ $$ = $5; }
	| FILTERBLOCK error			{ msg.print("Error reading filter block definition.\n"); YYABORT; }
	;

/* Single Statement / Flow Control */

statement:
	';'					{ $$ = nuparser.addFunction(NuCommand::NoFunction); }
	| stexpr ';'			{ $$ = $1; }
	| flowstatement				{ $$ = $1; }
	;

stexpr:
	DECLARATION namelist 			{ $$ = nuparser.addFunction(NuCommand::NoFunction); nuparser.setDeclaredVariableType(NuVTypes::NoData); }
	| expr					{ $$ = $1; }
	| DECLARATION error			{ msg.print("Invalid declaration statement.\n"); YYABORT; }
	;

flowstatement:
	IF '(' expr ')' blockment ELSE blockment
		{ $$ = nuparser.addFunction(NuCommand::If,$3,$5,$7); }
	| IF '(' expr ')' blockment
		{ $$ = nuparser.addFunction(NuCommand::If,$3,$5); }
	| FOR scope '(' stexpr ';' stexpr ';' stexpr ')' blockment
		{ $$ = nuparser.joinCommands($2, nuparser.addFunction(NuCommand::For, $4,$6,$8,$10)); nuparser.popScope(); }
	| WHILE scope '(' expr ')' blockment
		{ $$ = nuparser.joinCommands($2, nuparser.addFunction(NuCommand::While, $4,$6)); nuparser.popScope(); }
	| DO scope blockment WHILE '(' expr ')'
		{ $$ = nuparser.joinCommands($2, nuparser.addFunction(NuCommand::DoWhile, $3,$6)); nuparser.popScope(); }
	;

/* Range (X~Y) */
range:
	expr '~' expr				{ printf("GENERATE RANGE. TGAY\n"); }
	;

/* Constants */

constant:
	INTCONST				{ $$ = nuparser.addConstant(NuVTypes::IntegerData, $1); }
	| REALCONST				{ $$ = nuparser.addConstant(NuVTypes::RealData, $1); }
	| CHARCONST				{ $$ = nuparser.addConstant(NuVTypes::StringData, $1); }
	;

/* Variable declaration  name / assignment list */

assign:
	/* empty */				{ newVarName = *yylval.name; nuparser.setDeclarationAssignment(TRUE); }
	;

noassign:
	/* empty */				{ nuparser.setDeclarationAssignment(FALSE); }
	;

newname:
	NEWTOKEN				{ $$ = nuparser.addVariable($1); }
	| NEWTOKEN assign '[' expr ']' noassign	{ $$ = nuparser.addArrayVariable(&newVarName,$4); }
	| NEWTOKEN assign '=' expr noassign	{ $$ = nuparser.addVariable(&newVarName,$4); }
	| NEWTOKEN assign '[' expr ']' '=' expr noassign	{ $$ = nuparser.addArrayVariable(&newVarName,$4,$7); }
	;

namelist:
	newname					{ $$ = $1; }
	| namelist ',' newname			{ $$ = Tree::joinArguments($3,$1); }
	| constant				{ msg.print("Error: Constant value found in declaration.\n"); YYABORT; }
	| namelist newname			{ msg.print("Error: Missing comma between declarations?\n"); YYABORT; }
	;

/* Variables / Paths */

step:
	STEPTOKEN savename '[' expr ']'		{ if (!nuparser.expandPath(&newStepName, $4)) YYABORT; }
	| STEPTOKEN savename '[' constant ']'	{ if (!nuparser.expandPath(&newStepName, $4)) YYABORT; }
	| STEPTOKEN				{ if (!nuparser.expandPath($1)) YYABORT; }
	;

savename:
	/* empty */				{ newStepName = *yylval.name; }
	;

steplist:
	step 					{  }
	| steplist '.' step			{  }
	;

var:
	VARNAME '[' expr ']'			{ $$ = nuparser.wrapVariable($1,$3); if ($$ == NULL) YYABORT; }
	| VARNAME				{ $$ = nuparser.wrapVariable($1); if ($$ == NULL) YYABORT; }
	| var '.' 				{ $$ = nuparser.createPath($1); }
		steplist			{ $$ = nuparser.finalisePath(); }
	;

/* Expressions */

exprlist:
	expr					{ $$ = $1; }
	| exprlist ',' expr			{ $$ = Tree::joinArguments($3,$1); }
	;

expr:
	constant				{ $$ = $1; }
	| func					{ $$ = $1; }
	| var '=' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignment,$1,$3); if ($$ == NULL) YYABORT; }
	| var '=' error				{ msg.print("Mangled expression used in assignment.\n"); YYABORT; }
	| var PEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignmentPlus,$1,$3); if ($$ == NULL) YYABORT; }
	| var MEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignmentMinus,$1,$3); if ($$ == NULL) YYABORT; }
	| var TEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignmentMultiply,$1,$3); if ($$ == NULL) YYABORT; }
	| var DEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignmentDivide,$1,$3); if ($$ == NULL) YYABORT; }
	| var					{ $$ = $1; }
	| '-' expr %prec UMINUS			{ $$ = nuparser.addOperator(NuCommand::OperatorNegate, $2); if ($$ == NULL) YYABORT; }
	| var PP				{ $$ = nuparser.addOperator(NuCommand::OperatorPostfixIncrease, $1); if ($$ == NULL) YYABORT; }
	| var MM				{ $$ = nuparser.addOperator(NuCommand::OperatorPostfixDecrease, $1); if ($$ == NULL) YYABORT; }
	| PP var				{ $$ = nuparser.addOperator(NuCommand::OperatorPrefixIncrease, $2); if ($$ == NULL) YYABORT; }
	| MM var				{ $$ = nuparser.addOperator(NuCommand::OperatorPrefixDecrease, $2); if ($$ == NULL) YYABORT; }
	| PP error				{ msg.print("Prefix increment can only act on a variable value.\n"); YYABORT; }
	| MM error				{ msg.print("Prefix decrement can only act on a variable value.\n"); YYABORT; }
	| error PP				{ msg.print("Postfix increment can only act on a variable value.\n"); YYABORT; }
	| error MM				{ msg.print("Postfix decrement can only act on a variable value.\n"); YYABORT; }
	| expr '+' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAdd, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '-' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorSubtract, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '*' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorMultiply, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '/' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorDivide, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '^' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorPower, $1, $3); if ($$ == NULL) YYABORT; }
	| expr EQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expr NEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorNotEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '>' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorGreaterThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expr GEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorGreaterThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| expr '<' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorLessThan, $1, $3); if ($$ == NULL) YYABORT; }
	| expr LEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorLessThanEqualTo, $1, $3); if ($$ == NULL) YYABORT; }
	| '(' expr ')'				{ $$ = $2; }
	| '!' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorNot, $2); if ($$ == NULL) YYABORT; }
	| NEWTOKEN				{ msg.print("Error: '%s' has not been declared.\n", yylval.name->get()); YYABORT; }
	;


/* 3-Vector Constant / Assignment Group */
VECCONST:
	'#' expr ',' expr ',' expr '#'		{ $$ = nuparser.addVecConstant(NuVTypes::VectorData, $2, $4, $6); }
	;

/* Function */

func:
	FUNCCALL '(' ')'			{ $$ = nuparser.addFunction( (NuCommand::Function) $1); if ($$ == NULL) YYABORT; }
	| FUNCCALL '(' exprlist ')' 		{ $$ = nuparser.addFunctionWithArglist( (NuCommand::Function) $1,$3); if ($$ == NULL) YYABORT; }
	| FUNCCALL				{ $$ = nuparser.addFunction( (NuCommand::Function) $1); if ($$ == NULL) YYABORT; }
	;

%%

void yyerror(char *s)
{
	nuparser.deleteCurrentTree();
//    fprintf(stdout, "%s\n", s);
}
