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
%token DECLARATION WHILE FOR IF FILTERBLOCK
%nonassoc ELSE

%left '=' PEQ MEQ TEQ DEQ 
%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS
%left PP MM
%right '^'

%type <node> constant expr func var
%type <node> flowstatement statementexpr statement block blockment statementlist exprlist VECCONST
%type <node> namelist newname
%type <node> filter scope

%%

programlist:
	program					{ }
	| programlist program			{ }
	;

program:
	statementlist				{ if (!nuparser.addStatement($1)) YYERROR; }
	| filter				{ if (!nuparser.addStatement($1)) YYERROR; nuparser.finishTree(); }
	;

/* Compound Statement */

block:
	'{' scope statementlist '}' descope	{ $$ = nuparser.joinCommands($2,$3); }
        ;

scope:
	/* empty */				{ $$ = nuparser.pushScope(); if ($$ == NULL) YYERROR; }
	;

descope:
	/* empty */				{ if (!nuparser.popScope()) YYERROR; }
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
	NEWTOKEN assign '=' constant noassign	{ if (!nuparser.setFilterOption(&newVarName, $4)) YYERROR; }
	| optlist ',' NEWTOKEN assign '=' constant noassign	{ if (!nuparser.setFilterOption(&newVarName, $6)) YYERROR; }
	;

filter:
	FILTERBLOCK '(' optlist ')' block	{ $$ = $5; }
	| FILTERBLOCK error			{ msg.print("Error reading filter block definition.\n"); YYERROR; }
	;

/* Single Statement / Flow Control */

statement:
	';'					{ $$ = nuparser.addFunction(NuCommand::NoFunction, NULL); }
	| statementexpr ';'			{ $$ = $1; }
	| flowstatement				{ $$ = $1; }
	;

statementexpr:
	DECLARATION namelist 			{ $$ = nuparser.addFunction(NuCommand::NoFunction, NULL); nuparser.setDeclaredVariableType(NuVTypes::NoData); }
	| expr					{ $$ = $1; }
	| DECLARATION error			{ msg.print("Invalid declaration statement.\n"); YYERROR; }
	;

flowstatement:
	IF '(' expr ')' blockment ELSE blockment	{ $$ = nuparser.addIf($3,$5,$7); }
	| IF '(' expr ')' blockment		{ $$ = nuparser.addIf($3,$5); }
	| FOR scope '(' statementexpr ';' statementexpr ';' statementexpr ')' blockment	{ $$ = nuparser.joinCommands($2, nuparser.addFor($4,$6,$8,$10)); nuparser.popScope(); }
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
	| constant				{ msg.print("Error: Constant value found in declaration.\n"); YYERROR; }
	| namelist newname			{ msg.print("Error: Missing comma between declarations?\n"); YYERROR; }
	;

/* Variables / Paths */

step:
	STEPTOKEN savename '[' expr ']'		{ if (!nuparser.expandPath(&newStepName, $4)) YYERROR; }
	| STEPTOKEN				{ if (!nuparser.expandPath($1)) YYERROR; }
	;

savename:
	/* empty */				{ newStepName = *yylval.name; }
	;

steplist:
	step 					{  }
	| steplist '.' step			{  }
	;

var:
	VARNAME '[' expr ']'			{ $$ = nuparser.wrapVariable($1,$3); if ($$ == NULL) YYERROR; }
	| VARNAME				{ $$ = nuparser.wrapVariable($1); if ($$ == NULL) YYERROR; }
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
	| var '=' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignment,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var '=' error				{ msg.print("Mangled expression used in assignment.\n"); YYERROR; }
	| var PEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignmentPlus,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var MEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignmentMinus,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var TEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignmentMultiply,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var DEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAssignmentDivide,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var					{ $$ = $1; }
	| '-' expr %prec UMINUS			{ $$ = nuparser.addOperator(NuCommand::OperatorNegate,1, $2); if ($$ == NULL) YYERROR; }
	| var PP				{ $$ = nuparser.addOperator(NuCommand::OperatorPostfixIncrease, 0, $1); if ($$ == NULL) YYERROR; }
	| var MM				{ $$ = nuparser.addOperator(NuCommand::OperatorPostfixDecrease, 0, $1); if ($$ == NULL) YYERROR; }
	| PP var				{ $$ = nuparser.addOperator(NuCommand::OperatorPrefixIncrease, 0, $2); if ($$ == NULL) YYERROR; }
	| MM var				{ $$ = nuparser.addOperator(NuCommand::OperatorPrefixDecrease, 0, $2); if ($$ == NULL) YYERROR; }
	| PP error				{ msg.print("Prefix increment can only act on a variable value.\n"); YYERROR; }
	| MM error				{ msg.print("Prefix decrement can only act on a variable value.\n"); YYERROR; }
	| error PP				{ msg.print("Postfix increment can only act on a variable value.\n"); YYERROR; }
	| error MM				{ msg.print("Postfix decrement can only act on a variable value.\n"); YYERROR; }
	| expr '+' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorAdd, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '-' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorSubtract, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '*' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorMultiply, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '/' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorDivide, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '^' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorPower, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr EQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr NEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorNotEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '>' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorGreaterThan, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr GEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorGreaterThanEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '<' expr				{ $$ = nuparser.addOperator(NuCommand::OperatorLessThan, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr LEQ expr				{ $$ = nuparser.addOperator(NuCommand::OperatorLessThanEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| '(' expr ')'				{ $$ = $2; }
	| NEWTOKEN				{ msg.print("Error: '%s' has not been declared.\n", yylval.name->get()); YYERROR; }
	;


/* 3-Vector Constant / Assignment Group */
VECCONST:
	'#' expr ',' expr ',' expr '#'		{ $$ = nuparser.addVecConstant(NuVTypes::VectorData, $2, $4, $6); }
	;

/* Function */

func:
	FUNCCALL '(' ')'			{ $$ = nuparser.addFunction( (NuCommand::Function) $1,NULL); if ($$ == NULL) YYERROR; }
	| FUNCCALL	'(' exprlist ')' 	{ $$ = nuparser.addFunction( (NuCommand::Function) $1,$3); if ($$ == NULL) YYERROR; }
	;

%%

void yyerror(char *s)
{
	printf("LKJSFLKJASLKFDJ\n");
	nuparser.deleteCurrentTree();
//    fprintf(stdout, "%s\n", s);
}
