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
%token DECLARATION WHILE FOR IF
%nonassoc ELSE

%left '=' PEQ MEQ TEQ DEQ
%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS
%right '^'

%type <node> constant expr func var
%type <node> flowstatement statementexpr statement block blockment statementlist exprlist VECCONST
%type <node> namelist newname

%%

program:
	program statementlist			{ nuparser.tree->addStatement($2); }
	| /* NULL */
	;

/* Filter Definitions */


	
/* Compound Statement */

block:
	'{'					{ nuparser.tree->pushScope(); }
		statementlist '}'		{ $$ = $3; nuparser.tree->popScope(); }
        ;

statementlist:
	statement				{ $$ = $1; }
        | statementlist statement 		{ $$ = nuparser.tree->joinFunctions($1, $2); }
        | statementlist block	 		{ $$ = nuparser.tree->joinFunctions($1, $2); }
        ;

blockment:
	statement				{ $$ = $1; }
	| block					{ $$ = $1; }
	;

/* Single Statement / Flow Control */

statement:
	';'					{ $$ = nuparser.tree->joinFunctions(NULL,NULL); }
	| statementexpr ';'			{ $$ = $1; }
	| flowstatement				{ $$ = $1; }
	;

statementexpr:
	DECLARATION namelist 			{ $$ = nuparser.tree->addFunctionLeaf(NuCommand::Initialisations, $2); nuparser.tree->setDeclaredVariableType(NuVTypes::NoData); }
	| expr					{ $$ = $1; }
	;

flowstatement:
	IF '(' expr ')' blockment ELSE blockment	{ $$ = nuparser.tree->addIf($3,$5,$7); }
	| IF '(' expr ')' blockment		{ $$ = nuparser.tree->addIf($3,$5); }
	| FOR createscope '(' statementexpr ';' statementexpr ';' statementexpr ')' blockment	{ $$ = nuparser.tree->joinArguments(nuparser.tree->addFor($4,$6,$8,$10), $$); nuparser.tree->popScope(); }
	;

createscope:
	/* empty */				{ nuparser.tree->pushScope(); }
	;

/* Constants */

constant:
	INTCONST				{ $$ = nuparser.tree->addConstant(NuVTypes::IntegerData, $1); }
	| REALCONST				{ $$ = nuparser.tree->addConstant(NuVTypes::RealData, $1); }
	| CHARCONST				{ $$ = nuparser.tree->addConstant(NuVTypes::CharacterData, $1); }
	;

/* Variable declaration  name / assignment list */

assign:
	/* empty */				{ newVarName = *yylval.name; nuparser.tree->setDeclarationAssignment(TRUE); }
	;

noassign:
	/* empty */				{ nuparser.tree->setDeclarationAssignment(FALSE); }
	;

newname:
	NEWTOKEN				{ $$ = nuparser.tree->addVariable($1); }
	| NEWTOKEN assign '[' expr ']' noassign	{ $$ = nuparser.tree->addArrayVariable(&newVarName,$4); }
	| NEWTOKEN assign '=' expr noassign	{ $$ = nuparser.tree->addVariable(&newVarName,$4); }
	| NEWTOKEN assign '[' expr ']' '=' expr noassign	{ $$ = nuparser.tree->addArrayVariable(&newVarName,$4,$7); }
	;
	
namelist:
	newname					{ $$ = $1; }
	| namelist ',' newname			{ $$ = Tree::joinArguments($3,$1); }
	| constant				{ msg.print("Error: Constant value found in declaration.\n"); YYERROR; }
	| namelist newname			{ msg.print("Error: Missing comma between declarations?\n"); YYERROR; }
	;

/* Variables / Paths */

step:
	STEPTOKEN				{ if (!nuparser.tree->expandPath($1)) YYERROR; }
	| STEPTOKEN '[' expr ']'		{ if (!nuparser.tree->expandPath($1, $3)) YYERROR; }
;

steplist:
	step 					{  }
	| steplist '.' step			{  }
	;

var:
	VARNAME '[' expr ']'			{ $$ = nuparser.tree->wrapVariable($1,$3); if ($$ == NULL) YYERROR; }
	| VARNAME				{ $$ = nuparser.tree->wrapVariable($1); if ($$ == NULL) YYERROR; }
	| var '.' 				{ printf("HEllO pathvar.\n"); $$ = nuparser.tree->createPath($1); }
		steplist			{ $$ = nuparser.tree->finalisePath(); }
	;

/* Expressions */

exprlist:
	expr					{ $$ = $1; }
	| exprlist ',' expr			{ $$ = Tree::joinArguments($3,$1); }
	;

expr:
	constant				{ $$ = $1; }
	| func					{ $$ = $1; }
/*	| pathvar '.' NUMSTEP			{ nuparser.tree->expandPath($3); $$ = nuparser.tree->finalisePath(); }  */
/*	| pathvar '.' steplist '.' NUMSTEP	{ nuparser.tree->expandPath($5); $$ = nuparser.tree->finalisePath(); }  */
	| var '=' expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var PEQ expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorAssignmentPlus,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var MEQ expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorAssignmentMinus,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var TEQ expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorAssignmentMultiply,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var DEQ expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorAssignmentDivide,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var					{ $$ = $1; }
	| '-' expr %prec UMINUS			{ $$ = nuparser.tree->addOperator(NuCommand::OperatorNegate,1, $2); if ($$ == NULL) YYERROR; }
	| expr '+' expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorAdd, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '-' expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorSubtract, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '*' expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorMultiply, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '/' expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorDivide, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '^' expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorPower, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr EQ expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr NEQ expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorNotEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '>' expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorGreaterThan, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr GEQ expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorGreaterThanEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '<' expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorLessThan, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr LEQ expr				{ $$ = nuparser.tree->addOperator(NuCommand::OperatorLessThanEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| '(' expr ')'				{ $$ = $2; }
	| NEWTOKEN				{ msg.print("Error: '%s' has not been declared.\n", yylval.name->get()); YYERROR; }
	;


/* 3-Vector Constant / Assignment Group */
VECCONST:
	'#' expr ',' expr ',' expr '#'		{ $$ = nuparser.tree->addVecConstant(NuVTypes::VectorData, $2, $4, $6); }
	;

/* Function */

func:
	FUNCCALL '(' ')'			{ $$ = nuparser.tree->addFunctionLeaf( (NuCommand::Function) $1,NULL); if ($$ == NULL) YYERROR; }
	| FUNCCALL	'(' exprlist ')' 	{ $$ = nuparser.tree->addFunctionLeaf( (NuCommand::Function) $1,$3); if ($$ == NULL) YYERROR; }
	;

%%

void yyerror(char *s)
{
//    fprintf(stdout, "%s\n", s);
}
