/* -------- */
/* Prologue */
/* -------- */
%{

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "nucommand/commands.h"
#include "parser/treenode.h"

#include "parser/tree.h"
#include "base/dnchar.h"

/* Prototypes */
int yylex(void);
void yyerror(char *s);

/* Local Variables */
/* NuVTypes::DataType variableType = NuVTypes::NoData;  */

%}

/* Type Definition */
%union {
	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
};

%token <name> TOKENNAME
%token <node> INTCONST REALCONST CHARCONST
%token <node> NUMVAR CHARVAR PTRVAR VECVAR
%token <node> NUMSTEP PTRSTEP CHARSTEP VECSTEP
%token <functionId> NUMFUNCCALL CHARFUNCCALL VOIDFUNCCALL PTRFUNCCALL VECFUNCCALL
%token DECLARATION WHILE FOR
%token <node> IF
%nonassoc ELSE

%left '=' PEQ MEQ TEQ DEQ
%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS
%right '^'
%token ';'
%nonassoc BOB

%type <node> numexpr charexpr ptrexpr vecexpr anyexpr
%type <node> numfunc ptrfunc vecfunc charfunc voidfunc
%type <node> pathvar step steplist
%type <node> statement statementlist exprlist VECCONST
%type <node> namelist newname

%%

program:
	program statementlist			{ Tree::currentTree->addStatement($2); }
	| /* NULL */
	;

/* Compound Statement */

statementlist:
	statement				{ $$ = $1;printf("End of statement(list).\n");  }
	| '{' statement '}'			{ $$ = $2; printf("End of statement(inbrackets).\n"); }
        | '{' statementlist statement '}'	{ $$ = Tree::currentTree->addJoiner($2, $3); printf("End of statementlist.\n"); }
        ;

/* Single Statement */

statement:
	';'					{ $$ = Tree::currentTree->addJoiner(NULL,NULL); }
	| DECLARATION namelist ';'		{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations, $2); Tree::currentTree->setDeclaredVariableType(NuVTypes::NoData); }
	| numexpr ';'				{ $$ = $1; }
	| charexpr ';'				{ $$ = $1; }
	| vecexpr ';'				{ $$ = $1; }
	| ptrexpr ';'				{ $$ = $1; }
	| voidfunc ';'				{ $$ = $1; }
	| IF '(' anyexpr ')' statementlist	%prec BOB	{ $$ = $1; $1->addArguments(2,$3,$5); Tree::currentTree->popScope(); }
	| IF '(' anyexpr ')' statementlist ELSE statementlist	{ $$ = $1; $1->addArguments(3,$3,$5,$7); Tree::currentTree->popScope(); }
	| FOR '(' numexpr ';' numexpr ';' numexpr ')' statementlist
				{ $$ = Tree::currentTree->addScopedLeaf(NuCommand::For,4,$3,$5,$7,$9); Tree::currentTree->popScope(); };
	;

/* Variable declaration  name / assignment list */

newname:
	TOKENNAME				{ $$ = Tree::currentTree->addVariable($1); }
	| TOKENNAME '=' anyexpr			{ $$ = Tree::currentTree->addVariable($1,$3); }
	;
	
namelist:
	newname					{ $$ = $1; }
	| namelist ',' newname			{ $$ = Tree::joinArguments($3,$1); }
	;

/* Expressions */

exprlist:
	anyexpr					{ $$ = $1; }
	| exprlist ',' anyexpr			{ $$ = Tree::joinArguments($3,$1); }
	;

anyexpr:
	numexpr					{ $$ = $1; }
	| charexpr				{ $$ = $1; }
	| ptrexpr				{ $$ = $1; }
	| vecexpr				{ $$ = $1; }
	;

charexpr:
	CHARCONST				{ $$ = $1; }
	| charfunc				{ $$ = $1; }
	| CHARVAR '=' charexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| CHARVAR				{ $$ = $1; }
	| charexpr '+' charexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAdd, 1, $1, $3); }
	| charexpr '*' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorMultiply, 1, $1, $3); }
	| numexpr '*' charexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorMultiply, 2, $1, $3); }
	;

ptrexpr:
	ptrfunc					{ $$ = $1; }
	| pathvar '.' PTRSTEP			{ Tree::currentTree->expandPath($3); $$ = Tree::currentTree->finalisePath(); }
	| pathvar '.' steplist '.' PTRSTEP	{ Tree::currentTree->expandPath($5); $$ = Tree::currentTree->finalisePath(); }
	| PTRVAR '=' ptrexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| PTRVAR				{ $$ = $1; }
	| ptrexpr EQ ptrexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorEqualTo, 99, $1, $3); }
	;

vecexpr:
	VECCONST				{ $$ = $1; }
	| vecfunc				{ $$ = $1; }
	| VECVAR '=' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| VECVAR PEQ vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentPlus,1,$1,$3); }
	| VECVAR MEQ vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentMinus,1,$1,$3); }
	| VECVAR TEQ vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentMultiply,1,$1,$3); }
	| VECVAR DEQ vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentDivide,1,$1,$3); }
	| VECVAR				{ $$ = $1; }
	| vecexpr '*' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorMultiply, 1, $1, $3); }
	| vecexpr '-' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAdd, 1, $1, $3); }
	| vecexpr '+' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorSubtract, 1, $1, $3); }
	;

numexpr:
	INTCONST				{ $$ = $1; }
	| REALCONST				{ $$ = $1; }
	| numfunc				{ $$ = $1; }
	| pathvar '.' NUMSTEP			{ Tree::currentTree->expandPath($3); $$ = Tree::currentTree->finalisePath(); }
	| pathvar '.' steplist '.' NUMSTEP	{ Tree::currentTree->expandPath($5); $$ = Tree::currentTree->finalisePath(); }
	| NUMVAR '=' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| NUMVAR PEQ numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentPlus,1,$1,$3); }
	| NUMVAR MEQ numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentMinus,1,$1,$3); }
	| NUMVAR TEQ numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentMultiply,1,$1,$3); }
	| NUMVAR DEQ numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentDivide,1,$1,$3); }
	| NUMVAR				{ $$ = $1; }
	| '-' numexpr %prec UMINUS		{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorNegate,1, $2); }
	| numexpr '+' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAdd, 0, $1, $3); }
	| numexpr '-' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorSubtract, 0, $1, $3); }
	| numexpr '*' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorMultiply, 0, $1, $3); }
	| numexpr '/' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorDivide, 0, $1, $3); }
	| numexpr '^' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorPower, 0, $1, $3); }
	| numexpr EQ numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorEqualTo, 99, $1, $3); }
	| numexpr NEQ numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorNotEqualTo, 99, $1, $3); }
	| numexpr '>' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorGreaterThan, 99, $1, $3); }
	| numexpr GEQ numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorGreaterThanEqualTo, 99, $1, $3); }
	| numexpr '<' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorLessThan, 99, $1, $3); }
	| numexpr LEQ numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorLessThanEqualTo, 99, $1, $3); }
	| '(' numexpr ')'			{ $$ = $2; }
	;

/* Variable paths */

step:
	PTRSTEP					{ $$ = $1; Tree::currentTree->expandPath($1); }
	| VECSTEP				{ $$ = $1; }
	;

steplist:
	/* empty */				{ $$ = NULL; }
	| step 					{ $$ = $1; printf("Crap.\n"); }
/*	| step '[' numexpr ']'			{ } */
	| steplist '.' step			{ $$ = Tree::currentTree->joinArguments($1, $3); }
	;

pathvar:
	PTRVAR					{ printf("HEllo ptrpathvar.\n"); $$ = Tree::currentTree->createPath($1); }
	| VECVAR				{ printf("HEllo vacpathvar.\n"); $$ = Tree::currentTree->createPath($1); }
	;

/* 3-Vector Constant / Assignment Group */
VECCONST:
	'{' numexpr ',' numexpr ',' numexpr '}'	{ $$ = Tree::currentTree->addVecConstant(NuVTypes::VectorData, $2, $4, $6); }
	;

/* Function Definitions */

numfunc:
	NUMFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,NULL); }
	| NUMFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,$3); }
	;

charfunc:
	CHARFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,NULL); }
	| CHARFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,$3); }
	;

ptrfunc:
	PTRFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,NULL); }
	| PTRFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,$3); }
	;

vecfunc:
	VECFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,NULL); }
	| VECFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,$3); }
	;

voidfunc:
	VOIDFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,NULL); }
	| VOIDFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,$3); }
	;

%%

void yyerror(char *s)
{
//    fprintf(stdout, "%s\n", s);
}
