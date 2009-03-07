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
NuVTypes::DataType variableType = NuVTypes::NoData;

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
%token INTEGER REAL CHARACTER VECTOR ATOM BOND CELL FORCEFIELD FFATOM FFBOUND GRID MODEL PATTERN
%token WHILE FOR
%token <node> IF
%nonassoc ELSE

%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/'
%left '^' '.'
%nonassoc UMINUS
%token ';'

%type <node> numexpr charexpr ptrexpr vecexpr anyexpr
%type <node> numfunc ptrfunc vecfunc charfunc voidfunc
%type <node> pathvar step steplist
%type <node> statement statementlist declaration exprlist VECCONST
%type <name> namelist

%%

program:
	program statementlist			{ Tree::currentTree->addStatement($2); }
	| /* NULL */
	;

/* Compound Statement */

statementlist:
	statement				{ $$ = $1; }
	| '{' statement '}'			{ $$ = $2; }
        | '{' statementlist statement '}'	{ $$ = Tree::currentTree->addJoiner($2, $3); }
        ;

/* Single Statement */

statement:
	';'						{ $$ = Tree::currentTree->addJoiner(NULL,NULL); }
	| declaration					{ $$ = $1; }
	| numexpr ';'					{ $$ = $1; }
	| charexpr ';'					{ $$ = $1; }
	| vecexpr ';'					{ $$ = $1; }
	| ptrexpr ';'					{ $$ = $1; }
	| voidfunc ';'					{ $$ = $1; }
	| IF '(' anyexpr ')' statementlist		{ $$ = $1; $1->addArguments(2,$3,$5);  }
	| IF '(' anyexpr ')' statementlist ELSE statementlist	{ $1->addArguments(3,$3,$5,$7);  }
	| FOR '(' NUMVAR '=' numexpr ',' numexpr ')' statementlist { $$ = Tree::currentTree->addScopedLeaf(NuCommand::For,4,$3,$5,$7,$9); };
	| FOR '(' NUMVAR '=' numexpr ',' numexpr ',' numexpr ')' statementlist { $$ = Tree::currentTree->addScopedLeaf(NuCommand::For,5,$3,$5,$7,$9,$11); };
	;

/* Variable declaration / assignment list */

namelist:
	TOKENNAME				{ Tree::currentTree->addVariable(variableType,$1); }
	| TOKENNAME '=' anyexpr			{ Tree::currentTree->addVariable(variableType,$1,$3); }
	| namelist ',' TOKENNAME '=' anyexpr	{ Tree::currentTree->addVariable(variableType,$3,$5);}
	| namelist ',' TOKENNAME		{ Tree::currentTree->addVariable(variableType,$3); }
	;

/* Declaration statements - uses mid-rule action to store type for use in namelist rule */

/* Can this be reduced to a rule which stores the variable type in yylval and then retrieves it here? i.e. DECLARATION {vari.....} namelist;  */
declaration:
	INTEGER { variableType = NuVTypes::IntegerData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| REAL { variableType = NuVTypes::RealData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| CHARACTER { variableType = NuVTypes::CharacterData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| VECTOR { variableType = NuVTypes::VectorData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| ATOM { variableType = NuVTypes::AtomData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| BOND { variableType = NuVTypes::BondData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| CELL { variableType = NuVTypes::CellData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| FORCEFIELD { variableType = NuVTypes::ForcefieldData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| FFATOM { variableType = NuVTypes::ForcefieldAtomData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| FFBOUND { variableType = NuVTypes::ForcefieldBoundData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| GRID { variableType = NuVTypes::GridData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| MODEL { variableType = NuVTypes::ModelData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
	| PATTERN { variableType = NuVTypes::PatternData; }
	namelist ';'				{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations,0); }
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
	| pathvar '.' steplist '.' PTRSTEP	{ printf("POINTERPOOP.\n"); Tree::currentTree->expandPath($5); $$ = Tree::currentTree->finalisePath(); }
	| PTRVAR '=' ptrexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| PTRVAR				{ $$ = $1; }
	| ptrexpr EQ ptrexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorEqualTo, 99, $1, $3); }
	;

vecexpr:
	VECCONST				{ $$ = $1; }
	| vecfunc				{ $$ = $1; }
	| VECVAR '=' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
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
	| pathvar '.' steplist '.' NUMSTEP	{ printf("POOP.\n"); Tree::currentTree->expandPath($5); $$ = Tree::currentTree->finalisePath(); }
	| NUMVAR '=' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
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
