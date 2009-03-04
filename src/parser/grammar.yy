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
%token <node> NUMVAR CHARVAR VECVAR PTRVAR
%token <node> STEP
%token <functionId> NUMFUNCCALL CHARFUNCCALL VOIDFUNCCALL PTRFUNCCALL VECFUNCCALL
%token INTEGER REAL CHARACTER VECTOR ATOM BOND CELL FORCEFIELD FFATOM FFBOUND GRID MODEL PATTERN
%token WHILE IF PRINT FOR
%nonassoc ELSE

%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/'
%left '^' '.'
%nonassoc UMINUS
%token ';'

%type <node> numexpr charexpr ptrexpr vecexpr anyexpr
%type <node> numfunc charfunc ptrfunc vecfunc voidfunc
%type <node> numpath path
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
	| IF '(' anyexpr ')' statementlist			{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::If,2,$3,$5);  }
	| IF '(' anyexpr ')' statementlist ELSE statementlist	{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::If,3,$3,$5,$7);  }
	;

/* Variable declaration / assignment list */

namelist:
	TOKENNAME				{ Tree::currentTree->addVariable(variableType,$1); }
	| TOKENNAME '=' anyexpr			{ Tree::currentTree->addVariable(variableType,$1,$3); }
	| namelist ',' TOKENNAME '=' anyexpr	{ Tree::currentTree->addVariable(variableType,$3,$5);}
	| namelist ',' TOKENNAME		{ Tree::currentTree->addVariable(variableType,$3); }
	;

/* Declaration statements - uses mid-rule action to store type for use in namelist rule */

declaration:
	INTEGER { variableType = NuVTypes::IntegerData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| REAL { variableType = NuVTypes::RealData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| CHARACTER { variableType = NuVTypes::CharacterData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| VECTOR { variableType = NuVTypes::VectorData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| ATOM { variableType = NuVTypes::AtomData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| BOND { variableType = NuVTypes::BondData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| CELL { variableType = NuVTypes::CellData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| FORCEFIELD { variableType = NuVTypes::ForcefieldData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| FFATOM { variableType = NuVTypes::ForcefieldAtomData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| FFBOUND { variableType = NuVTypes::ForcefieldBoundData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| GRID { variableType = NuVTypes::GridData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| MODEL { variableType = NuVTypes::ModelData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	| PATTERN { variableType = NuVTypes::PatternData; }
	namelist ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
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
	| CHARVAR				{ $$ = $1; }
	| charfunc				{ $$ = $1; }
	| CHARVAR '=' charexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| charexpr '+' charexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAdd, 1, $1, $3); }
	| charexpr '*' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorMultiply, 1, $1, $3); }
	| numexpr '*' charexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorMultiply, 2, $1, $3); }
	;

ptrexpr:
	PTRVAR					{ $$ = $1; }
	| ptrfunc				{ $$ = $1; }
	| PTRVAR '=' ptrexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| ptrexpr EQ ptrexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorEqualTo, 99, $1, $3); }
	;

vecexpr:
	VECVAR					{ $$ = $1; }
	| VECCONST				{ $$ = $1; }
	| vecfunc				{ $$ = $1; }
	| VECVAR '=' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| vecexpr '*' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorMultiply, 1, $1, $3); }
	| vecexpr '-' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAdd, 1, $1, $3); }
	| vecexpr '+' vecexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorSubtract, 1, $1, $3); }
	;

numexpr:
	INTCONST				{ $$ = $1; }
	| REALCONST				{ $$ = $1; }
	| NUMVAR				{ $$ = $1; }
	| NUMVAR '=' numexpr			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); }
	| numfunc				{ $$ = $1; }
	| numpath				{ $$ = $1; }
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

/* Variable Path */

path:
	STEP					{ $$ = $1; }
	| path '.' STEP				{ $$ = Tree::currentTree->joinArguments($1, $3); }
	;

numpath:
	PTRVAR 					{  } '.' path '.' NUMVAR		{}
	| VECVAR '.' { Tree::currentTree->pushPath($1); } STEP { $$ = Tree::currentTree->addPath($1, $4); }
	;

/* 3-Vector Constant / Assignment Group */
VECCONST:
	'{' numexpr ',' numexpr ',' numexpr '}'	{ $$ = Tree::currentTree->addVecConstant(NuVTypes::VectorData, $2, $4, $6); }
	;

/* Function Definitions */

numfunc:
	NUMFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,0); }
	| NUMFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,1,$3); }
	;

charfunc:
	CHARFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,0); }
	| CHARFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,1,$3); }
	;

ptrfunc:
	PTRFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,0); }
	| PTRFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,1,$3); }
	;

vecfunc:
	VECFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,0); }
	| VECFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,1,$3); }
	;

voidfunc:
	VOIDFUNCCALL '(' ')'			{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,0); }
	| VOIDFUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addCommandLeaf( (NuCommand::Function) $1,1,$3); }
	;

%%

void yyerror(char *s)
{
//    fprintf(stdout, "%s\n", s);
}
