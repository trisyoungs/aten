/* -------- */
/* Prologue */
/* -------- */
%{

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "parser/commands.h"
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
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
};

%token <name> TOKENNAME
%token <node> INTCONST REALCONST CHARCONST VARIABLE FUNCTIONCALL
%token INTEGER REAL CHARACTER VECTOR
%token WHILE IF PRINT FOR
%nonassoc ELSE

%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/'
%left '^'
%nonassoc UMINUS
%token ';'

%type <node> expr statement statementlist declaration exprlist
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
/* {} doesn't work after ELSE */

statement:
	';'						{ $$ = Tree::currentTree->addJoiner(NULL,NULL); }
	| declaration					{ $$ = $1; }
	| expr ';'					{ $$ = $1; }
	| VARIABLE '=' expr ';'				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorAssignment,2,$1,$3); }
	| IF '(' expr ')' statementlist			{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::If,2,$3,$5);  }
	| IF '(' expr ')' statementlist ELSE statementlist	{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::If,3,$3,$5,$7);  }
	;

/* Variable declaration / assignment list */

namelist:
	TOKENNAME				{ Tree::currentTree->addVariable(variableType,$1); }
	| TOKENNAME '=' expr			{ Tree::currentTree->addVariable(variableType,$1,$3); }
	| namelist ',' TOKENNAME '=' expr	{ Tree::currentTree->addVariable(variableType,$3,$5);}
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
	;

/* Expressions */

exprlist:
	expr					{ printf("Expression argument.\n");  }
	| exprlist ',' expr			{ printf("Expression arguments.\n");  }
	;

expr:
	INTCONST				{ $$ = $1; }
	| REALCONST				{ $$ = $1; }
	| CHARCONST				{ $$ = $1; }
	| VARIABLE				{ $$ = $1; }
	| '-' expr %prec UMINUS			{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorNegate, 1, $2); }
	| expr '+' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorAdd, 2, $1, $3); }
	| expr '-' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorSubtract, 2, $1, $3); }
	| expr '*' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorMultiply, 2, $1, $3); }
	| expr '/' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorDivide, 2, $1, $3); }
	| expr '^' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorPower, 2, $1, $3); }
	| expr EQ expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorEqualTo, 2, $1, $3); }
	| expr NEQ expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorNotEqualTo, 2, $1, $3); }
	| expr '>' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorGreaterThan, 2, $1, $3); }
	| expr GEQ expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorGreaterThanEqualTo, 2, $1, $3); }
	| expr '<' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorLessThan, 2, $1, $3); }
	| expr LEQ expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorLessThanEqualTo, 2, $1, $3); }
/*        | '(' expr ')'				{ $$ = $2; } */
	| FUNCTIONCALL '(' ')'			{ $$ = $1; }
	| FUNCTIONCALL	'(' exprlist ')'	{ $$ = $1; $1->addArgument($3); }
	;

%%

void yyerror(char *s) {
    fprintf(stdout, "%s\n", s);
}
