%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "parser/treenode.h"
#include "parser/commands.h"
#include "parser/tree.h"
#include "base/dnchar.h"

/* Prototypes */
int yylex(void);
void yyerror(char *s);

%}

/* Type Definition */
%union {
	int functionId;			/* function id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
};

%token <name> TOKENNAME
%token <node> INTCONST REALCONST CHARCONST VARIABLE FUNCTION
%token INTEGER REAL CHARACTER VECTOR WHILE IF PRINT
%nonassoc IFX
%nonassoc ELSE

%left GE LE EQ NE '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS
%token ';'

%type <node> expr statement statementlist declaration
%type <name> namelist

%%

function:
	function statementlist			{ Tree::currentTree->addStatement($2); }
	| /* NULL */
	;

statementlist:
	statement				{ $$ = $1; }
        | statementlist statement		{ $$ = Tree::currentTree->addJoiner($1, $2); }
        ;

statement:
	';'					{ $$ = Tree::currentTree->addJoiner(NULL,NULL); }
	| declaration				{ $$ = $1; }
	| expr ';'				{ $$ = $1; }
       /*| PRINT expr ';'                 { $$ = opr(PRINT, 1, $2); } */
/*        | VARIABLE '=' expr ';'          { $$ = opr('=', 2, id($1), $3); } */
/*        | WHILE '(' expr ')' stmt        { $$ = opr(WHILE, 2, $3, $5); } */
/*        | IF '(' expr ')' stmt %prec IFX { $$ = opr(IF, 2, $3, $5); } */
/*        | IF '(' expr ')' stmt ELSE stmt { $$ = opr(IF, 3, $3, $5, $7); } */
/*        | '{' stmt_list '}'              { $$ = $2; } */
	;

namelist:
	TOKENNAME				{ Tree::currentTree->addVariable(NuVTypes::IntegerData,$1); }
	| TOKENNAME '=' expr			{ Tree::currentTree->addVariable(NuVTypes::IntegerData,$1,$3); }
	| namelist ',' TOKENNAME '=' expr	{ Tree::currentTree->addVariable(NuVTypes::IntegerData,$3,$5);}
	| namelist ',' TOKENNAME		{ Tree::currentTree->addVariable(NuVTypes::IntegerData,$3); }
	;

declaration:
	INTEGER namelist ';'			{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::Declarations,0); }
	;

expr:
	INTCONST				{ $$ = $1; }
	| REALCONST				{ $$ = $1; }
	| CHARCONST				{ $$ = $1; }
	| VARIABLE				{ $$ = $1; }
 /*        | '-' expr %prec UMINUS			{ $$ = opr(UMINUS, 1, $2); } */
	| expr '+' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorAdd, 2, $1, $3); }
	| expr '-' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorSubtract, 2, $1, $3); }
	| expr '*' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorMultiply, 2, $1, $3); }
	| expr '/' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorDivide, 2, $1, $3); }
	| expr '^' expr				{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::OperatorPower, 2, $1, $3); }
/*        | expr '<' expr				{ $$ = opr('<', 2, $1, $3); } */
/*        | expr '>' expr				{ $$ = opr('>', 2, $1, $3); } */
/*        | expr GE expr				{ $$ = opr(GE, 2, $1, $3); } */
/*        | expr LE expr				{ $$ = opr(LE, 2, $1, $3); } */
/*        | expr NE expr				{ $$ = opr(NE, 2, $1, $3); } */
/*        | expr EQ expr				{ $$ = opr(EQ, 2, $1, $3); } */
/*        | '(' expr ')'				{ $$ = $2; } */
        ;

%%

void yyerror(char *s) {
    fprintf(stdout, "%s\n", s);
}
