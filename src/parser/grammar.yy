%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "parser/tree.h"
#include "parser/treenode.h"
#include "parser/commands.h"

/* Prototypes */
//nodeType *opr(int oper, int nops, ...);
//nodeType *id(int i);
//nodeType *con(int value);
//void freeNode(nodeType *p);
//int ex(nodeType *p);
int yylex(void);

void yyerror(char *s);
//int sym[26];                    /* symbol table */
%}

/* Type Definition */
%union {
	/* int iValue;                  integer value */
	/* double rValue;               real value */
	/* char sIndex;                 symbol table index */
	TreeNode *node;             /* node pointer */
};

%token <node> INTEGER
%token WHILE IF PRINT
%nonassoc IFX
%nonassoc ELSE

%left GE LE EQ NE '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%type <node> expr

%%

expr:
          INTEGER				{ $$ = Tree::currentTree->addLeaf($1); }
       /* | VARIABLE				{ $$ = id($1); } */
 /*        | '-' expr %prec UMINUS			{ $$ = opr(UMINUS, 1, $2); } */
        | expr '+' expr				{ $$ = Tree::currentTree->addCommand(NuCommand::Addition, 2, $1, $3); }
        | expr '-' expr				{ $$ = Tree::currentTree->addCommand(NuCommand::Subtraction, 2, $1, $3); }
/*        | expr '*' expr				{ $$ = opr('*', 2, $1, $3); } */
/*        | expr '/' expr				{ $$ = opr('/', 2, $1, $3); } */
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
