%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "treenode.h"

/* Prototypes */
//nodeType *opr(int oper, int nops, ...);
//nodeType *id(int i);
//nodeType *con(int value);
//void freeNode(nodeType *p);
//int ex(nodeType *p);
int yylex(void);

void yyerror(char *s);
int sym[26];                    /* symbol table */
%}

/* Type Definition */
%union {
	int iValue;                 /* integer value */
	double rValue;              /* real value */
	char sIndex;                /* symbol table index */
	TreeNode *node;             /* node pointer */
};

%token <iValue> INTEGER
%token <sIndex> VARIABLE
%token WHILE IF PRINT
%nonassoc IFX
%nonassoc ELSE

%left GE LE EQ NE '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%type <nPtr> statement expr statement_list

%%

program:
        function                { exit(0); }
        ;

function:
          function statement         { ex($2); }
        | /* NULL */
        ;

statement:
          ';'                            { $$ = opr(';', 2, NULL, NULL); }
        | expr ';'                       { $$ = $1; }
        | PRINT expr ';'                 { $$ = opr(PRINT, 1, $2); }
        | VARIABLE '=' expr ';'          { $$ = opr('=', 2, id($1), $3); }
        | WHILE '(' expr ')' statement        { $$ = opr(WHILE, 2, $3, $5); }
        | IF '(' expr ')' statement %prec IFX { $$ = opr(IF, 2, $3, $5); }
        | IF '(' expr ')' statement ELSE statement { $$ = opr(IF, 3, $3, $5, $7); }
        | '{' statement_list '}'              { $$ = $2; }
        ;

statement_list:
          statement                  { $$ = $1; }
        | statement_list statement        { $$ = opr(';', 2, $1, $2); }
        ;

expr:
          INTEGER               { $$ = con($1); }
        | VARIABLE              { $$ = id($1); }
        | '-' expr %prec UMINUS { $$ = opr(UMINUS, 1, $2); }
        | expr '+' expr         { $$ = opr('+', 2, $1, $3); }
        | expr '-' expr         { $$ = opr('-', 2, $1, $3); }
        | expr '*' expr         { $$ = opr('*', 2, $1, $3); }
        | expr '/' expr         { $$ = opr('/', 2, $1, $3); }
        | expr '<' expr         { $$ = opr('<', 2, $1, $3); }
        | expr '>' expr         { $$ = opr('>', 2, $1, $3); }
        | expr GE expr          { $$ = opr(GE, 2, $1, $3); }
        | expr LE expr          { $$ = opr(LE, 2, $1, $3); }
        | expr NE expr          { $$ = opr(NE, 2, $1, $3); }
        | expr EQ expr          { $$ = opr(EQ, 2, $1, $3); }
        | '(' expr ')'          { $$ = $2; }
        ;

%%


nodeType *con(int value) {
    nodeType *p;
    size_t nodeSize;

    /* allocate node */
    nodeSize = SIZEOF_NODETYPE + sizeof(conNodeType);
    if ((p = malloc(nodeSize)) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeCon;
    p->con.value = value;

    return p;
}

nodeType *id(int i) {
    nodeType *p;
    size_t nodeSize;

    /* allocate node */
    nodeSize = SIZEOF_NODETYPE + sizeof(idNodeType);
    if ((p = malloc(nodeSize)) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeId;
    p->id.i = i;

    return p;
}

nodeType *opr(int oper, int nops, ...) {
    va_list ap;
    nodeType *p;
    size_t nodeSize;
    int i;

    /* allocate node */
    nodeSize = SIZEOF_NODETYPE + sizeof(oprNodeType) +
        (nops - 1) * sizeof(nodeType*);
    if ((p = malloc(nodeSize)) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeOpr;
    p->opr.oper = oper;
    p->opr.nops = nops;
    va_start(ap, nops);
    for (i = 0; i < nops; i++)
        p->opr.op[i] = va_arg(ap, nodeType*);
    va_end(ap);
    return p;
}

void freeNode(nodeType *p) {
    int i;

    if (!p) return;
    if (p->type == typeOpr) {
        for (i = 0; i < p->opr.nops; i++)
            freeNode(p->opr.op[i]);
    }
    free (p);
}

void yyerror(char *s) {
    fprintf(stdout, "%s\n", s);
}

int main(void) {
    yyparse();
    return 0;
}
