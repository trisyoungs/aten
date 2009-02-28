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
	int functionId;			/* function id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
};

%token <name> TOKENNAME
%token <node> INTCONST REALCONST CHARCONST VARIABLE
%token <functionId> FUNCTIONCALL
%token INTEGER REAL CHARACTER VECTOR
%token WHILE IF PRINT FOR
%nonassoc ELSE

%left GE LE EQ NE '>' '<'
%left '+' '-'
%left '*' '/'
%left '^'
%nonassoc UMINUS
%token ';'

%type <node> expr statement statementlist declaration function argumentlist
%type <name> namelist

%%

program:
	program statementlist			{ Tree::currentTree->addStatement($2); }
	| /* NULL */
	;

/* Compound Statement */

statementlist:
	statement				{ $$ = $1; }
        | statementlist statement		{ $$ = Tree::currentTree->addJoiner($1, $2); }
        ;

/* Single Statement */

statement:
	';'						{ $$ = Tree::currentTree->addJoiner(NULL,NULL); }
	| declaration					{ $$ = $1; }
	| expr ';'					{ $$ = $1; }
       /*| PRINT expr ';'                	 { $$ = opr(PRINT, 1, $2); } */
/*        | VARIABLE '=' expr ';'          { $$ = opr('=', 2, id($1), $3); } */
/*        | WHILE '(' expr ')' stmt        { $$ = opr(WHILE, 2, $3, $5); } */
	| IF '(' expr ')' statement			{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::If,2,$3,$5);  }
	| IF '(' expr ')' '{' statementlist '}' 	{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::If,2,$3,$6);  }
	| IF '(' expr ')' '{' statementlist '}' ELSE statement	{ $$ = Tree::currentTree->addCommandLeaf(NuCommand::If,3,$3,$6,$9);  }
	| IF '(' expr ')' '{' statementlist '}' ELSE '{' statementlist '}' { $$ = Tree::currentTree->addCommandLeaf(NuCommand::If,3,$3,$6,$10);  }
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
/*        | expr '<' expr				{ $$ = opr('<', 2, $1, $3); } */
/*        | expr '>' expr				{ $$ = opr('>', 2, $1, $3); } */
/*        | '(' expr ')'				{ $$ = $2; } */
	| function				{ $$ = $1; }
	;

/* Function / argument list */

argumentlist:
	/* empty */				{ printf("No args given to function.\n"); }
	| expr					{ printf("Expression argument.\n");  }
	| argumentlist ',' expr			{ printf("Expression arguments.\n");  }
	;

function:
	FUNCTIONCALL 				{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function)yylval.functionId ); }
	'(' argumentlist ')'			{  }
	;


%%

void yyerror(char *s) {
    fprintf(stdout, "%s\n", s);
}
