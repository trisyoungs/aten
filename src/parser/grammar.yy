/* -------- */
/* Prologue */
/* -------- */
%{

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "nucommand/commands.h"
#include "parser/tree.h"

/* Prototypes */
int yylex(void);
void yyerror(char *s);

/* Local Variables */
Dnchar newVarName;

/* NuVTypes::DataType variableType = NuVTypes::NoData;  */

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
	program statementlist			{ Tree::currentTree->addStatement($2); }
	| /* NULL */
	;

/* Compound Statement */

block:
	'{'					{ Tree::currentTree->pushScope(); }
		statementlist '}'		{ $$ = $3; Tree::currentTree->popScope(); }
        ;

statementlist:
	statement				{ $$ = $1; }
        | statementlist statement 		{ $$ = Tree::currentTree->joinFunctions($1, $2); }
        | statementlist block	 		{ $$ = Tree::currentTree->joinFunctions($1, $2); }
        ;

blockment:
	statement				{ $$ = $1; }
	| block					{ $$ = $1; }
	;

/* Single Statement / Flow Control */

statement:
	';'					{ $$ = Tree::currentTree->joinFunctions(NULL,NULL); }
	| statementexpr ';'			{ $$ = $1; }
	| flowstatement				{ $$ = $1; }
	;

statementexpr:
	DECLARATION namelist 			{ $$ = Tree::currentTree->addFunctionLeaf(NuCommand::Initialisations, $2); Tree::currentTree->setDeclaredVariableType(NuVTypes::NoData); }
	| expr					{ $$ = $1; }
	;

flowstatement:
	IF '(' expr ')' blockment ELSE blockment	{ $$ = Tree::currentTree->addIf($3,$5,$7); }
	| IF '(' expr ')' blockment		{ $$ = Tree::currentTree->addIf($3,$5); }
	| FOR createscope '(' statementexpr ';' statementexpr ';' statementexpr ')' blockment	{ $$ = Tree::currentTree->joinArguments(Tree::currentTree->addFor($4,$6,$8,$10), $$); Tree::currentTree->popScope(); }
	;

createscope:
	/* empty */				{ Tree::currentTree->pushScope(); }
	;

/* Constants */

constant:
	INTCONST				{ $$ = Tree::currentTree->addConstant(NuVTypes::IntegerData, $1); }
	| REALCONST				{ $$ = Tree::currentTree->addConstant(NuVTypes::RealData, $1); }
	| CHARCONST				{ $$ = Tree::currentTree->addConstant(NuVTypes::CharacterData, $1); }
	;

/* Variable declaration  name / assignment list */

assign:
	/* empty */				{ newVarName = *yylval.name; Tree::currentTree->setDeclarationAssignment(TRUE); }
	;

noassign:
	/* empty */				{ Tree::currentTree->setDeclarationAssignment(FALSE); }
	;

newname:
	NEWTOKEN					{ $$ = Tree::currentTree->addVariable($1); }
	| NEWTOKEN assign '[' expr ']' noassign		{ $$ = Tree::currentTree->addArrayVariable(&newVarName,$4); }
	| NEWTOKEN assign '=' expr noassign		{ $$ = Tree::currentTree->addVariable(&newVarName,$4); }
	| NEWTOKEN assign '[' expr ']' '=' expr noassign	{ $$ = Tree::currentTree->addArrayVariable(&newVarName,$4,$7); }
	;
	
namelist:
	newname					{ $$ = $1; }
	| namelist ',' newname			{ $$ = Tree::joinArguments($3,$1); }
	| constant				{ msg.print("Error: Constant value found in declaration.\n"); YYERROR; }
	| namelist newname			{ msg.print("Error: Missing comma between declarations?\n"); YYERROR; }
	;

/* Variables / Paths */

step:
	STEPTOKEN				{ if (!Tree::currentTree->expandPath($1)) YYERROR; }
	| STEPTOKEN '[' expr ']'		{ if (!Tree::currentTree->expandPath($1, $3)) YYERROR; }
;

steplist:
	step 					{  }
	| steplist '.' step			{  }
	;

var:
	VARNAME '[' expr ']'			{ $$ = Tree::currentTree->wrapVariable($1,$3); if ($$ == NULL) YYERROR; }
	| VARNAME				{ $$ = Tree::currentTree->wrapVariable($1); if ($$ == NULL) YYERROR; }
	| var '.' 				{ printf("HEllO pathvar.\n"); $$ = Tree::currentTree->createPath($1); }
		steplist			{ $$ = Tree::currentTree->finalisePath(); }
	;

/* Expressions */

exprlist:
	expr					{ $$ = $1; }
	| exprlist ',' expr			{ $$ = Tree::joinArguments($3,$1); }
	;

expr:
	constant				{ $$ = $1; }
	| func					{ $$ = $1; }
/*	| pathvar '.' NUMSTEP			{ Tree::currentTree->expandPath($3); $$ = Tree::currentTree->finalisePath(); }  */
/*	| pathvar '.' steplist '.' NUMSTEP	{ Tree::currentTree->expandPath($5); $$ = Tree::currentTree->finalisePath(); }  */
	| var '=' expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignment,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var PEQ expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentPlus,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var MEQ expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentMinus,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var TEQ expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentMultiply,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var DEQ expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAssignmentDivide,1,$1,$3); if ($$ == NULL) YYERROR; }
	| var					{ $$ = $1; }
	| '-' expr %prec UMINUS			{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorNegate,1, $2); if ($$ == NULL) YYERROR; }
	| expr '+' expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorAdd, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '-' expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorSubtract, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '*' expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorMultiply, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '/' expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorDivide, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '^' expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorPower, 0, $1, $3); if ($$ == NULL) YYERROR; }
	| expr EQ expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr NEQ expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorNotEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '>' expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorGreaterThan, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr GEQ expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorGreaterThanEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr '<' expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorLessThan, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| expr LEQ expr				{ $$ = Tree::currentTree->addOperator(NuCommand::OperatorLessThanEqualTo, 99, $1, $3); if ($$ == NULL) YYERROR; }
	| '(' expr ')'				{ $$ = $2; }
	| NEWTOKEN				{ msg.print("Error: '%s' has not been declared.\n", yylval.name->get()); YYERROR; }
	;


/* 3-Vector Constant / Assignment Group */
VECCONST:
	'#' expr ',' expr ',' expr '#'		{ $$ = Tree::currentTree->addVecConstant(NuVTypes::VectorData, $2, $4, $6); }
	;

/* Function */

func:
	FUNCCALL '(' ')'			{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,NULL); if ($$ == NULL) YYERROR; }
	| FUNCCALL	'(' exprlist ')' 	{ $$ = Tree::currentTree->addFunctionLeaf( (NuCommand::Function) $1,$3); if ($$ == NULL) YYERROR; }
	;

%%

void yyerror(char *s)
{
//    fprintf(stdout, "%s\n", s);
}
