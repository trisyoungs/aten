/* -------- */
/* Prologue */
/* -------- */
%{

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "command/commands.h"
#include "parser/parser.h"
#include "parser/tree.h"

/* Prototypes */
int yylex(void);
void yyerror(char *s);

/* Local Variables */
Dnchar tokenName;
Dnchar stepName;
Dnchar varName;
VTypes::DataType declaredType;

%}

/* Type Definition */
%union {
	int functionId;			/* Function enum id */
	Dnchar *name;			/* character pointer for names */
	TreeNode *node;			/* node pointer */
	Variable *variable;		/* variable pointer */
	Tree *functree;			/* user-defined function (tree) pointer */
	VTypes::DataType vtype;		/* variable type for next declarations */
};

%token <name> NEWTOKEN INTCONST REALCONST CHARCONST STEPTOKEN
%token <variable> VAR LOCALVAR
%token <functionId> FUNCCALL
%token <functree> USERFUNCCALL
%token <vtype> VARTYPE
%token DO WHILE FOR IF RETURN FILTERBLOCK
%nonassoc ELSE

%nonassoc AND OR
%left '=' PEQ MEQ TEQ DEQ 
%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/'
%right UMINUS
%left PP MM
%right '!'
%right '^'

%type <node> constant expr rawexpr func var rawvar
%type <node> flowstatement stexpr statement block blockment statementlist exprlist VECCONST
%type <node> namelist newname
%type <name> newvar
%type <node> filter pushscope declaration userfunc userfuncdef

%%

programlist:
	program						{ }
	| programlist program				{ }
	;

program:
	statementlist					{ if (!cmdparser.addStatement($1)) YYABORT; }
	| filter					{ }
	;

/* Compound Statement */

block:
	'{' pushscope statementlist '}' popscope	{ $$ = cmdparser.joinCommands($2,$3); }
	| '{' '}'					{ $$ = cmdparser.addFunction(Command::NoFunction); }
        ;

pushscope:
	/* empty */					{ $$ = cmdparser.pushScope(); if ($$ == NULL) YYABORT; }
	;

popscope:
	/* empty */					{ if (!cmdparser.popScope()) YYABORT; }
	;

statementlist:
	statement					{ $$ = $1; }
        | statementlist statement 			{ $$ = cmdparser.joinCommands($1, $2); }
        | statementlist block	 			{ $$ = cmdparser.joinCommands($1, $2); }
        ;

blockment:
	statement					{ $$ = $1; }
	| block						{ $$ = $1; }
	;

/* Filter Definitions */

optlist:
	NEWTOKEN savetokenname '=' constant		{ if (!cmdparser.setFilterOption(&tokenName, $4)) YYABORT; }
	| optlist ',' NEWTOKEN savetokenname '=' constant { if (!cmdparser.setFilterOption(&tokenName, $6)) YYABORT; }
	;

filter:
	FILTERBLOCK pushfilter '(' optlist ')' block 	{ if (!cmdparser.addStatement($6)) YYABORT; cmdparser.popTree(); }
	| FILTERBLOCK error				{ msg.print("Error reading filter block definition.\n"); YYABORT; }
	;

pushfilter:
	/* empty */					{ cmdparser.pushTree(TRUE); }
	;

/* Single Statement / Flow Control */

statement:
	';'						{ $$ = cmdparser.addFunction(Command::NoFunction); }
	| stexpr ';'					{ $$ = $1; }
	| flowstatement					{ $$ = $1; if ($$ == NULL) YYABORT; }
	| userfuncdef					{ $$ = $1; }
	;

stexpr:
	declaration					{ $$ = $1;  }
	| expr						{ $$ = $1; }
	;

flowstatement:
	IF '(' expr ')' blockment ELSE blockment	{ $$ = cmdparser.addFunction(Command::If,$3,$5,$7); }
	| IF '(' expr ')' blockment			{ $$ = cmdparser.addFunction(Command::If,$3,$5); }
	| FOR pushscope '(' stexpr ';' stexpr ';' stexpr ')' blockment	{ $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::For, $4,$6,$8,$10)); cmdparser.popScope(); }
	| WHILE pushscope '(' expr ')' blockment	{ $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::While, $4,$6)); cmdparser.popScope(); }
	| DO pushscope blockment WHILE '(' expr ')'	{ $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::DoWhile, $3,$6)); cmdparser.popScope(); }
	| RETURN expr ';'				{ $$ = cmdparser.addFunction(Command::Return,$2); }
	;

/* Range (X~Y) */

range:
	expr '~' expr					{ printf("GENERATE RANGE. TGAY\n"); }
	;

/* Constants */

constant:
	INTCONST					{ $$ = cmdparser.addConstant(VTypes::IntegerData, $1); }
	| REALCONST					{ $$ = cmdparser.addConstant(VTypes::DoubleData, $1); }
	| CHARCONST					{ $$ = cmdparser.addConstant(VTypes::StringData, $1); }
	;


/* User-defined function */

userfuncdef:
	VARTYPE savetype NEWTOKEN '(' pushfunc ')' block { if (!cmdparser.addStatement($7)) YYABORT; $$ = cmdparser.addFunction(Command::NoFunction); cmdparser.popTree(); declaredType = VTypes::NoData; }
	;

pushfunc:
	/* empty */					{ cmdparser.pushFunction(yylval.name->get(), declaredType); }
	;

/* Variable declaration and name / assignment list */

namelist:
	newname						{ $$ = $1; }
	| namelist ',' newname				{ $$ = Tree::joinArguments($3,$1); }
	| namelist ',' constant				{ msg.print("Error: Constant value found in declaration.\n"); YYABORT; }
	| namelist newname				{ msg.print("Error: Missing comma between declarations?\n"); YYABORT; }
	| namelist error				{ YYABORT; }
	;

newname:
	newvar '[' expr ']' 				{ $$ = cmdparser.addArrayVariable(declaredType, &tokenName,$3); }
	| newvar '=' expr 				{ $$ = cmdparser.addVariable(declaredType, &tokenName,$3); }
	| newvar '[' expr ']' '=' expr			{ $$ = cmdparser.addArrayVariable(declaredType, &tokenName,$3,$6); }
	| newvar					{ $$ = cmdparser.addVariable(declaredType, $1); }
	| VAR savevarname 				{ $$ = cmdparser.addVariable(declaredType, &varName); }
	| FUNCCALL					{ msg.print("Error: Existing function name cannot be redeclared as a variable.\n"); YYABORT; }
	| LOCALVAR					{ msg.print("Error: Existing variable in local scope cannot be redeclared.\n"); YYABORT; }
	| USERFUNCCALL					{ msg.print("Error: Existing user-defined function name cannot be redeclared.\n"); YYABORT; }
	| VARTYPE					{ msg.print("Error: Type-name used in variable declaration.\n"); YYABORT; }
	;

newvar:
	NEWTOKEN savetokenname				{ if (declaredType == VTypes::NoData) { msg.print("Token '%s' is undeclared.\n", tokenName.get()); YYABORT; } $$ = $1; }
	;


declaration:
	VARTYPE savetype namelist			{ $$ = cmdparser.addDeclarations($3); declaredType = VTypes::NoData; }
	| VARTYPE savetype error			{ msg.print("Illegal use of reserved word '%s'.\n", VTypes::dataType(declaredType)); YYABORT; }
	;

/* Variables / Paths */

step:
	STEPTOKEN savestepname '[' expr ']'		{ if (!cmdparser.expandPath(&stepName, $4)) YYABORT; }
	| STEPTOKEN savestepname '[' constant ']'	{ if (!cmdparser.expandPath(&stepName, $4)) YYABORT; }
	| STEPTOKEN					{ if (!cmdparser.expandPath($1)) YYABORT; }
	;

steplist:
	step 						{ }
	| steplist '.' step				{ }
	| steplist error				{ msg.print("Error formulating path.\n"); YYABORT; }
	;

var:
	rawvar						{ $$ = $1; if ($$ == NULL) YYABORT; }
	;

rawvar:
	VAR '[' expr ']'				{ $$ = cmdparser.wrapVariable($1,$3); }
	| VAR						{ $$ = cmdparser.wrapVariable($1); }
	| LOCALVAR '[' expr ']'				{ $$ = cmdparser.wrapVariable($1,$3); }
	| LOCALVAR					{ $$ = cmdparser.wrapVariable($1); }
	| rawvar '.' 					{ $$ = cmdparser.createPath($1); }
		steplist				{ $$ = cmdparser.finalisePath(); }
	| rawvar '('					{ msg.print("Can't use a variable as a function. Did you mean '[' instead?\n"); $$ = NULL; }
	;

/* Expressions */

exprlist:
	expr						{ $$ = $1; if ($$ == NULL) YYABORT; }
	| exprlist ',' expr				{ $$ = Tree::joinArguments($3,$1); }
	| exprlist expr					{ msg.print("Error: Missing comma between items?\n"); YYABORT; }
	;

expr:
	rawexpr						{ $$ = $1; if ($$ == NULL) YYABORT; }
	;

rawexpr:
	constant					{ $$ = $1; }
	| func						{ $$ = $1; }
	| userfunc					{ $$ = $1; }
	| var '=' expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignment,$1,$3); }
	| var '=' error					{ msg.print("Mangled expression used in assignment.\n"); YYABORT; }
	| var PEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentPlus,$1,$3); }
	| var MEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentMinus,$1,$3); }
	| var TEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentMultiply,$1,$3); }
	| var DEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentDivide,$1,$3); }
	| var						{ $$ = $1; }
	| '-' expr %prec UMINUS				{ $$ = cmdparser.addOperator(Command::OperatorNegate, $2); }
	| var PP					{ $$ = cmdparser.addOperator(Command::OperatorPostfixIncrease, $1);  }
	| var MM					{ $$ = cmdparser.addOperator(Command::OperatorPostfixDecrease, $1); }
	| PP var					{ $$ = cmdparser.addOperator(Command::OperatorPrefixIncrease, $2); }
	| MM var					{ $$ = cmdparser.addOperator(Command::OperatorPrefixDecrease, $2); }
	| PP error					{ msg.print("Prefix increment can only act on a variable value.\n"); YYABORT; }
	| MM error					{ msg.print("Prefix decrement can only act on a variable value.\n"); YYABORT; }
	| error PP					{ msg.print("Postfix increment can only act on a variable value.\n"); YYABORT; }
	| error MM					{ msg.print("Postfix decrement can only act on a variable value.\n"); YYABORT; }
	| expr '+' expr					{ $$ = cmdparser.addOperator(Command::OperatorAdd, $1, $3); }
	| expr '-' expr					{ $$ = cmdparser.addOperator(Command::OperatorSubtract, $1, $3); }
	| expr '*' expr					{ $$ = cmdparser.addOperator(Command::OperatorMultiply, $1, $3); }
	| expr '/' expr					{ $$ = cmdparser.addOperator(Command::OperatorDivide, $1, $3); }
	| expr '^' expr					{ $$ = cmdparser.addOperator(Command::OperatorPower, $1, $3); }
	| expr EQ expr					{ $$ = cmdparser.addOperator(Command::OperatorEqualTo, $1, $3); }
	| expr NEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorNotEqualTo, $1, $3); }
	| expr '>' expr					{ $$ = cmdparser.addOperator(Command::OperatorGreaterThan, $1, $3); }
	| expr GEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorGreaterThanEqualTo, $1, $3); }
	| expr '<' expr					{ $$ = cmdparser.addOperator(Command::OperatorLessThan, $1, $3); }
	| expr LEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorLessThanEqualTo, $1, $3); }
	| expr AND expr					{ $$ = cmdparser.addOperator(Command::OperatorAnd, $1, $3); }
	| expr OR expr					{ $$ = cmdparser.addOperator(Command::OperatorOr, $1, $3); }
	| '(' expr ')'					{ $$ = $2; }
	| '!' expr					{ $$ = cmdparser.addOperator(Command::OperatorNot, $2); }
	| NEWTOKEN					{ msg.print("Error: '%s' has not been declared as a function or a variable.\n", yylval.name->get()); YYABORT; }
	;


/* 3-Vector Constant / Assignment Group */
VECCONST:
	'#' expr ',' expr ',' expr '#'			{ $$ = cmdparser.addVecConstant(VTypes::VectorData, $2, $4, $6); }
	;

/* Functions */

func:
	FUNCCALL '(' ')'				{ $$ = cmdparser.addFunction( (Command::Function) $1); }
	| FUNCCALL '(' exprlist ')' 			{ $$ = cmdparser.addFunctionWithArglist( (Command::Function) $1,$3); }
	| FUNCCALL					{ $$ = cmdparser.addFunction( (Command::Function) $1); }
	;

userfunc:
	USERFUNCCALL '(' exprlist ')' 			{ $$ = cmdparser.addUserFunction($1,$3); }
	| USERFUNCCALL '(' ')'				{ $$ = cmdparser.addUserFunction($1); }
	| USERFUNCCALL					{ $$ = cmdparser.addUserFunction($1); }
	;

/* Semantic Value Subroutines */

savetokenname:
	/* empty */					{ tokenName = *yylval.name; }
	;

savetype:
	/* empty */					{ declaredType = yylval.vtype; }
	;

savevarname:
	/* empty */					{ varName = yylval.variable->name(); }
	;

savestepname:
	/* empty */					{ stepName = *yylval.name; }
	;

%%

void yyerror(char *s)
{
	printf("Deleting current tree.\n");
//	cmdparser.deleteCurrentTree();
//    fprintf(stdout, "%s\n", s);
}
