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
	int intconst;			/* integer constant value */
	double doubleconst;		/* double constant value */
};

%token <intconst> INTCONST ELEMENTCONST
%token <doubleconst> DOUBLECONST
%token <name> NEWTOKEN CHARCONST STEPTOKEN
%token <variable> VAR LOCALVAR
%token <functionId> FUNCCALL
%token <functree> USERFUNCCALL
%token <vtype> VARTYPE
%token DO WHILE FOR IF RETURN FILTERBLOCK HELP VOID DUMMY
%nonassoc ELSE

%nonassoc AND OR
%left '=' PEQ MEQ TEQ DEQ 
%left GEQ LEQ EQ NEQ '>' '<'
%left '+' '-'
%left '*' '/' '%'
%right UMINUS
%left PP MM
%right '!'
%right '^'

%type <node> constant expr rawexpr func var rawvar arg args
%type <node> fstatement decexpr statement block blockment statementlist exprlist ARRAYCONST
%type <node> namelist newname
%type <name> newvar
%type <node> filter pushscope declaration userfunc userfuncdef userstatementdef

%%

programlist:
	/* empty */					{ }
	| program					{ }
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
	blockment					{ $$ = $1; }
	| statementlist blockment 			{ $$ = cmdparser.joinCommands($1, $2); }
	;

blockment:
	statement ';'					{ $$ = $1; }
	| block						{ $$ = $1; }
	| fstatement					{ $$ = $1; if ($$ == NULL) YYABORT; }
	;

/* Filter Definitions */

optlist:
	NEWTOKEN savetokenname '=' constant		{ if (!cmdparser.setFilterOption(&tokenName, $4)) YYABORT; }
	| optlist ',' NEWTOKEN savetokenname '=' constant { if (!cmdparser.setFilterOption(&tokenName, $6)) YYABORT; }
	;

filter:
	FILTERBLOCK pushfilter '(' optlist ')' block 	{ if (!cmdparser.addStatement($6)) YYABORT; cmdparser.popTree(); }
	;

pushfilter:
	/* empty */					{ cmdparser.pushTree(TRUE); }
	;

/* Single Statement / Flow Control */

statement:
	decexpr						{ $$ = $1; }
	| HELP FUNCCALL					{ $$ = cmdparser.addFunction(Command::Help, cmdparser.addConstant($2)); }
	| RETURN expr					{ $$ = cmdparser.addFunction(Command::Return,$2); }
	| RETURN 					{ $$ = cmdparser.addFunction(Command::Return); }
	| statement decexpr				{ msg.print("Error: Expected ';' before current expression.\n"); YYABORT; }
	;

decexpr:
	declaration					{ $$ = $1; }
	| expr						{ $$ = $1; }
	;

fstatement:
	IF '(' expr ')' blockment ELSE blockment	{ $$ = cmdparser.addFunction(Command::If,$3,$5,$7); }
	| IF '(' expr ')' blockment			{ $$ = cmdparser.addFunction(Command::If,$3,$5); }
	| FOR pushscope '(' decexpr ';' expr ';' expr ')' blockment { $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::For, $4,$6,$8,$10)); cmdparser.popScope(); }
	| WHILE pushscope '(' expr ')' blockment	{ $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::While, $4,$6)); cmdparser.popScope(); }
	| DO pushscope blockment WHILE '(' expr ')'	{ $$ = cmdparser.joinCommands($2, cmdparser.addFunction(Command::DoWhile, $3,$6)); cmdparser.popScope(); }
	| userfuncdef					{ if (!cmdparser.addStatement($1)) YYABORT; }
	| userstatementdef				{ if (!cmdparser.addStatement($1)) YYABORT; }
	;

/* Constants */

constant:
	INTCONST					{ $$ = cmdparser.addConstant($1); }
	| DOUBLECONST					{ $$ = cmdparser.addConstant($1); }
	| CHARCONST					{ $$ = cmdparser.addConstant($1->get()); }
	| ELEMENTCONST					{ $$ = cmdparser.addElementConstant($1); }
	;


/* User-defined function */

userfuncdef:
	VARTYPE savetype NEWTOKEN '(' pushfunc args ')' block { if (!cmdparser.addStatement($8)) YYABORT; $$ = cmdparser.addFunction(Command::NoFunction); cmdparser.popTree(); declaredType = VTypes::NoData; }
	;

userstatementdef:
	VOID cleartype NEWTOKEN '(' pushfunc args ')' block { if (!cmdparser.addStatement($8)) YYABORT; $$ = cmdparser.addFunction(Command::NoFunction); cmdparser.popTree(); declaredType = VTypes::NoData; }
	;

args:
	/* empty */					{ }
	| arg						{ }
	| args ',' arg					{ }
	;

arg:
	VARTYPE savetype NEWTOKEN savetokenname		{ $$ = cmdparser.addVariableAsArgument(declaredType, &tokenName); }
	| VARTYPE savetype NEWTOKEN savetokenname '=' expr { $$ = cmdparser.addVariableAsArgument(declaredType, &tokenName, $6); }
	;

pushfunc:
	/* empty */					{ cmdparser.pushFunction(yylval.name->get(), declaredType); }
	;

/* Variable declaration and name / assignment list */

namelist:
	newname						{ $$ = $1; if ($1 == NULL) YYABORT; }
	| namelist ',' newname				{ if ($3 == NULL) YYABORT; $$ = Tree::joinArguments($3,$1); }
	| namelist ',' constant				{ msg.print("Error: Constant value found in declaration.\n"); YYABORT; }
	| namelist newname				{ msg.print("Error: Missing comma between declarations?\n"); YYABORT; }
	| namelist error				{ YYABORT; }
	| namelist ',' FUNCCALL				{ msg.print("Error: Existing function name cannot be redeclared as a variable.\n"); YYABORT; }
	| namelist ',' LOCALVAR				{ msg.print("Error: Existing variable in local scope cannot be redeclared.\n"); YYABORT; }
	| namelist ',' USERFUNCCALL			{ msg.print("Error: Existing user-defined function name cannot be redeclared.\n"); YYABORT; }
	| namelist ',' VARTYPE				{ msg.print("Error: Type-name used in variable declaration.\n"); YYABORT; }
	;

newname:
	newvar '[' expr ']' 				{ $$ = cmdparser.addArrayVariable(declaredType, &tokenName, $3); }
	| newvar '=' expr 				{ $$ = cmdparser.addVariable(declaredType, &tokenName, $3); }
	| newvar '=' ARRAYCONST				{ $$ = cmdparser.addVariable(declaredType, &tokenName, $3); }
	| newvar '[' expr ']' '=' expr			{ $$ = cmdparser.addArrayVariable(declaredType, &tokenName,$3,$6); }
	| newvar '[' expr ']' '=' ARRAYCONST		{ $$ = cmdparser.addArrayVariable(declaredType, &tokenName,$3,$6); }
	| newvar					{ $$ = cmdparser.addVariable(declaredType, $1); }
	| VAR savevarname 				{ $$ = cmdparser.addVariable(declaredType, &varName); }
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
	| exprlist expr					{ msg.print("Error: Missing comma between items.\n"); YYABORT; }
	;

expr:
	rawexpr						{ $$ = $1; if ($$ == NULL) YYABORT; }
	;

rawexpr:
	constant					{ $$ = $1; }
	| func						{ $$ = $1; }
	| userfunc					{ $$ = $1; }
	| var '=' expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignment,$1,$3); }
	| var '=' ARRAYCONST				{ $$ = cmdparser.addOperator(Command::OperatorAssignment,$1,$3); }
	| var '=' error					{ msg.print("Mangled expression used in assignment.\n"); YYABORT; }
	| var PEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentPlus,$1,$3); }
	| var MEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentSubtract,$1,$3); }
	| var TEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentMultiply,$1,$3); }
	| var DEQ expr					{ $$ = cmdparser.addOperator(Command::OperatorAssignmentDivide,$1,$3); }
	| '-' expr %prec UMINUS				{ $$ = cmdparser.addOperator(Command::OperatorNegate, $2); }
	| var PP					{ $$ = cmdparser.addOperator(Command::OperatorPostfixIncrease, $1);  }
	| var MM					{ $$ = cmdparser.addOperator(Command::OperatorPostfixDecrease, $1); }
	| PP var					{ $$ = cmdparser.addOperator(Command::OperatorPrefixIncrease, $2); }
	| MM var					{ $$ = cmdparser.addOperator(Command::OperatorPrefixDecrease, $2); }
	| var						{ $$ = $1; }
	| expr '+' expr					{ $$ = cmdparser.addOperator(Command::OperatorAdd, $1, $3); }
	| expr '-' expr					{ $$ = cmdparser.addOperator(Command::OperatorSubtract, $1, $3); }
	| expr '*' expr					{ $$ = cmdparser.addOperator(Command::OperatorMultiply, $1, $3); }
	| expr '/' expr					{ $$ = cmdparser.addOperator(Command::OperatorDivide, $1, $3); }
	| expr '^' expr					{ $$ = cmdparser.addOperator(Command::OperatorPower, $1, $3); }
	| expr '%' expr					{ $$ = cmdparser.addOperator(Command::OperatorModulus, $1, $3); }
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


/* Array Vector Constant / Assignment Group */
ARRAYCONST:
	'{' exprlist '}'				{ $$ = cmdparser.addArrayConstant($2); }
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

cleartype:
	/* empty */					{ declaredType = VTypes::NoData; }
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
}
