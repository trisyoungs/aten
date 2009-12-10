/* -------- */
/* Prologue */
/* -------- */
%{

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "classes/neta.h"
#include "classes/neta_parser.h"

/* Prototypes */
int NetaParser_lex(void);
void NetaParser_error(char *s);

/* Local Variables */
Neta::NetaValue savedval;

%}

// Redeclare function names
%name-prefix="NetaParser_"

/* Type Definition */
%union
{
	NetaNode *netanode;			/* Generic node pointer */
	NetaBoundNode *boundnode;		/* Bound node pointer */
	NetaRingNode *ringnode;			/* Ring node pointer */
	NetaChainNode *chainnode;		/* Chain node pointer */
	int intconst;				/* Integer number */
	Neta::NetaKeyword netakey;		/* NETA keyword ID */
	Atom::AtomGeometry atomgeom;		/* NETA geometry ID */
	Neta::NetaValue netaval;		/* NETA value ID */
	Refitem<ForcefieldAtom,int> *typelist;	/* Pointer to head of created element/type list */
};

%token IF

%left AND OR
%left '='
%left GEQ LEQ EQ NEQ '>' '<'

%token <intconst> INTCONST ELEMENT TYPEREF
%token <netakey> NETAKEY
%token <netaval> NETAVAL
%token NETARING NETACHAIN
%token <atomgeom> NETAGEOM
%token TOKEN
%type <netanode> node nodelist keyword value expander bound chain
%type <ringnode> pushctxtr
%type <chainnode> pushctxtc
%type <boundnode> pushctxtb
%type <typelist> elemtype elemtypes elemtypelist
%%

neta:
	/* empty */					{ netaparser.setDescription(NULL); YYACCEPT; }
	| nodelist					{ netaparser.setDescription($1); YYACCEPT; }
	;

nodelist:
	node						{ $$ = $1; }
	| nodelist ',' node				{ $$ = netaparser.join(Neta::NetaAndLogic, $1, $3); }
	| nodelist '|' node				{ $$ = netaparser.join(Neta::NetaOrLogic, $1, $3); }
	| '(' nodelist ')'				{ $$ = $2; }
	;

node:
	'!' keyword					{ $2->setReverseLogic(); $$ = $2; }
	| '!' value					{ $2->setReverseLogic(); $$ = $2; }
	| '!' expander					{ $2->setReverseLogic(); $$ = $2; }
	| '!' bound					{ $2->setReverseLogic(); $$ = $2; }
	| keyword					{ $$ = $1; }
	| value						{ $$ = $1; }
	| expander					{ $$ = $1; }
	| bound						{ $$ = $1; }
	| '$' TOKEN					{ $$ = netaparser.findDefine(netaparser.lastUnknownToken()); if ($$ == NULL) YYABORT; }
	;

/* Keywords : NETA statements that are simple, individual words with no arguments, and that cannot be expanded */
keyword:
	NETAKEY						{ $$ = netaparser.createKeywordNode($1); }
	| NETAGEOM					{ $$ = netaparser.createGeometryNode($1); }
	;

/* Values : NETA statements that require a comparison operator and a value, and cannot be expanded */
value:
	NETAVAL saveval '=' INTCONST			{ $$ = netaparser.createValueNode(savedval, Neta::EqualTo, $4); }
	| NETAVAL saveval '>' INTCONST			{ $$ = netaparser.createValueNode(savedval, Neta::GreaterThan, $4); }
	| NETAVAL saveval '<' INTCONST			{ $$ = netaparser.createValueNode(savedval, Neta::LessThan, $4); }
	| NETAVAL saveval GEQ INTCONST			{ $$ = netaparser.createValueNode(savedval, Neta::GreaterThanEqualTo, $4); }
	| NETAVAL saveval LEQ INTCONST			{ $$ = netaparser.createValueNode(savedval, Neta::LessThanEqualTo, $4); }
	| NETAVAL saveval NEQ INTCONST			{ $$ = netaparser.createValueNode(savedval, Neta::NotEqualTo, $4); }
	;

/* Expanders : NETA statements that may optionally take a bracketed expansion */
expander:
	NETARING					{ $$ = netaparser.createRingNode(); netaparser.popContext(); }
	| NETACHAIN					{ $$ = netaparser.createChainNode(); netaparser.popContext(); }
	| NETARING '(' pushctxtr nodelist ')'		{ $3->setInnerNeta($4); $$ = $3; netaparser.popContext(); }
	| NETACHAIN '(' pushctxtc chain ',' nodelist ')'{ $3->setInnerNeta($6,$4); $$ = $3; netaparser.popContext(); }
	| NETACHAIN '(' pushctxtc chain ')'		{ $3->setInnerNeta(NULL,$4); $$ = $3; netaparser.popContext(); }
	;

chain:
	bound						{ $$ = $1; }
	| '!' bound					{ $$ = $2; $2->setReverseLogic(); }
	| chain bound					{ $$ = netaparser.link($1,$2); }
	| chain '!' bound				{ $$ = netaparser.link($1,$3); $3->setReverseLogic(); }
	;

bound:
	'~' elemtypelist pushctxtb			{ $3->set($2, NULL, Bond::Any); $$ = $3; netaparser.popContext(); }
	| '-' elemtypelist pushctxtb			{ $3->set($2, NULL, Bond::Single); $$ = $3; netaparser.popContext(); }
	| '=' elemtypelist pushctxtb			{ $3->set($2, NULL, Bond::Double); $$ = $3; netaparser.popContext(); }
	| '~' elemtypelist '(' pushctxtb nodelist ')'	{ $4->set($2, $5, Bond::Any); $$ = $4; netaparser.popContext(); }
	| '-' elemtypelist '(' pushctxtb nodelist ')'	{ $4->set($2, $5, Bond::Single); $$ = $4; netaparser.popContext(); }
	| '=' elemtypelist '(' pushctxtb nodelist ')'	{ $4->set($2, $5, Bond::Double); $$ = $4; netaparser.popContext(); }
	;

/* Element/type list */
elemtypelist:
	elemtype					{ $$ = $1; }
	| '[' elemtypes ']'				{ $$ = $2; }
	;

elemtypes:
	elemtype					{ $$ = $1; }
	| elemtypes ',' elemtype			{ $$ = netaparser.joinElementTypes($1,$3); }
	;

elemtype:
	ELEMENT						{ $$ = netaparser.createElementType($1); if ($$ == NULL) YYABORT; }
	| '&' INTCONST 					{ $$ = netaparser.createElementType(-$2); if ($$ == NULL) YYABORT; }
	;

/* Value Management */
saveval:
	/* empty */					{ savedval = yylval.netaval; }
	;

/* Context creation */
pushctxtr:
	/* empty */					{ $$ = netaparser.createRingNode(); }
	;

pushctxtc:
	/* empty */					{ $$ = netaparser.createChainNode(); }
	;

pushctxtb:
	/* empty */					{ $$ = netaparser.createBoundNode(); }
	;

%%

void yyerror(char *s)
{
}
