/* -------- */
/* Prologue */
/* -------- */
%{

/* Includes */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "base/neta.h"
#include "base/neta_parser.h"

/* Prototypes */
int NetaParser_lex(void);
void NetaParser_error(char const *s);

ATEN_USING_NAMESPACE

/* Local Variables */
Neta::NetaValue savedVal;

%}

// Redeclare function names
%name-prefix "NetaParser_"

/* Type Definition */
%union
{
	NetaNode* netaNode;				/* Generic node pointer */
	NetaBoundNode* boundNode;			/* Bound node pointer */
	NetaRingNode* ringNode;				/* Ring node pointer */
	NetaChainNode* chainNode;			/* Chain node pointer */
	NetaMeasurementNode* measureNode;		/* Measurement node pointer */
	int intConst;					/* Integer number */
	double doubleConst;				/* Floating point number */
	Neta::NetaKeyword netaKey;			/* NETA keyword ID */
	Atom::AtomGeometry atomGeom;			/* NETA geometry ID */
	Neta::NetaValue netaVal;			/* NETA value ID */
	RefListItem<ForcefieldAtom,int>* typeList;	/* Pointer to head of created element/type list */
};

%token IF

%left AND OR
%left '='
%left GEQ LEQ EQ NEQ '>' '<'

%token <intConst> INTCONST ELEMENT TYPEREF
%token <doubleConst> DOUBLECONST
%token <netaKey> NETAKEY
%token <netaVal> NETAVAL
%token <netaRepeat> NETAREPEAT
%token NETARING NETACHAIN NETAGEOMETRY NETAPATH
%token <atomGeom> NETAGEOMETRYTYPE
%token TOKEN
%type <netaNode> node nodelist keyword value repeat expander bound chain
%type <ringNode> pushctxtr
%type <chainNode> pushctxtc
%type <measureNode> pushctxtg pushctxtp
%type <boundNode> pushctxtb
%type <typeList> elemtype elemtypes elemtypelist
%%

neta:
	/* empty */					{ NetaParser::setDescription(NULL); YYACCEPT; }
	| nodelist					{ NetaParser::setDescription($1); YYACCEPT; }
	;

nodelist:
	node						{ $$ = $1; }
	| nodelist ',' node				{ $$ = NetaParser::join(Neta::NetaAndLogic, $1, $3); }
	| nodelist '|' nodelist				{ $$ = NetaParser::join(Neta::NetaOrLogic, $1, $3); }
	| '(' nodelist ')'				{ $$ = $2; }
	;

node:
	'!' keyword					{ $2->setReverseLogic(); $$ = $2; }
	| '!' value					{ $2->setReverseLogic(); $$ = $2; }
	| '!' repeat					{ $2->setReverseLogic(); $$ = $2; }
	| '!' expander					{ $2->setReverseLogic(); $$ = $2; }
	| '!' bound					{ $2->setReverseLogic(); $$ = $2; }
	| keyword					{ $$ = $1; }
	| value						{ $$ = $1; }
	| repeat					{ $$ = $1; }
	| expander					{ $$ = $1; }
	| bound						{ $$ = $1; }
	| '$' TOKEN					{
		$$ = NetaParser::findDefine(NetaParser::lastUnknownToken());
		if ($$ == NULL)
		{
			if (!NetaParser::quiet()) Messenger::print("Error: NETA description references a non-existent 'define' name (%s)\n", qPrintable(NetaParser::lastUnknownToken())); YYABORT; }
		}
	| TOKEN						{
		if (!NetaParser::quiet()) Messenger::print("Error: NETA description contains an unrecognised keyword (%s)\n", qPrintable(NetaParser::lastUnknownToken()));
		YYABORT;
		}
	| INTCONST					{ if (!NetaParser::quiet()) Messenger::print("Error: Stray integer constant found in NETA description.\n"); YYABORT; }
	;

/* Keywords : NETA statements that are simple, individual words with no arguments, and that cannot be expanded */
keyword:
	NETAKEY						{ $$ = NetaParser::createKeywordNode($1); }
	| NETAGEOMETRYTYPE				{ $$ = NetaParser::createGeometryNode($1); }
	;

/* Values : NETA statements that require a comparison operator and a value, and cannot be expanded */
value:
	NETAVAL saveval '=' '=' INTCONST		{ $$ = NetaParser::createValueNode(savedVal, Neta::EqualTo, $5); }
	| NETAVAL saveval '=' INTCONST			{ $$ = NetaParser::createValueNode(savedVal, Neta::EqualTo, $4); }
	| NETAVAL saveval '>' INTCONST			{ $$ = NetaParser::createValueNode(savedVal, Neta::GreaterThan, $4); }
	| NETAVAL saveval '<' INTCONST			{ $$ = NetaParser::createValueNode(savedVal, Neta::LessThan, $4); }
	| NETAVAL saveval GEQ INTCONST			{ $$ = NetaParser::createValueNode(savedVal, Neta::GreaterThanEqualTo, $4); }
	| NETAVAL saveval LEQ INTCONST			{ $$ = NetaParser::createValueNode(savedVal, Neta::LessThanEqualTo, $4); }
	| NETAVAL saveval NEQ INTCONST			{ $$ = NetaParser::createValueNode(savedVal, Neta::NotEqualTo, $4); }
	;

/* Values: Special case for repeat (which we use exclusively elsewhere in 'chain' */
repeat:
	NETAREPEAT saveval '=' '=' INTCONST		{ $$ = NetaParser::createValueNode(savedVal, Neta::EqualTo, $5); }
	| NETAREPEAT saveval '=' INTCONST		{ $$ = NetaParser::createValueNode(savedVal, Neta::EqualTo, $4); }
	| NETAREPEAT saveval '>' INTCONST		{ $$ = NetaParser::createValueNode(savedVal, Neta::GreaterThan, $4); }
	| NETAREPEAT saveval '<' INTCONST		{ $$ = NetaParser::createValueNode(savedVal, Neta::LessThan, $4); }
	| NETAREPEAT saveval GEQ INTCONST		{ $$ = NetaParser::createValueNode(savedVal, Neta::GreaterThanEqualTo, $4); }
	| NETAREPEAT saveval LEQ INTCONST		{ $$ = NetaParser::createValueNode(savedVal, Neta::LessThanEqualTo, $4); }
	| NETAREPEAT saveval NEQ INTCONST		{ $$ = NetaParser::createValueNode(savedVal, Neta::NotEqualTo, $4); }
	;

/* Expanders : NETA statements that may optionally take a bracketed expansion */
expander:
	NETARING					{ $$ = NetaParser::createRingNode(); NetaParser::popContext(); }
	| NETARING '(' pushctxtr nodelist ')'		{ $3->setInnerNeta($4); $$ = $3; NetaParser::popContext(); }
	| NETACHAIN '(' pushctxtc chain ')'		{ if (!NetaParser::quiet()) Messenger::error("NETA Error: 'chain' keyword must have repeat specifier as last argument."); $$ = NULL; }
	| NETACHAIN '(' pushctxtc chain ',' repeat ')'	{ $3->setInnerNeta(NULL,$4); $$ = $3; NetaParser::popContext(); }
	| NETAGEOMETRY '(' pushctxtg DOUBLECONST ',' DOUBLECONST ',' chain ')' { $3->setInnerNeta(NULL,$8); $3->setRequiredValue($4,$6); $$ = $3; NetaParser::popContext(); }
	| NETAPATH '(' pushctxtp DOUBLECONST ',' DOUBLECONST ',' chain ')' { $3->setInnerNeta(NULL,$8); $3->setRequiredValue($4,$6); $$ = $3; NetaParser::popContext(); }
	;

chain:
	bound						{ $$ = $1; }
	| '!' bound					{ $$ = $2; $2->setReverseLogic(); }
	| chain ',' bound				{ $$ = NetaParser::link($1,$3); }
	| chain ',' '!' bound				{ $4->setReverseLogic(); $$ = NetaParser::link($1,$4); }
	;

bound:
	'~' elemtypelist pushctxtb			{ $3->set($2, NULL, Bond::Any); $$ = $3; NetaParser::popContext(); }
	| '-' elemtypelist pushctxtb			{ $3->set($2, NULL, Bond::Single); $$ = $3; NetaParser::popContext(); }
	| '=' elemtypelist pushctxtb			{ $3->set($2, NULL, Bond::Double); $$ = $3; NetaParser::popContext(); }
	| '~' elemtypelist '(' pushctxtb nodelist ')'	{ $4->set($2, $5, Bond::Any); $$ = $4; NetaParser::popContext(); }
	| '-' elemtypelist '(' pushctxtb nodelist ')'	{ $4->set($2, $5, Bond::Single); $$ = $4; NetaParser::popContext(); }
	| '=' elemtypelist '(' pushctxtb nodelist ')'	{ $4->set($2, $5, Bond::Double); $$ = $4; NetaParser::popContext(); }
	;

/* Element/type list */
elemtypelist:
	elemtype					{ $$ = $1; }
	| '[' elemtypes ']'				{ $$ = $2; }
	;

elemtypes:
	elemtype					{ $$ = $1; }
	| elemtypes ',' elemtype			{ $$ = NetaParser::joinElementTypes($1,$3); }
	;

elemtype:
	ELEMENT						{ $$ = NetaParser::createElementType($1); if ($$ == NULL) YYABORT; }
	| '&' INTCONST 					{ $$ = NetaParser::createElementType(-$2); if ($$ == NULL) YYABORT; }
	;

/* Value Management */
saveval:
	/* empty */					{ savedVal = yylval.netaVal; }
	;

/* Context creation */
pushctxtr:
	/* empty */					{ $$ = NetaParser::createRingNode(); }
	;

pushctxtc:
	/* empty */					{ $$ = NetaParser::createChainNode(); }
	;

pushctxtg:
	/* empty */					{ $$ = NetaParser::createMeasurementNode(false); }
	;

pushctxtp:
	/* empty */					{ $$ = NetaParser::createMeasurementNode(true); }
	;

pushctxtb:
	/* empty */					{ $$ = NetaParser::createBoundNode(); }
	;

%%

void yyerror(char const *s)
{
}
