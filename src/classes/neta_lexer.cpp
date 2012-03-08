/*
	*** NETA Lexer
	*** src/parser/neta_lexer.cpp
	Copyright T. Youngs 2007-2012

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <string.h>
#include "base/sysfunc.h"
#include "base/elements.h"
#include "classes/neta.h"
#include "classes/neta_grammar.h"
#include "classes/neta_parser.h"

// Symbols
const char *NetaSymbolTokenKeywords[NetaParser::nNetaSymbolTokens] = { ">=", "<=", "!=", "<>" };
int NetaSymbolTokenValues[NetaParser::nNetaSymbolTokens] = { GEQ, LEQ, NEQ, NEQ };

// Bison-generated NetaParser_lex()
int NetaParser_lex()
{
	return netaparser.lex();
}

// Parser lexer, called by yylex()
int NetaParser::lex()
{
	if (neta_ == NULL)
	{
		printf("Lexer called when no target NETA structure set.\n");
		return 0;
	}

	int n;
	bool done;
	static Dnchar token;
	char c;
	static Dnchar name;
	token.clear();

	// Skip over whitespace
	while ((c = getChar()) == ' ' || c == '\t' || c == '\r' || c == '\n' );

	if (c == 0) return 0;

	// Set this point as the start of our new token (for error reporting)
	tokenStart_ = stringPos_-1;

	/*
	// Integer number
	*/
	/*
	// Number Detection - Either '.' or  a digit begins a number
	*/
	if (isdigit (c))
	{
		token += c;
		done = FALSE;
		do
		{
			c = getChar();
			if (isdigit(c)) token += c;
			else
			{
				unGetChar();
				done = TRUE;
			}
		} while (!done);
		NetaParser_lval.intconst = atoi(token);
		msg.print(Messenger::Test, "NETA : found an integer constant [%s] [%i]\n", token.get(), NetaParser_lval.intconst);
		return INTCONST;
	}

	/*
	// Alphanumeric-token : Element symbol, NETA keyword, value, expander, or geometry
	*/
	if (isalpha (c))
	{
		do
		{
			token += c;
			c = getChar();
		}
		while (isalnum(c) || (c == '_'));
		unGetChar();
		msg.print(Messenger::Test, "NETA : found an alpha token [%s]...\n", token.get());

		// Element Symbol (or 'Any')
		if (token == "Any")
		{
			NetaParser_lval.intconst = 0;
			msg.print(Messenger::Test, "NETA : ...which is the any element symbol (Any)\n");
			return ELEMENT;
		}
		for (n=0; n<elements().nElements(); ++n) if (token == elements().symbol(n)) break;
		if (n < elements().nElements())
		{
			NetaParser_lval.intconst = n;
			msg.print(Messenger::Test, "NETA : ...which is a an element symbol (%i)\n",n);
			return ELEMENT;
		}

		// Is this a NETA keyword?
		Neta::NetaKeyword nk = Neta::netaKeyword(token, FALSE);
		if (nk != Neta::nNetaKeywords)
		{
			msg.print(Messenger::Test, "NETA : ...which is a keyword (->NETAKEY)\n");
			NetaParser_lval.netakey = nk;
			return NETAKEY;
		}

		// Is this a NETA geometry?
		Atom::AtomGeometry ag = Atom::atomGeometry(token, FALSE);
		if (ag != Atom::nAtomGeometries)
		{
			msg.print(Messenger::Test, "NETA : ...which is a geometry (->NETAGEOM)\n");
			NetaParser_lval.atomgeom = ag;
			return NETAGEOM;
		}

		// Is this a NETA value?
		Neta::NetaValue nv = Neta::netaValue(token, FALSE);
		if (nv != Neta::nNetaValues)
		{
			msg.print(Messenger::Test, "NETA : ...which is a value (->NETAVAL)\n");
			NetaParser_lval.netaval = nv;
			return NETAVAL;
		}

		// Is this a NETA expander?
		Neta::NetaExpander ne = Neta::netaExpander(token, FALSE);
		if (ne != Neta::nNetaExpanders)
		{
			msg.print(Messenger::Test, "NETA : ...which is an expander (->NETAEXP)\n");
			if (ne == Neta::RingExpander) return NETARING;
			else if (ne == Neta::ChainExpander) return NETACHAIN;
			return 0;
		}

		// Is it a bond type?
		Bond::BondType bt = Bond::bondType(token, FALSE);
		if (bt != Bond::nBondTypes)
		{
			msg.print(Messenger::Test, "NETA : ...which is a bond type (->INTCONST, %i)\n", bt);
			NetaParser_lval.intconst = bt;
			return INTCONST;
		}

		// If we get to here then we have found an unrecognised alphanumeric token
		msg.print(Messenger::Test, "NETA : ...which is unrecognised (->TOKEN)\n");
		lastUnknownToken_ = token;
		return TOKEN;
	}

	/* We have found a symbolic character (or a pair) that corresponds to an operator */
	// Return immediately in the case of brackets, commas etc.
	if ((c == '(') || (c == ')') || (c == ',') || (c == '[') || (c == ']') || (c == '-') || (c == '~') || (c == '&') || ( c == '!') || (c == '$') || (c == '=') || (c == '?') || (c == ':'))
	{
		msg.print(Messenger::Test, "NETA : found symbol [%c]\n",c);
		return c;
	}
	token += c;
	// Similarly, if the next character is a bracket or double quotes, return immediately
	char c2 = peekChar();
	if ((c2 == '(') || (c2 == ')') || (c2 == ';') || (c2 == '{') || (c2 == '}') || (c2 == '"')) return c;
	// If it is 'punctuation', add this second character to our operator and search for it
	if (ispunct(c2))
	{
		c = getChar();
		token += c;
		msg.print(Messenger::Test, "NETA : found symbol [%s]\n",token.get());
		NetaSymbolToken st = (NetaSymbolToken) enumSearch("", nNetaSymbolTokens, NetaSymbolTokenKeywords, token);
		if (st != nNetaSymbolTokens) return NetaSymbolTokenValues[st];
		else msg.print("Error: Unrecognised symbol found in input (%s).\n", token.get());
 	}
	else
	{
		// Make sure that this is a known symbol
		if ((c == '$') || (c == '%') || (c == '&') || (c == '@') || (c == '?') || (c == ':'))
		{
			msg.print("Error: Unrecognised symbol found in input (%c).\n", c);
		}
		else return c;
	}
	return 0;
}
