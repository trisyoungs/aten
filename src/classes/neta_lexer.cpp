/*
	*** NETA Lexer
	*** src/parser/neta_lexer.cpp
	Copyright T. Youngs 2007-2015

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
#include "classes/neta_grammar.hh"
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
	// Number Detection - Either '-', '.' or a digit begins a number
	*/
	bool nextCharIsPossibleDigit = (isdigit(peekChar()) || (peekChar() == '.'));
	if ((c == '.') || isdigit(c) || ((c == '-') && nextCharIsPossibleDigit))
	{
		// Default to integer, unless first char is '.'
		bool integer = (c == '.' ? FALSE : TRUE);
		bool hasexp = FALSE;
		token += c;
		done = FALSE;
		do
		{
			c = getChar();
			if (isdigit(c)) token += c;
			else if (c == '.')
			{
				integer = FALSE;
				token += '.';
			}
			else if ((c == 'e') || (c == 'E'))
			{
				// Check for previous exponential in number
				if (hasexp)
				{
					msg.print("Error: Number has two exponentiations (e/E).\n");
					return 0;
				}
				token += 'E';
				hasexp = TRUE;
			}
			else if ((c == '-') || (c == '+'))
			{
				// We allow '-' or '+' only as part of an exponentiation, so if it is not preceeded by 'E' we stop parsing
				if ((!token.isEmpty()) && (token.lastChar() != 'E'))
				{
					unGetChar();
					done = TRUE;
				}
				else token += c;
			}
			else
			{
				unGetChar();
				done = TRUE;
			}
		} while (!done);
		// We now have the number as a text token...
		if (!hasexp)
		{
			if (integer) NetaParser_lval.intconst = atoi(token);
			else NetaParser_lval.doubleconst = atof(token);
		}
		else
		{
			// Exponentiations are always returned as a double
			integer = FALSE;
			NetaParser_lval.doubleconst = atof(beforeChar(token,'E')) * pow(10.0, atof(afterChar(token,'E')));
		}
		if (integer) msg.print(Messenger::Parse, "NETA : found an integer constant [%s] [%i]\n", token.get(), NetaParser_lval.intconst);
		else msg.print(Messenger::Parse, "NETA : found a floating-point constant [%s] [%e]\n", token.get(), NetaParser_lval.doubleconst);
		return (integer ? INTCONST : DOUBLECONST);
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
		for (n=0; n<Elements().nElements(); ++n) if (token == Elements().symbol(n)) break;
		if (n < Elements().nElements())
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
			msg.print(Messenger::Test, "NETA : ...which is a geometry (->NETAGEOMETRYTYPE)\n");
			NetaParser_lval.atomgeom = ag;
			return NETAGEOMETRYTYPE;
		}

		// Is this a NETA value?
		Neta::NetaValue nv = Neta::netaValue(token, FALSE);
		if (nv == Neta::RepeatValue)
		{
			msg.print(Messenger::Test, "NETA : ...which is a repeat value (->NETAREPEAT)\n");
			NetaParser_lval.netaval = nv;
			return NETAREPEAT;
		}
		else if (nv != Neta::nNetaValues)
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
			else if (ne == Neta::GeometryExpander) return NETAGEOMETRY;
			else if (ne == Neta::PathExpander) return NETAPATH;
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
