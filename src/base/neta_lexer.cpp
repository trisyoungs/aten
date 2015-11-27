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
#include "base/elementmap.h"
#include "base/neta.h"
#include "base/neta_parser.h"

ATEN_USING_NAMESPACE

// Must include grammar.hh *after* ATEN_USING_NAMESPACE so that class declarations are found correctly
#include "base/neta_grammar.hh"

// Symbols
const char* NetaSymbolTokenKeywords[NetaParser::nNetaSymbolTokens] = { ">=", "<=", "!=", "<>" };
int NetaSymbolTokenValues[NetaParser::nNetaSymbolTokens] = { GEQ, LEQ, NEQ, NEQ };

// Bison-generated NetaParser_lex()
int NetaParser_lex()
{
	return NetaParser::lex();
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
	static QString token;
	char c;
	token.clear();

	// Skip over whitespace
	while ((c = getChar()) == ' ' || c == '\t' || c == '\r' || c == '\n' );

	if (c == 0) return 0;

	// Set this point as the start of our new token (for error reporting)
	tokenStart_ = stringPos_-1;

	/*
	 * Integer number
	 */
	/*
	 * Number Detection - Either '-', '.' or a digit begins a number
	 */
	bool nextCharIsPossibleDigit = (isdigit(peekChar()) || (peekChar() == '.'));
	if ((c == '.') || isdigit(c) || ((c == '-') && nextCharIsPossibleDigit))
	{
		// Default to integer, unless first char is '.'
		bool integer = (c != '.');
		bool hasexp = false;
		token += c;
		done = false;
		do
		{
			c = getChar();
			if (isdigit(c)) token += c;
			else if (c == '.')
			{
				integer = false;
				token += '.';
			}
			else if ((c == 'e') || (c == 'E'))
			{
				// Check for previous exponential in number
				if (hasexp)
				{
					Messenger::print("Error: Number has two exponentiations (e/E).");
					return 0;
				}
				token += 'E';
				hasexp = true;
			}
			else if ((c == '-') || (c == '+'))
			{
				// We allow '-' or '+' only as part of an exponentiation, so if it is not preceeded by 'E' we stop parsing
				if ((!token.isEmpty()) && (!token.endsWith("E")))
				{
					unGetChar();
					done = true;
				}
				else token += c;
			}
			else
			{
				unGetChar();
				done = true;
			}
		} while (!done);

		// We now have the number as a text token...
		if (!hasexp)
		{
			if (integer) NetaParser_lval.intConst = token.toInt();
			else NetaParser_lval.doubleConst = token.toDouble();
		}
		else
		{
			// Exponentiations are always returned as a double
			integer = false;
			NetaParser_lval.doubleConst = token.toDouble();
		}
		if (integer) Messenger::print(Messenger::Parse, "NETA : found an integer constant [%s] [%i]", qPrintable(token), NetaParser_lval.intConst);
		else Messenger::print(Messenger::Parse, "NETA : found a floating-point constant [%s] [%e]", qPrintable(token), NetaParser_lval.doubleConst);
		return (integer ? INTCONST : DOUBLECONST);
	}

	/*
	 * Alphanumeric-token : Element symbol, NETA keyword, value, expander, or geometry
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
		Messenger::print(Messenger::Typing, "NETA : found an alpha token [%s]...", qPrintable(token));

		// Element Symbol (or 'Any')
		if (token == "Any")
		{
			NetaParser_lval.intConst = 0;
			Messenger::print(Messenger::Typing, "NETA : ...which is the any element symbol (Any)");
			return ELEMENT;
		}
		for (n=0; n<Elements().nElements(); ++n) if (token == Elements().symbol(n)) break;
		if (n < Elements().nElements())
		{
			NetaParser_lval.intConst = n;
			Messenger::print(Messenger::Typing, "NETA : ...which is a an element symbol (%i)",n);
			return ELEMENT;
		}

		// Is this a NETA keyword?
		Neta::NetaKeyword nk = Neta::netaKeyword(token, false);
		if (nk != Neta::nNetaKeywords)
		{
			Messenger::print(Messenger::Typing, "NETA : ...which is a keyword (->NETAKEY)");
			NetaParser_lval.netaKey = nk;
			return NETAKEY;
		}

		// Is this a NETA geometry?
		Atom::AtomGeometry ag = Atom::atomGeometry(token, false);
		if (ag != Atom::nAtomGeometries)
		{
			Messenger::print(Messenger::Typing, "NETA : ...which is a geometry (->NETAGEOMETRYTYPE)");
			NetaParser_lval.atomGeom = ag;
			return NETAGEOMETRYTYPE;
		}

		// Is this a NETA value?
		Neta::NetaValue nv = Neta::netaValue(token, false);
		if (nv == Neta::RepeatValue)
		{
			Messenger::print(Messenger::Typing, "NETA : ...which is a repeat value (->NETAREPEAT)");
			NetaParser_lval.netaVal = nv;
			return NETAREPEAT;
		}
		else if (nv != Neta::nNetaValues)
		{
			Messenger::print(Messenger::Typing, "NETA : ...which is a value (->NETAVAL)");
			NetaParser_lval.netaVal = nv;
			return NETAVAL;
		}

		// Is this a NETA expander?
		Neta::NetaExpander ne = Neta::netaExpander(token, false);
		if (ne != Neta::nNetaExpanders)
		{
			Messenger::print(Messenger::Typing, "NETA : ...which is an expander (->NETAEXP)");
			if (ne == Neta::RingExpander) return NETARING;
			else if (ne == Neta::ChainExpander) return NETACHAIN;
			else if (ne == Neta::GeometryExpander) return NETAGEOMETRY;
			else if (ne == Neta::PathExpander) return NETAPATH;
			return 0;
		}

		// Is it a bond type?
		Bond::BondType bt = Bond::bondType(token, false);
		if (bt != Bond::nBondTypes)
		{
			Messenger::print(Messenger::Typing, "NETA : ...which is a bond type (->INTCONST, %i)", bt);
			NetaParser_lval.intConst = bt;
			return INTCONST;
		}

		// If we get to here then we have found an unrecognised alphanumeric token
		Messenger::print(Messenger::Typing, "NETA : ...which is unrecognised (->TOKEN)");
		lastUnknownToken_ = token;
		return TOKEN;
	}

	/* We have found a symbolic character (or a pair) that corresponds to an operator */
	// Return immediately in the case of brackets, commas etc.
	if ((c == '(') || (c == ')') || (c == ',') || (c == '[') || (c == ']') || (c == '-') || (c == '~') || (c == '&') || ( c == '!') || (c == '$') || (c == '=') || (c == '?') || (c == ':'))
	{
		Messenger::print(Messenger::Typing, "NETA : found symbol [%c]",c);
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
		Messenger::print(Messenger::Typing, "NETA : found symbol [%s]", qPrintable(token));
		NetaSymbolToken st = (NetaSymbolToken) enumSearch("", nNetaSymbolTokens, NetaSymbolTokenKeywords, token);
		if (st != nNetaSymbolTokens) return NetaSymbolTokenValues[st];
		else Messenger::print("Error: Unrecognised symbol found in input (%s).", qPrintable(token));
 	}
	else
	{
		// Make sure that this is a known symbol
		if ((c == '$') || (c == '%') || (c == '&') || (c == '@') || (c == '?') || (c == ':'))
		{
			Messenger::print("Error: Unrecognised symbol found in input (%c).", c);
		}
		else return c;
	}
	return 0;
}
