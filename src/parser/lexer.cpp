/*
	*** Lexical Analyzer
	*** src/parser/lexer.cpp
	Copyright T. Youngs 2007-2009

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

#include "parser/tree.h"
#include "parser/grammar.h"
#include "base/sysfunc.h"
#include <ctype.h>
#include <string.h>

// Symbolic tokens - array of corresponding values refers to Bison's tokens
class Tokens
{
	public:
	enum SymbolToken { AssignSymbol, GEQSymbol, LEQSymbol, CNEQSymbol, FNEQSymbol, PlusEqSymbol, MinusEqSymbol, TimesEqSymbol, DivideEqSymbol, nSymbolTokens };
};
const char *SymbolTokenKeywords[Tokens::nSymbolTokens] = { "==", ">=", "<=", "!=", "<>", "+=", "-=", "*=", "/=" };
int SymbolTokenValues[Tokens::nSymbolTokens] = { EQ, GEQ, LEQ, NEQ, NEQ, PEQ, MEQ, TEQ, DEQ};

// Lexical Analyser - Used the getchar function of the current active Tree (stored in static member Tree::currentTree)
int yylex()
{
	if (Tree::currentTree == NULL)
	{
		printf("Lexer called when no current tree pointer available.\n");
		return 0;
	}

	int c, length, n;
	bool done, integer, hasexp;
	static char token[256];
	static Dnchar name;
	length = 0;
	token[0] = '\0';

	// Skip over whitespace
	while ((c = Tree::currentTree->getChar()) == ' ' || c == '\t');

	if (c == EOF) return 0;

	/*
	// A '.' followed by a character indicates a variable path - generate a step
	*/
// 	printf("LEx begin at (%c), peek = %c\n",c, Tree::currentTree->peekChar());
	if ((c == '.') && isalpha(Tree::currentTree->peekChar()))
	{
		Tree::currentTree->setExpectPathStep(TRUE);
		return '.';
	}
	
	/*
	// Number Detection - Either '.' or  a digit begins a number
	*/
	if (c == '.' || isdigit (c))
	{
		integer = TRUE;
		hasexp = FALSE;
		token[length++] = c;
		done = FALSE;
		do
		{
			c = Tree::currentTree->getChar();
			if (isdigit(c)) token[length++] = c;
			else if (c == '.')
			{
				integer = FALSE;
				token[length++] = '.';
			}
			else if ((c == 'e') || (c == 'E'))
			{
				// Check for previous exponential in number
				if (hasexp)
				{
					msg.print("Parse Error: Number has two exponentiations (e/E).\n");
					return 0;
				}
				hasexp = TRUE;
				token[length++] = 'E';
			}
			else if ((c == '-') || (c == '+'))
			{
				// We allow '-' or '+' only as part of an exponentiation, so if it is not preceeded by 'E' we stop parsing
				if (token[length-1] != 'E')
				{
					Tree::currentTree->unGetChar();
					token[length] = '\0';
					done = TRUE;
				}
				else token[length++] = c;
			}
			else
			{
				Tree::currentTree->unGetChar();
				token[length] = '\0';
				done = TRUE;
			}
		} while (!done);
		// We now have the number as a text token...
		name = token;
		yylval.name = &name;
		return (integer ? INTCONST : REALCONST);
	}

	/*
	// Literal Character String - surrounded by ""
	*/
	if (c == '"')
	{
		// Just read everything until we find another '"'
		done = FALSE;
		do
		{
			c = Tree::currentTree->getChar();
			if (c == '"')
			{
				// Check for null string...
				if (length == 0) done = TRUE;
				else if (token[length-1] == '\\') token[length++] = '"';
				else done = TRUE;
			}
			else token[length++] = c;
		} while (!done);
		token[length] = '\0';
		name = token;
		yylval.name = &name;
		return CHARCONST;
	}

	/*
	// Alphanumeric-token - function or variable
	*/
	if (isalpha (c))
	{
// 		printf("An alphanumeric token...\n");
		do
		{
			token[length++] = c;
			c = Tree::currentTree->getChar();
		}
		while (isalnum (c));
// 		printf("Character that terminated alphtoken = %c\n", c);
		Tree::currentTree->unGetChar();
		token[length] = '\0';
		// Skip over keyword detection if we are expecting a path step
		if (!Tree::currentTree->expectPathStep())
		{
			// Is this a variable declaration statement?
			NuVTypes::DataType dt = NuVTypes::dataType(token);
			if (dt != NuVTypes::nDataTypes)
			{
				Tree::currentTree->setDeclaredVariableType(dt);
				return DECLARATION;
			}

			// Is this a recognised high-level keyword?
			if (strcmp(token,"if") == 0) return IF;
			else if (strcmp(token,"else") == 0) return ELSE;
			else if (strcmp(token,"for") == 0) return FOR;
			else if (strcmp(token,"while") == 0) return WHILE;
	
			// If we get to here then its not a high-level keyword.
			// Is it a function keyword?
			for (n=0; n<NuCommand::nFunctions; n++) if (strcmp(token,NuCommand::data[n].keyword) == 0) break;
			if (n != NuCommand::nFunctions)
			{
				msg.print(Messenger::Parse, "Found function '%s' (is %i).\n", token, n);
				yylval.functionId = n;
				return FUNCCALL;
			}
		}

		// The token isn't a high- or low-level function. It's either a path step or a normal variable
		if (Tree::currentTree->expectPathStep())
		{
			name = token;
			yylval.name = &name;
			return STEPTOKEN;
		}
		else
		{
			// Search the variable lists currently in scope...
			NuVariable *v;
			if (!Tree::currentTree->isVariableInScope(token, v)) return 0;
			else if (v != NULL)
			{
				yylval.variable = v;
				return VARNAME;
			}
		}

		// If we get to here then we have found an unrecognised alphanumeric token (a new variable?)
		msg.print(Messenger::Parse, "Found unknown token '%s'...\n", token);
		name = token;
		yylval.name = &name;
		return NEWTOKEN;
	}

	/* We have found a symbolic character (or a pair) that corresponds to an operator */
	// Return immediately in the case of single-character literals
	printf("Symbol is %c\n", c);
	if ((c == '(') || (c == ')') || (c == ';') || (c == '{') || (c == '}')) return c;
	// Similarly, if the next character is a bracket, return immediately
	char c2 = Tree::currentTree->peekChar();
	if ((c2 == '(') || (c2 == ')') || (c2 == ';') || (c2 == '{') || (c2 == '}')) return c;
	// If the following character is '"', we also return immediately - have a clause in the following loop...
	do
	{
		token[length++] = c;
		c = Tree::currentTree->getChar();
		if (c == '"') break;
	}
	while (ispunct(c));
	Tree::currentTree->unGetChar();
	token[length] = '\0';
// 	printf("Token is %s\n",token);
	if (length == 1) return token[0];
	else
	{
		Tokens::SymbolToken st = (Tokens::SymbolToken) enumSearch("", Tokens::nSymbolTokens, SymbolTokenKeywords, token);
		if (st != Tokens::nSymbolTokens) return SymbolTokenValues[st];
		else msg.print("Error: Unrecognised symbol found in input.\n");
	}
	return 0;
}

