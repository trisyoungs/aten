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
#include "parser/treenode.h"
#include "parser/grammar.h"
#include "parser/integer.h"
#include "parser/real.h"
#include "base/sysfunc.h"
#include <ctype.h>

// Lexical Analyser - Used the getchar function of the current active Tree (stored in static member Tree::currentTree)
int yylex()
{
	if (Tree::currentTree == NULL)
	{
		printf("Lexer called when no current tree pointer available.\n");
		return 0;
	}

	int c, length;
	bool done, integer, hasexp;
	static char token[256];
	length = 0;
	token[0] = '\0';

	// Skip over whitespace    XXX Unless inside character constant....
	while ((c = Tree::currentTree->getChar()) == ' ' || c == '\t');

	if (c == EOF) return 0;

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
					msg.print("Parse error: Number has two exponentiations (e/E).\n");
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
		// We now have the number, so create a constant and return it
		if (integer)
		{
			NuIntegerVariable *var = new NuIntegerVariable(atoi(token), TRUE);
			yylval.node = var;
			return INTEGER;
		}
		else
		{
			NuRealVariable *var = new NuRealVariable(atof(token), TRUE);
			yylval.node = var;
			return REAL;
		}
	}

	/* Char starts an identifier => read the name.	*/
	if (isalpha (c))
	{
		done = FALSE;
		do
		{
			/* Add this character to the buffer.	  */
			token[length++] = c;
			/* Get another character.			  */
			c = Tree::currentTree->getChar();
		}
		while (isalnum (c));
		Tree::currentTree->unGetChar();
		token[length] = '\0';

		// Search for token name....
/*		s = getsym (symbuf);
		if (s == 0) s = putsym (symbuf, VAR);
		yylval.node = s;
		return s->type;*/
	}

	/* Any other character is a token by itself.	 */
	return c;
}

