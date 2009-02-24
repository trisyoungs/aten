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

#include "parser/grammar.h"
#include "variables/accesspath.h"
#include "base/sysfunc.h"

int yylex()
{
	int c;
     
	/* Ignore white space, get first nonwhite character.  */
	while ((c = getchar ()) == ' ' || c == '\t');
     
	if (c == EOF) return 0;
     
	/* Char starts a number => parse the number.	  */
	if (c == '.' || isdigit (c))
	{
		ungetc (c, stdin);
		scanf ("%lf", &yylval.val);
		return NUM;
	}
     
	/* Char starts an identifier => read the name.	*/
	if (isalpha (c))
	{
		symrec *s;
		static char *symbuf = 0;
		static int length = 0;
		int i;
     
		/* Initially make the buffer long enough
		for a 40-character symbol name.  */
		if (length == 0)
		  length = 40, symbuf = (char *)malloc (length + 1);
     
		i = 0;
		do
		{
			/* If buffer is full, make it bigger.	 */
			if (i == length)
			{
				length *= 2;
				symbuf = (char *) realloc (symbuf, length + 1);
		 	}
			 /* Add this character to the buffer.	  */
			 symbuf[i++] = c;
			 /* Get another character.			  */
			 c = getchar ();
		}
		while (isalnum (c));
     
		ungetc (c, stdin);
		symbuf[i] = '\0';
     
		s = getsym (symbuf);
		if (s == 0) s = putsym (symbuf, VAR);
		yylval.tptr = s;
		return s->type;
	}
     
	/* Any other character is a token by itself.	 */
	return c;
}

