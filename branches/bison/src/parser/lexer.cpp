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

#include "parser/treenode.h"
#include "parser/grammar.h"
#include "parser/commandnode.h"
#include "parser/integer.h"
#include "parser/character.h"
#include "parser/real.h"
#include "parser/variablenode.h"
#include "base/sysfunc.h"
#include "parser/tree.h"
#include <ctype.h>
#include <string.h>

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
	// A '.' followed by a character indicates a variable path - stop here and return '.'
	*/
	printf("LEx begin at (%c), peek = %c\n",c, Tree::currentTree->peekChar());
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
		// We now have the number, so create a constant and return it
		if (integer)
		{
			NuIntegerVariable *var = new NuIntegerVariable(atoi(token), TRUE);
			yylval.node = var;
			Tree::currentTree->addConstant(var);
			return INTCONST;
		}
		else
		{
			NuRealVariable *var = new NuRealVariable(atof(token), TRUE);
			yylval.node = var;
			Tree::currentTree->addConstant(var);	
			return REALCONST;
		}
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
		NuCharacterVariable *var = new NuCharacterVariable(token, TRUE);
		Tree::currentTree->addConstant(var);
		yylval.node = var;
		return CHARCONST;
	}

	/*
	// Alphanumeric-token - function or variable
	*/
	if (isalpha (c))
	{
		printf("An alphanumeric token...\n");
		do
		{
			token[length++] = c;
			c = Tree::currentTree->getChar();
		}
		while (isalnum (c));
		printf("Character that terminated alphtoken = %c\n", c);
		Tree::currentTree->unGetChar();
		token[length] = '\0';

		// Skip over keyword detection if we are expecting a path step
		if (!Tree::currentTree->expectPathStep())
		{
			// Is this a recognised high-level keyword?
			if (strcmp(token,"integer") == 0) return INTEGER;
			else if (strcmp(token,"real") == 0) return REAL;
			else if (strcmp(token,"character") == 0) return CHARACTER;
			else if (strcmp(token,"vector") == 0) return VECTOR;
			else if (strcmp(token,"atom") == 0) return ATOM;
			else if (strcmp(token,"forcefield") == 0) return FORCEFIELD;
			else if (strcmp(token,"grid") == 0) return GRID;
			else if (strcmp(token,"model") == 0) return MODEL;
			else if (strcmp(token,"pattern") == 0) return PATTERN;
			else if (strcmp(token,"if") == 0) return IF;
			else if (strcmp(token,"else") == 0) return ELSE;
			else if (strcmp(token,"for") == 0) return FOR;
			else if (strcmp(token,"while") == 0) return WHILE;
	
			// If we get to here then its not a high-level keyword.
			// Is it a function keyword?
			for (n=0; n<NuCommand::nFunctions; n++) if (strcmp(token,NuCommand::data[n].keyword) == 0) break;
			if (n != NuCommand::nFunctions)
			{
				printf("Command is [%s], id = %i\n", token, n);
				yylval.functionId = n;
				// Our return type here depends on the return type of the function
				switch (NuCommand::data[n].returnType)
				{
					case (NuVTypes::NoData):
						return VOIDFUNCCALL;
						break;
					case (NuVTypes::IntegerData):
					case (NuVTypes::RealData):
						return NUMFUNCCALL;
						break;
					case (NuVTypes::CharacterData):
						return CHARFUNCCALL;
						break;
					case (NuVTypes::VectorData):
						return VECFUNCCALL;
						break;
					default:
						return PTRFUNCCALL;
						break;
				}
			}
		}

		// The token isn't a high- or low-level function. It's either a path step or a normal variable
		if (Tree::currentTree->expectPathStep())
		{
			// Search the path variable at the top of the pathStack for an accessor matching this token...
			AccessNode *newstep = Tree::currentTree->searchAccessors(token);
	printf("Accessor token is %li\n", newstep);
			if (newstep == NULL) return 0;

			// Flag that we don't necessarily expect another path step. This will be set to true on the next discovery of a '.' before an alpha
			Tree::currentTree->setExpectPathStep(FALSE);
			yylval.node = (TreeNode*) newstep;
			return STEP;
		}
		else
		{
			// Search the variable lists currently in scope...
			NuVariable *v = Tree::currentTree->isVariableInScope(token);
			if (v != NULL)
			{
				// Since this is a proper, modifiable variable we must encapsulate it in a VariableNode and pass that instead
				VariableNode *vnode = new VariableNode(v);
				yylval.node = vnode;
				// Are we already 'in' a path?
				// Peek the next character - if it is 
				// Our return type here depends on the type of the variable
				switch (v->returnType())
				{
					case (NuVTypes::NoData):
						return 0;
						break;
					case (NuVTypes::IntegerData):
					case (NuVTypes::RealData):
						return NUMVAR;
						break;
					case (NuVTypes::CharacterData):
						return CHARVAR;
						break;
					case (NuVTypes::VectorData):
						return VECVAR;
						break;
					default:
						return PTRVAR;
						break;
				}
			}
		}

		// If we get to here then we have found an unrecognised alphanumeric token (a new variable?)
		printf("Lexer found an unknown token name = [%s]\n", token);
		name = token;
		yylval.name = &name;
		return TOKENNAME;
	}

	printf("Symbolic character...\n");
	/* We have found a symbolic character (or a pair) that corresponds to an operator */
	// Return immediately in the case of string single-character literals
	if ((c == '(') || (c == ')') || (c == ';') || (c == '{') || (c == '}')) return c;
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
	printf("Token is %s\n",token);
	if (length == 1) return token[0];
	else
	{
		if (strcmp(token,"==") == 0) return EQ;
		else if (strcmp(token,">=") == 0) return GEQ;
		else if (strcmp(token,"<=") == 0) return LEQ;
		else if (strcmp(token,"!=") == 0) return NEQ;
		else if (strcmp(token,"<>") == 0) return NEQ;
	}
	return 0;
}

