/*
	*** Parser Lexer
	*** src/parser/parser_lexer.cpp
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

#include "base/dnchar.h"
#include "parser/variable.h"
#include "parser/grammar.h"
#include "parser/parser.h"
#include "parser/tree.h"
#include "parser/forest.h"
#include "nucommand/commands.h"
#include "base/sysfunc.h"

// Symbols
const char *SymbolTokenKeywords[NuParser::nSymbolTokens] = { "==", ">=", "<=", "!=", "<>", "+=", "-=", "*=", "/=", "++", "--" };
int SymbolTokenValues[NuParser::nSymbolTokens] = { EQ, GEQ, LEQ, NEQ, NEQ, PEQ, MEQ, TEQ, DEQ, PP, MM };

// Original yylex()
int yylex()
{
	return nuparser.lex();
}

// Parser lexer, called by yylex()
int NuParser::lex()
{
	if (forest_ == NULL)
	{
		printf("Lexer called when no target Forest set.\n");
		return 0;
	}

	int c, length, n;
	bool done, integer, hasexp;
	static char token[256];
	static Dnchar name;
	length = 0;
	token[0] = '\0';

	// Skip over whitespace
	while ((c = getChar()) == ' ' || c == '\t');

	if (c == 0) return 0;

	// Set this point as the start of our new token (for error reporting)
	tokenStart_ = stringPos_-1;

	/*
	// A '.' followed by a character indicates a variable path - generate a step
	*/
	msg.print(Messenger::Parse, "LEXER: begins at [%c], peek = [%c]\n",c, peekChar());
	if ((c == '.') && isalpha(peekChar()))
	{
		msg.print(Messenger::Parse, "LEXER (%li): found a '.' before an alpha character - expecting a path step next...\n",tree_);
		expectPathStep_ = TRUE;
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
			c = getChar();
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
					unGetChar();
					token[length] = '\0';
					done = TRUE;
				}
				else token[length++] = c;
			}
			else
			{
				unGetChar();
				token[length] = '\0';
				done = TRUE;
			}
		} while (!done);
		// We now have the number as a text token...
		msg.print(Messenger::Parse, "LEXER (%li): found a number [%s]\n", tree_,token);
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
			c = getChar();
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
		msg.print(Messenger::Parse, "LEXER (%li): found a literal string [%s]...\n",tree_,token);
		name = token;
		yylval.name = &name;
		return CHARCONST;
	}

	/*
	// Alphanumeric-token - function or variable
	*/
	if (isalpha (c))
	{
		do
		{
			token[length++] = c;
			c = getChar();
		}
		while (isalnum (c));
		unGetChar();
		token[length] = '\0';
		msg.print(Messenger::Parse, "LEXER (%li): found an alpha token [%s]...\n", tree_, token);
		// Skip over keyword detection if we are expecting a path step
		if (!expectPathStep_)
		{
			// Is this a variable declaration statement?
			NuVTypes::DataType dt = NuVTypes::dataType(token);
			if (dt != NuVTypes::nDataTypes)
			{
				msg.print(Messenger::Parse, "LEXER (%li): ...which is a variable type name (->DECLARATION)\n",tree_);

				setDeclaredVariableType(dt);
				return DECLARATION;
			}

			// Is this a recognised high-level keyword?
			n = 0;
			if (strcmp(token,"if") == 0) n = IF;
			else if (strcmp(token,"else") == 0) n = ELSE;
			else if (strcmp(token,"for") == 0) n = FOR;
			else if (strcmp(token,"while") == 0) n = WHILE;
			if (n != 0)
			{
				msg.print(Messenger::Parse, "LEXER (%li): ...which is a high-level keyword (%i)\n",tree_,n);
				return n;
			}

			// Is this the start of a filter or a function?
			Tree::FilterType ft = Tree::filterType(token);
			if (ft != Tree::nFilterTypes)
			{
				msg.print(Messenger::Parse, "LEXER (%li): ...which is a filter block name (->FILTERBLOCK)\n",tree_,n);
				tree_ = forest_->createFilter(ft);
				return FILTERBLOCK;
			}

			// If we get to here then its not a high-level keyword.
			// Is it a function keyword?
			for (n=0; n<NuCommand::nCommands; n++) if (strcmp(token,NuCommand::data[n].keyword) == 0) break;
			if (n != NuCommand::nCommands)
			{
				msg.print(Messenger::Parse, "LEXER (%li): ... which is a function (->FUNCCALL).\n", tree_);
				yylval.functionId = n;
				return FUNCCALL;
			}
		}

		// The token isn't a high- or low-level function. It's either a path step or a normal variable
		if (expectPathStep_)
		{
			expectPathStep_ = FALSE;
			msg.print(Messenger::Parse, "LEXER (%li): ...which we assume is a path step (->STEPTOKEN)\n", tree_);
			name = token;
			yylval.name = &name;
			return STEPTOKEN;
		}
		else
		{
			// Search the variable lists currently in scope...
			NuVariable *v;
			if (!isVariableInScope(token, v))
			{
				return 0;
			}
			else if (v != NULL)
			{
				msg.print(Messenger::Parse, "LEXER (%li): ...which is an existing variable (->VARNAME)\n", tree_);
				yylval.variable = v;
				return VARNAME;
			}
		}

		// If we get to here then we have found an unrecognised alphanumeric token (a new variable?)
		msg.print(Messenger::Parse, "LEXER (%li): ...which is is unrecognised (->NEWTOKEN)\n", tree_);
		name = token;
		yylval.name = &name;
		return NEWTOKEN;
	}

	/* We have found a symbolic character (or a pair) that corresponds to an operator */
	// Return immediately in the case of brackets and the semicolon
	if ((c == '(') || (c == ')') || (c == ';') || (c == '{') || (c == '}'))
	{
		msg.print(Messenger::Parse, "LEXER (%li): found symbol [%c]\n",tree_,c);
		return c;
	}
	token[0] = c;
	// Similarly, if the next character is a bracket or double quotes, return immediately
	char c2 = peekChar();
	if ((c2 == '(') || (c2 == ')') || (c2 == ';') || (c2 == '{') || (c2 == '}') || (c2 == '"')) return c;
	// If it is 'punctuation', add this second character to our operator and search for it
	if (ispunct(c2))
	{
		c = getChar();
		token[1] = c;
		token[2] = '\0';
		msg.print(Messenger::Parse, "LEXER (%li): found symbol [%s]\n",tree_,token);
		SymbolToken st = (SymbolToken) enumSearch("", nSymbolTokens, SymbolTokenKeywords, token);
		if (st != nSymbolTokens) return SymbolTokenValues[st];
		else msg.print("Error: Unrecognised symbol found in input.\n");
 	}
	else return c;
	return 0;
}
