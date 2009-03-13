/*
	*** Parser
	*** src/parser/parser.cpp
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
#include "nucommand/commands.h"
#include "base/sysfunc.h"

// External Declarations
NuParser nuparser;
int yyparse();

// Symbols
const char *SymbolTokenKeywords[NuParser::nSymbolTokens] = { "==", ">=", "<=", "!=", "<>", "+=", "-=", "*=", "/=" };
int SymbolTokenValues[NuParser::nSymbolTokens] = { EQ, GEQ, LEQ, NEQ, NEQ, PEQ, MEQ, TEQ, DEQ};

// Constructor
NuParser::NuParser()
{
	// Private variables
	isFileSource_ = FALSE;
	fileSource_ = NULL;
	stringPos_ = -1;
	stringLength_ = 0;
	lineNumber_ = 0;
	expectPathStep_ = FALSE;

	// Public variables
	tree = NULL;
}

// Destructor
NuParser::~NuParser()
{
}

// Print error information and location
void NuParser::printErrorInfo()
{
	// QUICK'n'DIRTY!
	char *temp = new char[stringLength_+32];
	for (int i=0; i<stringPos_; i++) temp[i] = ' ';
	temp[stringPos_] = '\0';
	// Print current string
	if (isFileSource_)
	{
		printf("(Line %4i) : %s\n", stringSource_.get());
		printf("           : %s^\n", temp);
	}
	else
	{
		printf(" %s\n", stringSource_.get());
		printf(" %s^\n", temp);
	}
	delete[] temp;
}

/*
// Character Stream Retrieval
*/

// Return whether the current input stream is a file
bool NuParser::isFileSource()
{
	return isFileSource_;
}

// Get next character from current input stream
char NuParser::getChar()
{
	char c = 0;
	if (isFileSource_)
	{
	}
	else
	{
		// Return current character
		if (stringPos_ == stringLength_) return '\0';
		c = stringSource_[stringPos_];
		// Increment string position
		stringPos_++;
	}
	return c;
}

// Peek next character from current input stream
char NuParser::peekChar()
{
	char c = 0;
	if (isFileSource_)
	{
	}
	else
	{
		// Return current character
		if (stringPos_ == stringLength_) return '\0';
		c = stringSource_[stringPos_];
	}
	return c;
}

// 'Replace' last character read from current input stream
void NuParser::unGetChar()
{
	if (isFileSource_)
	{
	}
	else
	{
		// Decrement string position
		stringPos_--;
	}
}

// Original yylex()
int yylex()
{
	return nuparser.lex();
}

// Parser lexer, called by yylex()
int NuParser::lex()
{
	if (tree == NULL)
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
	while ((c = getChar()) == ' ' || c == '\t');

	if (c == EOF) return 0;

	/*
	// A '.' followed by a character indicates a variable path - generate a step
	*/
// 	printf("LEx begin at (%c), peek = %c\n",c, Tree::currentTree->peekChar());
	if ((c == '.') && isalpha(peekChar()))
	{
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
			c = getChar();
		}
		while (isalnum (c));
// 		printf("Character that terminated alphtoken = %c\n", c);
		unGetChar();
		token[length] = '\0';
		// Skip over keyword detection if we are expecting a path step
		if (!expectPathStep_)
		{
			// Is this a variable declaration statement?
			NuVTypes::DataType dt = NuVTypes::dataType(token);
			if (dt != NuVTypes::nDataTypes)
			{
				tree->setDeclaredVariableType(dt);
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
		if (expectPathStep_)
		{
			name = token;
			yylval.name = &name;
			return STEPTOKEN;
		}
		else
		{
			// Search the variable lists currently in scope...
			NuVariable *v;
			if (!tree->isVariableInScope(token, v)) return 0;
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
	char c2 = peekChar();
	if ((c2 == '(') || (c2 == ')') || (c2 == ';') || (c2 == '{') || (c2 == '}')) return c;
	// If the following character is '"', we also return immediately - have a clause in the following loop...
	do
	{
		token[length++] = c;
		getChar();
		if (c == '"') break;
	}
	while (ispunct(c));
	unGetChar();
	token[length] = '\0';
// 	printf("Token is %s\n",token);
	if (length == 1) return token[0];
	else
	{
		SymbolToken st = (SymbolToken) enumSearch("", nSymbolTokens, SymbolTokenKeywords, token);
		if (st != nSymbolTokens) return SymbolTokenValues[st];
		else msg.print("Error: Unrecognised symbol found in input.\n");
	}
	return 0;
}

/*
// Tree Data
*/

// Create a new tree in the forest
void NuParser::createTree()
{
}

// Create a new function tree in the forest
void NuParser::createFunction()
{
}

/*
// Tree Generation
*/

// Fill target forest from specified character string
bool NuParser::generate(Forest *f, const char *s)
{
	msg.enter("NuParser::generate[string]");
	// Clear any data in the existing forest
	if (f == NULL)
	{
		printf("Internal Error: No Forest passted to NuParser::generate().\n");
		msg.exit("NuParser::generate[string]");
		return FALSE;
	}
	f->clear();
	// Store the source string
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	isFileSource_ = FALSE;
	expectPathStep_ = FALSE;
	// Perform the parsing
	int result = yyparse();
	if (result != 0)
	{
		msg.print("Error occurred here:\n");
		printErrorInfo();
		msg.exit("NuParser::generate[string]");
		return FALSE;		
	}
	else print();
	msg.exit("NuParser::generate[string]");
	return TRUE;
}
