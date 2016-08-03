/*
	*** Parser Lexer
	*** src/parser/parser_lexer.cpp
	Copyright T. Youngs 2007-2016

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

#include "main/aten.h"
#include "parser/parser.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Must include grammar.hh *after* ATEN_USING_NAMESPACE so that class declarations are found correctly
#include "parser/parser_grammar.hh"

// Symbols
const char* SymbolTokenKeywords[CommandParser::nSymbolTokens] = { "==", ">=", "<=", "!=", "<>", "+=", "-=", "*=", "/=", "++", "--", "&&", "||" };
int SymbolTokenValues[CommandParser::nSymbolTokens] = { EQ, GEQ, LEQ, NEQ, NEQ, PEQ, MEQ, TEQ, DEQ, PLUSPLUS, MINUSMINUS, AND, OR };

// Bison-generated CommandParser_lex()
int CommandParser_lex()
{
	return CommandParser::lex();
}

// Parser lexer, called by yylex()
int CommandParser::lex()
{
	if (program_ == NULL)
	{
		printf("Lexer called when no target Program set.\n");
		return 0;
	}

	int n;
	bool done, integer, hasExp;
	QString token;
	char quoteChar, c;

	// Skip over whitespace
	while ((c = getChar()) == ' ' || c == '\t' || c == '\r' || c == '\n' );

	if (c == 0) return ATEN_EOF;

	// Set this point as the start of our new token (for error reporting)
	tokenStart_ = stringPos_-1;

	/*
	 * A '.' followed by a character indicates a variable path - generate a step
	 */
	Messenger::print(Messenger::Parse, "LEXER (%p): begins at [%c], peek = [%c]", tree_, c, peekChar());
	if ((c == '.') && isalpha(peekChar()))
	{
		Messenger::print(Messenger::Parse, "LEXER (%p): found a '.' before an alpha character - expecting a path step next...",tree_);
		expectPathStep_ = true;
		return '.';
	}

	/*
	 * Number Detection - Either '.' or  a digit begins a number
	 */
	if (c == '.' || isdigit(c))
	{
		// Default to integer, unless first char is '.'
		integer = (c == '.' ? false : true);
		hasExp = false;
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
				if (hasExp)
				{
					if (!quiet_) Messenger::print("Error: Number has two exponentiations (e/E).");
					return 0;
				}
				token += 'E';
				hasExp = true;
			}
			else if ((c == '-') || (c == '+'))
			{
				// We allow '-' or '+' only as part of an exponentiation, so if it is not preceeded by 'E' we stop parsing
				if ((!token.isEmpty()) && (!token.endsWith('E')))
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
		if (!hasExp)
		{
			if (integer) CommandParser_lval.intConst = token.toInt();
			else CommandParser_lval.doubleConst = token.toDouble();
		}
		else
		{
			// Exponentiations are always returned as a double
			integer = false;
			CommandParser_lval.doubleConst = token.toDouble();
		}
		if (integer) Messenger::print(Messenger::Parse, "LEXER (%p): found an integer constant [%s] [%i]", tree_, qPrintable(token), CommandParser_lval.intConst);
		else Messenger::print(Messenger::Parse, "LEXER (%p): found a floating-point constant [%s] [%e]", tree_, qPrintable(token), CommandParser_lval.doubleConst);
		return (integer ? INTCONST : DOUBLECONST);
	}

	/*
	 * Literal Character String - surrounded by ""
	 */
	if ((c == '"') || ( c == '\''))
	{
		quoteChar = c;
		// Just read everything until we find a matching quote
		done = false;
		do
		{
			c = getChar();
			// Check for escaped characters....
			if (c == '\\')
			{
				// Look at next character and either add it as-is, or convert it to its proper control code
				char c2 = getChar();
				switch (c2)
				{
					case ('n'):
						token += '\n'; break;
					case ('t'):
						token += '\t'; break;
					case ('r'):
						token += '\r'; break;
					default:
						token += c2; break;
				}
			}
			else if (c == quoteChar) done = true;
			else if (c == '\0')
			{
				if (!quiet_) Messenger::print("Runaway character constant in input.");
				return 0;
			}
			else token += c;
		} while (!done);
		Messenger::print(Messenger::Parse, "LEXER (%p): found a literal string [%s]...",tree_, qPrintable(token));
		lexedName_ = token;
		return CHARCONST;
	}

	/*
	 * Alphanumeric-token - function or variable
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
		Messenger::print(Messenger::Parse, "LEXER (%p): found an alpha token [%s]...", tree_, qPrintable(token));

		// Skip over keyword detection if we are expecting a path step
		if (!expectPathStep_)
		{
			// Is this a variable declaration statement?
			VTypes::DataType dt = VTypes::dataType(token);
			if (dt != VTypes::nDataTypes)
			{
				Messenger::print(Messenger::Parse, "LEXER (%p): ...which is a variable type name (->VTYPE)",tree_);
				CommandParser_lval.vtype = dt;
				return VTYPE;
			}

			// Built-in constants
			if ((token == "true") || (token == "TRUE"))
			{
				CommandParser_lval.intConst = 1;
				return INTCONST;
			}
			else if ((token == "false") || (token == "FALSE") || (token == "NULL"))
			{
				CommandParser_lval.intConst = 0;
				return INTCONST;
			}
			else if (token == "PI")
			{
				CommandParser_lval.doubleConst = 3.14159265358979323846;
				return DOUBLECONST;
			}
			else if (token == "DEGRAD")
			{
				CommandParser_lval.doubleConst = DEGRAD;
				return DOUBLECONST;
			}
			else if (token == "ANGBOHR")
			{
				CommandParser_lval.doubleConst = ANGBOHR;
				return DOUBLECONST;
			}
			else if (token == "AVOGADRO")
			{
				CommandParser_lval.doubleConst = AVOGADRO;
				return DOUBLECONST;
			}

			// Element symbol?
			for (n=0; n<ElementMap::nElements(); ++n) if (token == ElementMap::symbol(n)) break;
			if (n < ElementMap::nElements())
			{
				CommandParser_lval.intConst = n;
				Messenger::print(Messenger::Parse, "LEXER (%p): ...which is a an element symbol (%i)", tree_, n);
				return ELEMENTCONST;
			}

			// Is this a recognised high-level keyword?
			n = 0;
			if (token == "if") n = ATEN_IF;
			else if (token == "else") n = ATEN_ELSE;
			else if (token == "for") n = ATEN_FOR;
			else if (token == "do") n = ATEN_DO;
			else if (token == "continue") n = ATEN_CONTINUE;
			else if (token == "break") n = ATEN_BREAK;
			else if (token == "while") n = ATEN_WHILE;
			else if (token == "switch") n = ATEN_SWITCH;
			else if (token == "case") n = ATEN_CASE;
			else if (token == "default") n = ATEN_DEFAULT;
			else if (token == "return") n = ATEN_RETURN;
			else if (token == "void") n = ATEN_VOID;
			else if (token == "help") n = HELP;
			else if (token == "in") n = ATEN_IN;
			else if (token == "global") n = ATEN_GLOBAL;
			else if (token == "new") n = ATEN_NEW;
			if (n != 0)
			{
				Messenger::print(Messenger::Parse, "LEXER (%p): ...which is a high-level keyword (%i)",tree_,n);
				return n;
			}

			// Is it an existing variable in scope?
			Variable* v;
			if (tree_ != NULL)
			{
				// Search the variable lists currently in scope...
				int scopelevel;
				v = tree_->findLocalVariable(token, scopelevel);
				if (v != NULL)
				{
					if (scopelevel == 0)
					{
						Messenger::print(Messenger::Parse, "LEXER (%p): ...which is an existing variable in the same scope (->VARSAMESCOPE)", tree_);
						CommandParser_lval.variable = v;
						return VARSAMESCOPE;
					}
					else
					{
						Messenger::print(Messenger::Parse, "LEXER (%p): ...which is an existing variable (->VAR)", tree_);
						CommandParser_lval.variable = v;
						return VAR;
					}
				}
			}

			// Is it an existing variable in global scope?
			for (RefListItem<Tree,bool>* ri = stack_.first(); ri != NULL; ri = ri->next)
			{
				v = ri->item->globalVariables().find(token);
				if (v != NULL)
				{
					if (tree_ == ri->item)
					{
						Messenger::print(Messenger::Parse, "LEXER (%p): ...which is an existing variable in the same global scope (->VARSAMESCOPE)", tree_);
						CommandParser_lval.variable = v;
						return VARSAMESCOPE;
					}
					else
					{
						Messenger::print(Messenger::Parse, "LEXER (%p): ...which is an existing variable in global scope (->VAR)", tree_);
						CommandParser_lval.variable = v;
						return VAR;
					}
				}
			}
			
			// Not in global scope - was it passed as a CLI value?
			v = aten_->findPassedValue(token);
			if (v != NULL)
			{
				Messenger::print(Messenger::Parse, "LEXER (%p): ...which is an existing value passed through the CLI (->VAR).", tree_);
				CommandParser_lval.variable = v;
				return VAR;
			}

			// Is it one of Aten's function keywords?
			n = Commands::command(token);
			if (n != Commands::nCommands)
			{
				Messenger::print(Messenger::Parse, "LEXER (%p): ... which is a function (->FUNCCALL).", tree_);
				CommandParser_lval.functionId = n;
				// Quick check - if we are declaring variables then we must raise an error
				functionStart_ = tokenStart_;
				return FUNCCALL;
			}

			// Is it a user-defined function keyword in the local scope? (Requires valid tree)
			Tree* func;
			if (tree_ != NULL)
			{
				func = tree_->findLocalFunction(token);
				if (func != NULL)
				{
					Messenger::print(Messenger::Parse, "LEXER (%p): ... which is a used-defined function local to this tree (->USERFUNCCALL).", tree_);
					CommandParser_lval.tree = func;
					return USERFUNCCALL;
				}
			}

			// Is it a user-defined function keyword in the global (Program-wide) scope, or Aten's global scope?
			func = program_->findFunction(token);
			if (func == NULL) func = aten_->findIncludeFunction(token);
			if (func != NULL)
			{
				Messenger::print(Messenger::Parse, "LEXER (%p): ... which is a used-defined Program-global function (->USERFUNCCALL).", tree_);
				CommandParser_lval.tree = func;
				return USERFUNCCALL;
			}
		}

		// The token isn't a high- or low-level function. It's either a path step or a normal variable
		if (expectPathStep_)
		{
			expectPathStep_ = false;
			Messenger::print(Messenger::Parse, "LEXER (%p): ...which we assume is a path step (->STEPTOKEN)", tree_);
			lexedName_ = token;
			return STEPTOKEN;
		}

		// If we get to here then we have found an unrecognised alphanumeric token
		Messenger::print(Messenger::Parse, "LEXER (%p): ...which is unrecognised (->NEWTOKEN)", tree_);
		lexedName_ = token;
		return NEWTOKEN;
	}

	/* We have found a symbolic character (or a pair) that corresponds to an operator */
	// Return immediately in the case of brackets, comma, and semicolon
	if ((c == '(') || (c == ')') || (c == ';') || (c == ',') || (c == '{') || (c == '}') || (c == '[') || (c == ']') || (c == '%') || (c == ':') || (c == '?') || (c == ':'))
	{
		Messenger::print(Messenger::Parse, "LEXER (%p): found symbol [%c]",tree_,c);
		return c;
	}
	token += c;
	// Similarly, if the next character is a period, bracket or double quotes, return immediately
	char c2 = peekChar();
	if ((c2 == '.') || (c2 == '(') || (c2 == ')') || (c2 == ';') || (c2 == '{') || (c2 == '}') || (c2 == '"')) return c;
	// If next character is '-', return now if previous char was *not* another '-'
	if ((c2 == '-') && (c != '-')) return c;
	// If it is 'punctuation', add this second character to our operator and search for it
	if (ispunct(c2))
	{
		c = getChar();
		token += c;
		Messenger::print(Messenger::Parse, "LEXER (%p): found symbol [%s]", tree_, qPrintable(token));
		SymbolToken st = (SymbolToken) enumSearch("", nSymbolTokens, SymbolTokenKeywords, qPrintable(token));
		if (st != nSymbolTokens) return SymbolTokenValues[st];
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
