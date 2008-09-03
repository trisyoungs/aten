/*
	*** File parsing routines
	*** src/base/parser.cpp
	Copyright T. Youngs 2007,2008

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

#include "base/parser.h"
#include "base/sysfunc.h"
#include "base/mathfunc.h"
#include "base/constants.h"
#include "base/messenger.h"

// Singleton
Parser parser;

// Parse options
const char *ParseOptionKeywords[Parser::nParseOptions] = { "defaults", "usequotes", "skipblanks", "stripbrackets", "noexpressions", "noescapes" };
Parser::ParseOption Parser::parseOption(const char *s)
{
	return (Parser::ParseOption) power(2,enumSearch("parse option", Parser::nParseOptions, ParseOptionKeywords, s));
}

// Constructor
Parser::Parser()
{
	// Private variables
	endOfLine_ = FALSE;
	nArgs_ = 0;
	line_[0] = '\0';
	lineLength_ = 0;
	linePos_ = 0;
	optionMask_ = Parser::Defaults;
	sourceFile_ = NULL;
}

// Determine form of argument (internal argument id)
Parser::ArgumentForm Parser::argumentForm(int argno)
{
	// If the argument was quoted with single-quotes then we assume it is an expression
	// Similarly, if it was double-quoted we assume it is a character constant
	if (quoted_[argno] == 34) return Parser::ConstantForm;
	else if (quoted_[argno] == 39) return Parser::ExpressionForm;
	else return argumentForm(arguments_[argno].get());
}

// Determine form of argument (from character string, no quoting recognition)
Parser::ArgumentForm Parser::argumentForm(const char *s)
{
	// At least one full stop surrounded by a letter on the right and a letter or a square bracket on the left
	char *c = strchr(s, '.');
	if (c != '\0')
	{
		char *r = c++;
		char *l = c--;
		//printf("leftchar = '%c', rightchar = '%c'\n", *l, *r);
		if ((!isdigit(*l)) && (!isdigit(*r))) return Parser::ReferenceForm;
	}
	int noperators, n, nvars, nbrackets;
	noperators = countChars(s, "-+*/^%", 1);
	nbrackets = countChars(s, "()");
	nvars = countChars(s, "$");
	bool hasneg = s[0] == '-';
	// If there are operators or brackets it can only be an expression
	if ((noperators > 0) || (nbrackets > 0)) return Parser::ExpressionForm;
	else if ((hasneg) && (nvars > 0)) return Parser::ExpressionForm;
	else if (nvars == 1) return Parser::VariableForm;
	else return Parser::ConstantForm;
}

// Returns number of arguments grabbed from last parse
int Parser::nArgs()
{
	return nArgs_;
}

// Returns the specified argument as a character string
const char *Parser::argc(int i)
{
	return arguments_[i].get();
}

// Returns the specified argument as an integer
int Parser::argi(int i)
{
	return arguments_[i].asInteger();
}

// Returns the specified argument as a double
double Parser::argd(int i)
{
	return arguments_[i].asDouble();
}

// Returns the specified argument as a bool
bool Parser::argb(int i)
{
	return arguments_[i].asBool();
}

// Returns the specified argument as a float
float Parser::argf(int i)
{
	return (float) argd(i);
}

// Returns whether the specified argument is empty
bool Parser::isBlank(int i)
{
	return (arguments_[i][0] == '\0' ? TRUE : FALSE);
}

// Returns whether the specified argument was quoted
bool Parser::wasQuoted(int i)
{
	return (quoted_[i] == 0 ? FALSE : TRUE);
}

// Set argument manually
void Parser::setArg(int i, const char *s)
{
	arguments_[i] = s;
}

// Return pointer to current line
const char *Parser::line()
{
	return line_;
}

/*
// String parsing methods
*/

// Read single line from file
int Parser::readLine(ifstream *xfile)
{
	msg.enter("Parser::readLine");
	// Returns : 0=ok, 1=error, -1=eof
	sourceFile_ = xfile;
	sourceFile_->getline(line_, MAXLINELENGTH-1);
	if (sourceFile_->eof())
	{
		sourceFile_->close();
		msg.exit("Parser::readLine");
		return -1;
	}
	if (sourceFile_->fail())
	{
		sourceFile_->close();
		msg.exit("Parser::readLine");
		return 1;
	}
	lineLength_ = strlen(line_);
	linePos_ = 0;
	//printf("Line = [%s], length = %i\n",line_,lineLength_);
	msg.exit("Parser::readLine");
	return 0;
}

bool Parser::getNextArg(int destarg)
{
	// Get the next input chunk from the internal string and put into argument specified.
	msg.enter("Parser::getNextArg");
	static int arglen, readresult;
	static bool done, hadquotes, expression, failed;
	static char c, quotechar, d;
	failed = FALSE;
	done = FALSE;
	hadquotes = FALSE;
	quotechar = '\0';
	expression = FALSE;
	endOfLine_ = FALSE;
	arglen = 0;
	while (linePos_ < lineLength_)
	{
		c = line_[linePos_];
		switch (c)
		{
			// Backslash - escape next character (read new line if its an EOL marker)
			// If we're inside quotes, keep backslash *and* next character
			case (92):
				d = line_[linePos_ + 1];
				if ((optionMask_&Parser::NoEscapes) || (quotechar != '\0'))
				{
					tempArg_[arglen] = c;
					arglen ++;
					tempArg_[arglen] = d;
					arglen ++;
					linePos_ ++;
				}
				else if ((d == 10) || (d == 13))
				{
					// Next char is newline, so read another line from file if we have one
					if (sourceFile_ == NULL)
					{
						done = TRUE;
						endOfLine_ = TRUE;
					}
					else
					{
						// Read a new line
						readresult = readLine(sourceFile_);
						if (readresult != 0)
						{
							failed = TRUE;
							done = TRUE;
						}
					}
				}
				else
				{
					tempArg_[arglen] = d;
					arglen ++;
					linePos_ ++;
				}
				break;
			// End of line markers
			case (10):	// Line feed (\n)
			case (13):	// Carriage Return
				done = TRUE;
				endOfLine_ = TRUE;
				break;
			// Delimiters
			// If we encounter one and arg length != 0 this signals the end of the argument.
			case (9):	// Horizontal Tab
			case (' '):	// Space
			case (','):	// Comma
				if ((quotechar != '\0') || expression)
				{
					tempArg_[arglen] = c;
					arglen ++;
				}
				else if (arglen != 0) done = TRUE;
				break;
			// Quote marks
			// If Parser::UseQuotes, keep delimiters and other quote marks inside the quoted text.
			case (34):	// Double quotes
			case (39):	// Single quotes
				if (expression) break;
				if (!(optionMask_&Parser::UseQuotes)) break;
				if (quotechar == '\0') quotechar = c;
				else if (quotechar == c)
				{
					quotechar = '\0';
					hadquotes = TRUE;
					// If double-quotes, set the quotes flag
					if (destarg != -1) quoted_[destarg] = c;
					done = TRUE;
				}
				else
				{
					tempArg_[arglen] = c;
					arglen ++;
				}
				break;
			// Brackets
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if ((optionMask_&Parser::StripBrackets) && (!expression)) break;
				tempArg_[arglen] = c;
				arglen ++;
				break;
			// Comment markers
			case ('#'):	// "#" Rest/all of line is a comment
				endOfLine_ = TRUE;
				done = TRUE;
				break;
			// Normal character
			default: 
				tempArg_[arglen] = c;
				arglen ++;
				break;
		}
		// Increment line position
		linePos_++;
		if (done || failed) break;
	}
	// Finalise argument
	tempArg_[arglen] = '\0';
	if (linePos_ == lineLength_) endOfLine_ = TRUE;
	// Store the result in the desired destination
	if (destarg != -1) arguments_[destarg] = tempArg_;
	// Strip off the characters up to position 'n', but not including position 'n' itself
	//line_.eraseStart(n+1);
	//printf("Rest of line is now [%s]\n",line.get());
	msg.exit("Parser::getNextArg");
	if (failed) return FALSE;
	return (arglen == 0 ? (hadquotes ? TRUE : FALSE) : TRUE);
}

// Rip next n characters
bool Parser::getNextN(int length)
{
	// Put the next 'length' characters from line_ into temparg.
	msg.enter("Parser::getNextN");
	int arglen = 0;
	char c;
	if (lineLength_ == 0)
	{
		msg.exit("Parser::getNextN");
		return FALSE;
	}
	int charsleft = lineLength_ - linePos_;
	if (length > charsleft) length = charsleft;
	//if (length > lineLength_) length = lineLength_;
	for (int n=0; n<length; n++)
	{
		c = line_[linePos_];
		switch (c)
		{
			// Brackets
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optionMask_&Parser::StripBrackets) break;
				tempArg_[arglen] = c;
				arglen ++;
				break;
		//	case (32):      // Space - ignore to get left-justified data
		//		break;
			default:
				tempArg_[arglen] = c;
				arglen ++;
				break;
		}
		linePos_ ++;
	}
	// Add terminating character to temparg
	tempArg_[arglen] = '\0';
	//printf("getNextN found [%s], length = %i\n", tempArg_, arglen);
	//line_.eraseStart(length);
	msg.exit("Parser::getNextN");
	return TRUE;
}

// Get all arguments (delimited) from Parser::line_
void Parser::getAllArgsDelim()
{
	// Parse the string in 'line_' into arguments in 'args'
	msg.enter("Parser::getAllArgsDelim");
	nArgs_ = 0; 
	for (int n=0; n<MAXARGS; n++)
	{
		arguments_[n].clear();
		quoted_[n] = 0;
	}
	endOfLine_ = FALSE;
	while (!endOfLine_)
	{
		if (getNextArg(nArgs_))
		{
			msg.print(Messenger::Parse,"getAllArgsDelim arg=%i [%s]\n", nArgs_, argc(nArgs_));
			nArgs_ ++;
		}
	}
	msg.exit("Parser::getAllArgsDelim");
}

/*
// Delimited Parsing Routines
*/

// Parse delimited (from file)
int Parser::getArgsDelim(ifstream *xfile, int options)
{
	// Standard file parse routine.
	// Splits the line from the file into delimited arguments via the 'parseline' function
	msg.enter("Parser::getArgsDelim[ifstream]");
	bool done = FALSE;
	int result;
	// Lines beginning with '#' are ignored as comments
	// Blank lines are skipped if blankskip == TRUE.
	// Returns : 0=ok, 1=error, -1=eof
	optionMask_ = options;
	do
	{
		// Read line from file and parse it
		result = readLine(xfile);
		if (result != 0)
		{
			msg.exit("Parser::getArgsDelim[ifstream]");
			return result;
		}
		// Assume that we will finish after parsing the line we just read in
		done = TRUE;
		// To check for blank lines, do the parsing and then check nargs()
		getAllArgsDelim();
		if ((optionMask_&Parser::SkipBlanks) && (nArgs_ == 0)) done = FALSE;
	} while (!done);
	msg.exit("Parser::getArgsDelim[ifstream]");
	return 0;
}

// Get next argument (delimited) from file stream
const char *Parser::getArgDelim(ifstream *xfile)
{
	msg.enter("Parser::getArgDelim[ifstream]");
	static char result[512];
	static int length;
	static bool done;
	// Clear old result
	result[0] = '\0';
	length = 0;
	done = FALSE;
	*xfile >> result;
	msg.exit("Parser::getArgDelim[ifstream]");
	return result;
}

// Parse all arguments (delimited) from string
void Parser::getArgsDelim(const char *s, int options)
{
	strcpy(line_,s);
	lineLength_ = strlen(line_);
	linePos_ = 0;
	optionMask_ = options;
	getAllArgsDelim();
}

// Parse string into lines
void Parser::getLinesDelim(const char *s)
{
	static int n, arglen;
	static bool done;
	static char c, quotechar, arg[MAXARGLENGTH];
	done = FALSE;
	quotechar = '\0';
	endOfLine_ = FALSE;
	nArgs_ = 0;
	arglen = 0;
	for (n=0; s[n] != '\0'; n++)
	{
		c = s[n];
		switch (c)
		{
			// End of line delimiters
			case (10):	// Line feed (\n)
			case (13):	// Carriage Return
			case (';'):	// Semicolon
				// Store the current argument and reset counters
				arg[arglen] = '\0';
				arguments_[nArgs_] = arg;
				nArgs_++;
				arglen = 0;
				break;
			// Otherwise, add the character to the string
			default:
				arg[arglen] = c;
				arglen ++;
				break;
		}
	}
	// Check current length of arg since we will miss the last argument as it terminates with '\0'
	if (arglen != 0)
	{
		arg[arglen] = '\0';
		arguments_[nArgs_] = arg;
		nArgs_ ++;
	}
}

// Skip lines from file
int Parser::skipLines(ifstream *xfile, int nlines)
{
	msg.enter("Parser::skipLines");
	int result;
	for (int n=0; n<nlines; n++)
	{
		result = readLine(xfile);
		if (result != 0)
		{
			msg.exit("Parser::skipLines");
			return result;
		}
	}
	msg.exit("Parser::skipLines");
	return 0;
}

// Shift all arguments up one position (leaving arg[0] blank)
void Parser::shiftArgsUp()
{
	msg.enter("Parser::shiftArgsUp");
	for (int i=MAXARGS-1; i>0; i--)
	{
		arguments_[i] = arguments_[i-1];
		quoted_[i] = quoted_[i-1];
	}
	nArgs_ ++;
	arguments_[0].clear();
	msg.exit("Parser::shiftArgsUp");
}

/*
// Atom type definition functions
*/

const char *Parser::parseAtomtypeString(Dnchar &source)
{
	// Cut the next atomtype command from the supplied string. Put in 'dest', along with any bracketed
	// argument part. Use brackets a bit like quotes are used above, except we don't toggle the flag.
	// Ignore spaces and horizontal tabs. Commas separate commands.
	msg.enter("Parser::parseAtomtypeString");
	static int n, nchars, bracketlevel;
	static bool done, el_list;
	static Dnchar typecmd;
	nchars = 0;
	bracketlevel = 0;
	el_list = FALSE;
	done = FALSE;
	typecmd.createEmpty(source);
	//source.print();
	for (n=0; n<source.length(); n++)
	{
		switch (source[n])
		{
			case (32):	// Space
				break;
			case (10):	// Line feed (\n) - Signals end of the keyword.
				done = TRUE;
				break;
			case (13):	// Carriage Return - Signals end of the keyword.
				done = TRUE;
				break;
			case (44):	// Comma - end of keyword or separator in element list
				if (bracketlevel == 0 && !el_list) done = TRUE;
				else typecmd += ',';
				break;
			case (9):	// Horizontal Tab
				break;
			case (40):	// "(" - Signals beginning of options list
				bracketlevel ++;
				typecmd += '(';
				break;
			case (41):	// ")" - Signals end of options list
				bracketlevel --;
				typecmd += ')';
				break;
			case (91):	// "[" - Signals beginning of element list
				el_list = TRUE;
				// If we're inside brackets, *don't* remove it
				if (bracketlevel != 0) typecmd += source[n];
				break;
			case (93):	// "]" - Signals end of element list
				el_list = FALSE;
				// If we're inside brackets, *don't* remove it
				if (bracketlevel != 0) typecmd += source[n];
				break;
			default: 
				typecmd += source[n];
				break;
		}
		if (done) break;
	}
	// Strip off the keyword characters from the string.
	//printf("Erasing in parseAtomtypeString...\n");
	// ERASE now takes 'start' and 'end' (not 'count') as parameters.
	source.eraseStart(n+1);
	//printf("Result = ");
	//typecmd.print();
	msg.exit("Parser::parseAtomtypeString");
	return typecmd.get();
}

const char *Parser::trimAtomtypeKeyword(Dnchar &source)
{
	// Remove the keyword part of the command and put in 'dest', leaving the options (minus brackets)
	// in the original string. Remove '[' and ']' from keyword since this is only used to keep a list of elements together.
	msg.enter("Parser::trimAtomtypeKeyword");
	static bool done, equals;
	static Dnchar keywd;
	done = FALSE;
	equals = FALSE;
	//printf("String given to trimAtomKeyword = '%s'\n",source.get());
	keywd.createEmpty(source);
	int n;
	for (n=0; n<source.length(); n++)
	{
		switch (source[n])
		{
			case (40):	// "(" - if present signals start of options
				done = TRUE;
				break;
			case (10):	// Line feed (\n) - Signals end of the keyword.
				done = TRUE;
				break;
			case (13):	// Carriage Return - Signals end of the keyword.
				done = TRUE;
				break;
			case (61):	// "=" - Signals keyword/single option delimiter (unless first character)
				if (n == 0) keywd += source[n];
				else
				{
					done = TRUE;
					equals = TRUE;
				}
				break;
			case (91):	// '['
			case (93):	// ']'
				break;
			default: 
				keywd += source[n]; 
				break;
		}
		if (done) break;
	}
	// Trim off the keyword part
	if (equals) source.eraseStart(n+1);
	else source.eraseStart(n);
	//printf("RESULT in trim atkeywd = '%s'\n",dest.get());
	// Remove brackets if they're there
	if (source.length() != 0) 
		if (source[0] == '(')
		{
			source.eraseStart(1);
			source.eraseEnd(1);
		}
	msg.exit("Parser::trimAtomtypeKeyword");
	return keywd.get();
}
