/*
	*** File parsing routines
	*** src/parse/parse.cpp
	Copyright T. Youngs 2007

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

#include "base/prefs.h"
#include "base/elements.h"
#include "parse/parser.h"
#include "parse/format.h"

line_parser parser;

// Parse options
const char *PO_strings[PO_NITEMS] = { "__DEFAULT__", "usequotes", "skipblanks", "stripbrackets", "__DUMMY__" };
parse_option PO_from_text(const char *s)
	{ return (parse_option) int(pow(2,enum_search("parse option",PO_NITEMS,PO_strings,s))); }

// Constructor
line_parser::line_parser()
{
	end_of_line = FALSE;
	nargs = 0;
	optmask = PO_DEFAULTS;
}

/*
// String parsing methods
*/

bool line_parser::get_next_arg(int destarg)
{
	// Get the next input chunk from the internal string and put into argument specified
	dbg_begin(DM_PARSE,"parser::get_next_arg");
	static int n, arglen;
	static bool done, hadquotes;
	static char c, quotechar, arg[MAXARGLENGTH];
	done = FALSE;
	hadquotes = FALSE;
	quotechar = '\0';
	end_of_line = FALSE;
	arglen = 0;
	for (n=0; n<line.length(); n++)
	{
		c = line[n];
		switch (c)
		{
			// End of line markers
			case (10):	// Line feed (\n)
			case (13):	// Carriage Return
				done = TRUE;
				end_of_line = TRUE;
				break;
			// Delimiters
			// If we encounter one and arg length != 0 this signals the end of the argument.
			case (9):	// Horizontal Tab
			case (' '):	// Space
			case (','):	// Comma
				if (quotechar != '\0')
				{
					arg[arglen] = c;
					arglen ++;
				}
				else if (arglen != 0) done = TRUE;
				break;
			// Quote marks
			// If PO_USEQUOTES, keep delimiters and other quote marks inside the quoted text.
			case (34):	// Double quotes
			case (39):	// Single quotes
				if (!(optmask&PO_USEQUOTES)) break;
				if (quotechar == '\0') quotechar = c;
				else if (quotechar == c)
				{
					quotechar = '\0';
					hadquotes = TRUE;
					done = TRUE;
				}
				else
				{
					arg[arglen] = c;
					arglen ++;
				}
				break;
			// Brackets
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optmask&PO_STRIPBRACKETS) break;
				arg[arglen] = c;
				arglen ++;
				break;
			// Comment markers
			case ('#'):	// "#" Rest/all of line is a comment
				end_of_line = TRUE;
				done = TRUE;
				break;
			// Normal character
			default: 
				arg[arglen] = c;
				arglen ++;
				break;
		}
		if (done) break;
	}
	arg[arglen] = '\0';
	if (n == line.length()) end_of_line = TRUE;
	// Store the result in the desired destination
	if (destarg == -1) temparg = arg;
	else arguments[destarg] = arg;
	// Strip off the characters up to position 'n', but not including position 'n' itself
	line.erasestart(n+1);
	//printf("Rest of line is now [%s]\n",line.get());
	dbg_end(DM_PARSE,"parser::get_next_arg");
	return (arglen == 0 ? (hadquotes ? TRUE : FALSE) : TRUE);
}

// Rip next n characters
bool line_parser::get_next_n(int length)
{
	// Put the next 'length' characters from source into temparg.
	dbg_begin(DM_PARSE,"parser::get_next_n");
	temparg.create_empty(length+1);
	int arglen = 0;
	char c;
	if (line.length() == 0)
	{
		dbg_end(DM_PARSE,"parser::get_next_n");
		return FALSE;
	}
	if (length > line.length()) length = line.length();
	for (int n=0; n<length; n++)
	{
		c = line[n];
		switch (c)
		{
			// Brackets
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optmask&PO_STRIPBRACKETS) break;
				temparg += c;
				arglen ++;
				break;
		//	case (32):      // Space - ignore to get left-justified data
		//		break;
			default:
				temparg += c;
				arglen ++;
				break;
		}
	}
	line.erasestart(length);
	dbg_end(DM_PARSE,"parser::get_next_n");
	return TRUE;
}

// Get all (delimited)
void line_parser::get_all_args_delim(dnchar &source)
{
	// Parse the string in 'source' into arguments in 'args'
	dbg_begin(DM_PARSE,"parser::get_all_args_delim[string]");
	nargs = 0; 
	for (int n=0; n<MAXARGS; n++) arguments[n].clear();
	end_of_line = FALSE;
	while (!end_of_line)
	{
		if (get_next_arg(nargs))
		{
			msg(DM_PARSE,"get_all_args_delim[string] arg=%i [%s]\n",nargs,argc(nargs));
			nargs++;
		}
	}
	dbg_end(DM_PARSE,"parser::get_all_args_delim[string]");
}

// Get all (formatted)
void line_parser::get_all_args_formatted(dnchar &source, format *fmt)
{
	// Parse the string in 'source' into arguments in 'args'
	dbg_begin(DM_PARSE,"parser::get_all_args_formatted");
	nargs = 0;
	bool parseresult;
	for (int n=0; n<MAXARGS; n++) arguments[n].clear();
	format_node *fn = fmt->get_nodes();
	while (fn != NULL)
	{
		// If field length specifier is zero, just get the next arg, otherwise get by length
		if (fn->get_length() == 0) parseresult = get_next_arg(-1);
		else parseresult = get_next_n(fn->get_length());
		if (!parseresult)
		{
			msg(DM_VERBOSE,"parser::get_all_args_formatted <<<< '%s' passed end of line >>>>\n",fn->get_variable()->get_name());
			fn->get_variable()->reset();
		}
		else fn->get_variable()->set(temparg.get());
		fn = fn->next;
	}
	dbg_end(DM_PARSE,"parser::get_all_args_formatted");
}

/*
// Delimited Parsing Routines
*/

// Parse delimited (from file)
int line_parser::get_args_delim(ifstream *xfile, int options)
{
	// Standard file parse routine.
	// Splits the line from the file into delimited arguments via the 'parseline' function
	dbg_begin(DM_PARSE,"parser::get_args[file]");
	bool done = FALSE;
	static char linefromfile[MAXLINELENGTH];
	// Lines beginning with '#' are ignored as comments
	// Blank lines are skipped if blankskip == TRUE.
	// Returns : 0=ok, 1=error, -1=eof
	optmask = options;
	do
	{
		xfile->getline(linefromfile,MAXLINELENGTH-1);
		if (xfile->eof())
		{
			xfile->close();
			dbg_end(DM_PARSE,"parser::get_args[file]");
			return -1;
		}
		if (xfile->fail())
		{
			xfile->close();
			dbg_end(DM_PARSE,"parser::get_args[file]");
			return 1;
		}
		line = linefromfile;
		// Assume that we will finish after parsing the line we just read in
		done = TRUE;
		// To check for blank lines, do the parsing and then check nargs()
		get_all_args_delim(line);
		if ((optmask&PO_SKIPBLANKS) && (nargs == 0)) done = FALSE;
	} while (!done);
	dbg_end(DM_PARSE,"parser::get_args[file]");
	return 0;
}

// Get next argument (delimited) from file stream
const char *line_parser::get_arg_delim(ifstream *xfile)
{
	dbg_begin(DM_PARSE,"parser::get_args[file]");
	static char result[512];
	static int length;
	static bool done;
	// Clear old result
	result[0] = '\0';
	length = 0;
	done = FALSE;
	*xfile >> result;
	return result;
}

// Parse all arguments (delimited) from string
void line_parser::get_args_delim(const char *s, int options)
{
	line = s;
	optmask = options;
	get_all_args_delim(line);
}

// Cut next delimited argument from supplied string
const char *line_parser::get_next_delim(dnchar &s, int options)
{
	static int result;
	optmask = options;
	line = s.get();
	result = get_next_arg(-1);
	s = line.get();
	return (result ? temparg.get() : "");
}

// Parse string into lines
void line_parser::get_lines_delim(const char *s)
{
	static int n, arglen;
	static bool done;
	static char c, quotechar, arg[MAXARGLENGTH];
	done = FALSE;
	quotechar = '\0';
	end_of_line = FALSE;
	nargs = 0;
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
				arguments[nargs] = arg;
				nargs ++;
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
		arguments[nargs] = arg;
		nargs ++;
	}
}

// Skip lines from file
int line_parser::skip_lines(ifstream *xfile, int nlines)
{
	dbg_begin(DM_PARSE,"parser::skip_lines");
	static char skipline[MAXLINELENGTH];
	for (int n=0; n<nlines; n++)
	{
		xfile->getline(skipline,MAXLINELENGTH-1);
		if (xfile->eof())
		{
			xfile->close();
			dbg_end(DM_PARSE,"parser::skip_lines");
			return -1;
		}
		if (xfile->fail())
		{
			xfile->close();
			dbg_end(DM_PARSE,"parser::skip_lines");
			return 1;
		}
	}
	dbg_end(DM_PARSE,"parser::skip_lines");
	return 0;
}

/*
// Formatted Parsing
*/

// Parse formatted (from file)
int line_parser::get_args_formatted(ifstream *xfile, int options, format *fmt)
{
	// Splits the line from the file into parts determiend by the supplied format
	dbg_begin(DM_PARSE,"parser::get_args_formatted[file]");
	static char linefromfile[MAXLINELENGTH];
	bool done = FALSE;
	// Lines beginning with '#' are ignored as comments
	// Blank lines are skipped if blankskip == TRUE.
	// Returns : 0=ok, 1=error, -1=eof
	optmask = options;
	do
	{
		xfile->getline(linefromfile,MAXLINELENGTH-1);
		if (xfile->eof())
		{
			xfile->close();
			dbg_end(DM_PARSE,"parser::get_args_formatted[file]");
			return -1;
		}
		if (xfile->fail())
		{
			xfile->close();
			dbg_end(DM_PARSE,"parser::get_args_formatted[file]");
			return 1;
		}
		line = linefromfile;
		// Assume that we will finish after parsing the line we just read in
		done = TRUE;
		// To check for blank lines, do the parsing and then check nargs()
		get_all_args_formatted(line,fmt);
		if ((optmask&PO_SKIPBLANKS) && (nargs == 0)) done = FALSE;
	} while (!done);
	dbg_end(DM_PARSE,"parser::get_args[file]");
	return 0;
}

// Parse formatted (from string)
void line_parser::get_args_formatted(const char *source, int options, format *fmt)
{
	// Splits the line from the file into parts determiend by the supplied format
	dbg_begin(DM_PARSE,"parser::get_args[string]");
	// Lines beginning with '#' are ignored as comments
	// Blank lines are skipped if blankskip == TRUE.
	// Returns : 0=ok, 1=error, -1=eof
	line = source;
	optmask = options;
	get_all_args_formatted(line,fmt);
	dbg_end(DM_PARSE,"parser::get_args[string]");
}

/*
// Atom type definition functions
*/

const char *line_parser::parse_atstring(dnchar &source)
{
	// Cut the next atomtype command from the supplied string. Put in 'dest', along with any bracketed
	// argument part. Use brackets a bit like quotes are used above, except we don't toggle the flag.
	// Ignore spaces and horizontal tabs. Commas separate commands.
	dbg_begin(DM_PARSE,"parser::parse_atstring");
	static int n, nchars, bracketlevel;
	static bool done, el_list;
	static dnchar typecmd;
	nchars = 0;
	bracketlevel = 0;
	el_list = FALSE;
	done = FALSE;
	typecmd.create_empty(source);
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
	//printf("Erasing in parse_atstring...\n");
	// ERASE now takes 'start' and 'end' (not 'count') as parameters.
	source.erasestart(n+1);
	//printf("Result = ");
	//typecmd.print();
	dbg_end(DM_PARSE,"parser::parse_atstring");
	return typecmd.get();
}

const char *line_parser::trim_atkeyword(dnchar &source)
{
	// Remove the keyword part of the command and put in 'dest', leaving the options (minus brackets)
	// in the original string. Remove '[' and ']' from keyword since this is only used to keep a list of elements together.
	dbg_begin(DM_PARSE,"parser::trim_atkeywd");
	static bool done, equals;
	static dnchar keywd;
	done = FALSE;
	equals = FALSE;
	//printf("String given to trim_atkeywd = '%s'\n",source.get());
	keywd.create_empty(source);
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
	if (equals) source.erasestart(n+1);
	else source.erasestart(n);
	//printf("RESULT in trim atkeywd = '%s'\n",dest.get());
	// Remove brackets if they're there
	if (source.length() != 0) 
		if (source[0] == '(')
		{
			source.erasestart(1);
			source.eraseend(1);
		}
	dbg_end(DM_PARSE,"parser::trim_atkeywd");
	return keywd.get();
}

// Parse tokens in numerical expression
bool line_parser::get_args_expression(const char *s)
{
	dbg_begin(DM_PARSE,"parser::get_args_expression");
	nargs = 0; 
	for (int n=0; n<MAXARGS; n++) arguments[n].clear();
	static int n, arglen;
	static bool result;
	static char arg[MAXARGLENGTH];
	static const char *c;
	result = TRUE;
	arglen = 0;
	for (c = &s[0]; *c != '\0'; c++)
	{
		switch (*c)
		{
			// Skip delimiters
			case (9):	// Horizontal Tab
			case (' '):	// Space
				break;
			// Expression tokens
			case ('+'):
			case ('-'):
			case ('/'):
			case ('*'):
			case ('^'):
			case ('%'):
				// No current other argument - must be error
				if (arglen == 0)
				{
					result = FALSE;
					break;
				}
				// Store current argument
				arg[arglen] = '\0';
				arguments[nargs] = arg;
				nargs++;
				arg[0] = *c;
				arg[1] = '\0';
				arguments[nargs] = arg;
				nargs++;
				arglen = 0;
				break;
			// Normal character
			default: 
				arg[arglen] = *c;
				arglen ++;
				break;
		}
	}
	// Store current argument
	if (arglen != 0)
	{
		arg[arglen] = '\0';
		arguments[nargs] = arg;
		nargs++;
	}
	//for (int i=0; i<nargs; i++) printf("EXPR ARG %i = [%s]\n",i,arguments[i].get());
	dbg_end(DM_PARSE,"parser::get_args_expression");
	return result;
}

// Return whether specified argument is an operator
bool line_parser::is_operator(int argno, char op)
{
	// Check length.
	if (arguments[argno].length() != 1) return FALSE;
	// Check character
	return (arguments[argno][0] == op ? TRUE : FALSE);
}
