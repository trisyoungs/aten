/*
	*** Line / variable formatter
	*** src/command/format.cpp
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

#include "command/format.h"
#include "base/messenger.h"
#include "base/parser.h"
#include <string.h>

// Static variables
char Format::createdString_[8096];

// Returns first node in format
FormatNode* Format::nodes()
{
	return nodes_.first();
}

// Create using delimited arguments
bool Format::createDelimited(const char *s, VariableList &vlist)
{
	msg.enter("Format::createDelimited");
	int n;
	FormatNode *fn;
	static Parser lp;
	// Clear any existing node list
	nodes_.clear();
	// First, parseline the formatting string
	lp.getArgsDelim(s,Parser::Defaults);
	// Now, step through the args[] array and convert the substrings into format nodes
	for (n=0; n<lp.nArgs(); n++)
	{
		// Create our new format node and store the information in it
		fn = nodes_.add();
		if (!fn->set(lp.argc(n), vlist))
		{
			msg.print("Failed to add format node '%s'.\n", lp.argc(n));
			msg.exit("Format::createDelimited");
			return FALSE;
		}
	}
	//if (nfailed != 0) msg.print("Warning : Format string contained %i unrecognised identifiers.\n",nfailed);
	msg.exit("Format::createDelimited");
	return TRUE;
}

// Create using character-by-character method
bool Format::createExact(const char *s, VariableList &vlist)
{
	msg.enter("Format::createExact");
	// Go through supplied string, converting variables as we go
	static char text[512], varstr[512];
	int nchars = 0, vchars = 0, n;
	bool done;
	FormatNode *fn;
	// Clear any existing node list
	nodes_.clear();
	n = 0;
	while (s[n] != '\0')
	{
		//printf("Current s[n] = '%c'\n",s[n]);
		// Special check for '\' - add next character whatever it is
		if (s[n] == 92)
		{
			text[nchars] = s[n+1];
			nchars ++;
			n += 2;
			continue;
		}
		// If the character is not '$', just add it to 'text' and continue
		if (s[n] != '$')
		{
			text[nchars] = s[n];
			nchars ++;
			n++;
			continue;
		}
		// This is the start of a variable format. Add 'text' node if not empty...
		if (nchars != 0)
		{
			text[nchars] = '\0';
			//printf("Found variable - adding previous text '%s'...\n",text);
			fn = nodes_.add();
			fn->set(text, vlist);
			nchars = 0;
		}
		// Clear the variable string and start adding characters
		// Add characters to 'varstr' until we find the end of the format
		vchars = 1;
		varstr[0] = '$';
		n++;
		done = FALSE;
		while (s[n] != '\0')
		{
			switch (s[n])
			{
				case ('{'):
					n++;
					break;
				case ('}'):
					n++;
				case ('\0'):
				case ('$'):
					done = TRUE;
					break;
				case (','):
				case ('('):
				case (')'):
				case (' '):
// 				case ('['):
// 				case (']'):
				case (34):	// Double quotes
				case (39):	// Single quote
					done = TRUE;
					break;
				default:
					varstr[vchars] = s[n];
					vchars ++;
					n++;
					break;
			}
			if (done) break;
		}
		// Now have variable (and format) in 'var'
		varstr[vchars] = '\0';
		//printf("...the variable after which is '%s'.\n",varstr);
		fn = nodes_.add();
		if (!fn->set(varstr, vlist))
		{
			msg.print("Failed to add format node '%s'.\n", varstr);
			msg.exit("Format::createExact");
			return FALSE;
		}
		// Reset vchars for the next cycle
		vchars = 0;
	}
	// Need to check here to see if vchars or nchars != 0
	if (nchars != 0)
	{
		text[nchars] = '\0';
		//printf("Loop has ended - remaining text is '%s'.\n",text);
		fn = nodes_.add();
		fn->set(text, vlist);
	}
	else if (vchars != 0)
	{
		varstr[vchars] = '\0';
		//printf("Loop has ended - remaining variable is '%s'.\n",varstr);
		fn = nodes_.add();
		fn->set(varstr, vlist);
		if (!fn->set(varstr, vlist))
		{
			msg.print("Failed to add format node '%s'.\n", varstr);
			msg.exit("Format::createExact");
			return FALSE;
		}
	}	
	msg.exit("Format::createExact");
	return TRUE;
}

// Create format nodes from a supplied formatting string
bool Format::create(const char *s, VariableList &vars, bool delimited)
{
	return (delimited ? createDelimited(s, vars) : createExact(s, vars));
}

// Create string
bool Format::createString()
{
	// Creates a formatted output string from the model supplied
	msg.enter("Format::createString");
	static char bit[1024], fmt[16];
	static Variable *v;
	bool result = TRUE;
	createdString_[0] = '\0';
	bit[0] = '\0';
	fmt[0] = '\0';
	// Step through each formatting node, adding on the specified data from the model/atom
	for (FormatNode *fn = nodes_.first(); fn != NULL; fn = fn->next)
	{
		v = fn->variable();
		// For each format node in the list, check the type of the argument and create a relevant format node
		switch (v->type())
		{
			case (VTypes::CharacterData):
				if (fn->length() == 0) strcpy(fmt,"%s");
				else sprintf(fmt,"%%%is",fn->length());
				sprintf(bit,fmt,v->asCharacter());
				break;
			case (VTypes::IntegerData):
				if (fn->length() == 0) strcpy(fmt,"%i");
				else
				{
					if (fn->zeroPadInteger())
					{
						if (fn->length() < 0) sprintf(fmt,"%%-0%ii",abs(fn->length()));
						else sprintf(fmt,"%%0%ii",fn->length());
					}
					else sprintf(fmt,"%%%ii",fn->length());
				}
				sprintf(bit,fmt,v->asInteger());
				break;
			case (VTypes::RealData):
				if (fn->length() == 0) strcpy(fmt,"%f");
				else sprintf(fmt,"%%%i.%if",fn->length(),fn->precision());
				sprintf(bit,fmt,v->asDouble());
				break;
			default:
				msg.print("Variables of type '%s' cannot be used in a format string.\n", VTypes::dataType(v->type()));
				result = FALSE;
				break;
		}
		msg.print(Messenger::Parse,"Format:::createString - added [%s], format [%s]\n", bit, fmt);
		strcat(createdString_,bit);
	}
	msg.exit("Format::createString");
	return result;
}

// Return created string
const char *Format::createdString()
{
	return createdString_;
}

/*
// Formatted Parsing
*/

// Get all (formatted)
void Format::getAllArgsFormatted()
{
	// Parse the string in the sub-classed Parser's line into arguments in 'args'
	msg.enter("Format::getAllArgsFormatted");
	nArgs_ = 0;
	bool parseresult;
	
	for (FormatNode *fn = nodes_.first(); fn != NULL; fn = fn->next)
	{
		// If field length specifier is zero, just get the next arg, otherwise get by length
		if (fn->length() == 0) parseresult = getNextArg(-1);
		else parseresult = getNextN(fn->length());
		if (!parseresult)
		{
			msg.print(Messenger::Verbose,"Format::getAllArgsFormatted <<<< '%s' passed end of line >>>>\n", fn->variable()->name());
			fn->variable()->set("");
		}
		else
		{
			fn->variable()->set(tempArg_);
			nArgs_ ++;
		}
// 		printf("Variable %s now has value '%s'\n",fn->variable()->name(), fn->variable()->asCharacter());
	}
	msg.exit("Format::getAllArgsFormatted");
}

// Parse formatted (from file)
int Format::getArgsFormatted(ifstream *xfile, int options)
{
	// Splits the line from the file into parts determined by the supplied format
	msg.enter("Format::getArgsFormatted[file]");
	int result;
	bool done = FALSE;
	optionMask_ = options;
	nArgs_ = 0;
	do
	{
		result = readLine(xfile);
		if (result != 0)
		{
			msg.exit("Format::getArgsFormatted[file]");
			return result;
		}
		// Assume that we will finish after parsing the line we just read in
		done = TRUE;
		// To check for blank lines, do the parsing and then check nargs()
		getAllArgsFormatted();
		if ((optionMask_&Parser::SkipBlanks) && (nArgs_ == 0)) done = FALSE;
	} while (!done);
	msg.exit("Format::getArgsFormatted[file]");
	return 0;
}

// Parse formatted (from string)
void Format::getArgsFormatted(const char *source, int options)
{
	// Splits the line from the file into parts determiend by the supplied format
	msg.enter("Format::getArgsFormatted[string]");
	strcpy(line_, source);
	linePos_ = 0;
	lineLength_ = strlen(line_);
	optionMask_ = options;
	getAllArgsFormatted();
	msg.exit("Format::getArgsFormatted[string]");
}
