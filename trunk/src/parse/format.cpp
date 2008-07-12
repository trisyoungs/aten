/*
	*** Line / variable formatting
	*** src/parse/format.cpp
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

#include "parse/variablelist.h"
#include "parse/format.h"
#include "parse/parser.h"
#include "base/sysfunc.h"
#include <cstring>

// Constructors
FormatNode::FormatNode()
{
	// Private variables
	variable_ = NULL;
	length_ = 0;
	precision_ = 0;
	zeroPadInteger_ = FALSE;

	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructors
Format::~Format()
{
	nodes_.clear();
}

// Get format node variable
Variable *FormatNode::variable()
{
	return variable_;
}

// Get field length
int FormatNode::length()
{
	return length_;
}

// Get field precision
int FormatNode::precision()
{
	return precision_;
}

// Return whether to pad integers with zeros
bool FormatNode::zeroPadInteger()
{
	return zeroPadInteger_;
}

// Returns first node
FormatNode* Format::nodes()
{
	return nodes_.first();
}

// Set format node
bool FormatNode::set(const char *s, VariableList &vlist)
{
	msg.enter("FormatNode::set");
	// Format of formatters is 'F%n.m': F = format quantity/variable, n.m = length,precision
	int pos1, pos2;
	static char specifier[512], len[32], pre[32];
	char *c;
	// 'Reset' strings
	specifier[0] = '\0';
	len[0] = '\0';
	pre[0] = '\0';
	// Everything up to the '%' character is the quantity / variable
	for (pos1 = 0; s[pos1] != '\0'; pos1++)
	{
		if (s[pos1] == '%') break;
		specifier[pos1] = s[pos1];
	}
	specifier[pos1] = '\0';
	if (s[pos1] == '%')
	{
		// Everything past the '%' character (and up to a '.') is the length...
		pos1 ++;
		for (pos2 = pos1; s[pos2] != '\0'; pos2++)
		{
			if (s[pos2] == '.') break;
			len[pos2-pos1] = s[pos2];
		}
		len[pos2-pos1] = '\0';
		// Lastly, if a decimal point exists then the last part is the precision
		if (s[pos2] == '.')
		{
			for (pos1=pos2+1; s[pos1] != '\0'; pos1++) pre[pos1-(pos2+1)] = s[pos1];
			pre[pos1-(pos2+1)] = '\0';
		}
	}
	msg.print(Messenger::Parse,"FormatNode::set : Parsed specifier[%s] length[%s] precision[%s]\n", specifier, len, pre);
	// If we're given a variable, check that is has been declared
	if (specifier[0] == '$')
	{
		c = specifier;
		c ++;
		variable_ = vlist.get(c);
		if (variable_ == NULL)
		{
			printf("Variable '%s' in format string has not been declared.\n", c);
			return FALSE;
		}
	}
	else if (specifier[0] == '*') variable_ = vlist.dummy();
	else variable_ = vlist.addConstant(specifier);
	// Store the data
	length_ = (len[0] == '\0' ? 0 : atoi(len));
	precision_ = (pre[0] == '\0' ? 0 : atoi(pre));
	// If the first character of len[0] is zero (or the first is '-' and the second is zero) set padding for integers to true
	if (len[0] == '0' || ((len[0] == '-') && (len[1] == '0'))) zeroPadInteger_ = TRUE;
	msg.exit("FormatNode::set");
	return TRUE;
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
			printf("Failed to add format node '%s'.\n", lp.argc(n));
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
				case ('['):
				case (']'):
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
			printf("Failed to add format node '%s'.\n", varstr);
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
			printf("Failed to add format node '%s'.\n", varstr);
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
const char *Format::createString()
{
	// Creates a formatted output string from the model supplied
	msg.enter("Format::createString");
	static char result[8096], bit[1024], fmt[16];
	static Variable *v;
	result[0] = '\0';
	bit[0] = '\0';
	fmt[0] = '\0';
	// Step through each formatting node, adding on the specified data from the model/atom
	for (FormatNode *fn = nodes_.first(); fn != NULL; fn = fn->next)
	{
		v = fn->variable();
		// For each format node in the list, check the type of the argument and create a relevant format node
		switch (v->type())
		{
			case (Variable::CharacterVariable):
				if (fn->length() == 0) strcpy(fmt,"%s");
				else sprintf(fmt,"%%%is",fn->length());
				sprintf(bit,fmt,v->asCharacter());
				break;
			case (Variable::IntegerVariable):
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
			case (Variable::FloatVariable):
				if (fn->length() == 0) strcpy(fmt,"%f");
				else sprintf(fmt,"%%%i.%if",fn->length(),fn->precision());
				sprintf(bit,fmt,v->asDouble());
				break;
			default:
				printf("Variables of type '%s' cannot be used in a format string.\n", Variable::variableType(v->type()));
		}
		msg.print(Messenger::Parse,"Format:::createString - added [%s], format [%s]\n", bit, fmt);
		strcat(result,bit);
	}
	msg.exit("Format::createString");
	return result;
}
