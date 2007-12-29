/*
	*** Line / variable formatting
	*** src/parse/format.cpp
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

#include "parse/format.h"
#include "parse/parser.h"
#include "base/sysfunc.h"

// Constructors
format_node::format_node()
{
	v = NULL;
	length = 0;
	precision = 0;
	next = NULL;
	prev = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_FORMAT_NODE] ++;
	#endif
}

format::format()
{
	#ifdef MEMDEBUG
		memdbg.create[MD_FORMAT] ++;
	#endif
}

// Destructors
format::~format()
{
	nodes.clear();
	#ifdef MEMDEBUG
		memdbg.destroy[MD_FORMAT] ++;
	#endif
}

format_node::~format_node()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_FORMAT_NODE] ++;
	#endif
}

// Set format node
bool format_node::set(const char *s, variable_list &vlist)
{
	dbg_begin(DM_PARSE,"format_node::create");
	// Format of formatters is 'F@n.m': F = format quantity/variable, n.m = length,precision
	int m, pos1, pos2;
	static char specifier[32], len[32], pre[32];
	char *c;
	for (m=0; m<32; m++)
	{
		specifier[m] = '\0';
		len[m] = '\0';
		pre[m] = '\0';
	}
	// Everything up to the '@' character is the quantity / variable
	for (pos1 = 0; s[pos1] != '\0'; pos1++)
	{
		if (s[pos1] == '@') break;
		specifier[pos1] = s[pos1];
	}
	if (s[pos1] == '@')
	{
		// Everything past the '@' character (and up to a '.') is the length...
		pos1 ++;
		for (pos2 = pos1; s[pos2] != '\0'; pos2++)
		{
			if (s[pos2] == '.') break;
			len[pos2-pos1] = s[pos2];
		}
		// Lastly, if a decimal point exists then the last part is the precision
		if (s[pos2] == '.') for (pos1=pos2+1; s[pos1] != '\0'; pos1++) pre[pos1-(pos2+1)] = s[pos1];
	}
	msg(DM_PARSE,"format::create : Parsed specifier[%s] length[%s] precision[%s]\n", specifier, len, pre);
	// If we're given a variable, check that is has been declared
	if (specifier[0] == '$')
	{
		c = specifier;
		c ++;
		v = vlist.get(c);
		if (v == NULL)
		{
			printf("Variable '%s' in format string has not been declared.\n", c);
			return FALSE;
		}
	}
	else if (specifier[0] == '*') v = vlist.get_dummy();
	else v = vlist.add_constant(specifier);
	// Store the data
	length = (len[0] == '\0' ? 0 : atoi(len));
	precision = (pre[0] == '\0' ? 0 : atoi(pre));
	dbg_end(DM_PARSE,"format_node::create");
	return TRUE;
}

// Create using delimited arguments
bool format::create_delimited(const char *s, variable_list &vlist)
{
	dbg_begin(DM_PARSE,"format::create_delimited");
	int n;
	format_node *fn;
	static line_parser lp;
	// Clear any existing node list
	nodes.clear();
	// First, parseline the formatting string
	lp.get_args_delim(s,PO_DEFAULTS);
	// Now, step through the args[] array and convert the substrings into format nodes
	for (n=0; n<lp.get_nargs(); n++)
	{
		// Create our new format node and store the information in it
		fn = nodes.add();
		if (!fn->set(lp.argc(n), vlist))
		{
			printf("Failed to add format node '%s'.\n", lp.argc(n));
			dbg_end(DM_PARSE,"format::create");
			return FALSE;
		}
	}
	//if (nfailed != 0) msg(DM_NONE,"Warning : Format string contained %i unrecognised identifiers.\n",nfailed);
	dbg_end(DM_PARSE,"format::create");
	return TRUE;
}

// Create using character-by-character method
bool format::create_exact(const char *s, variable_list &vlist)
{
	dbg_begin(DM_PARSE,"format::create_exact");
	// Go through supplied string, converting variables as we go
	static char srcstr[512], text[512], varstr[512];
	int nchars = 0, vchars = 0;
	bool done;
	char *c;
	format_node *fn;
	// Clear any existing node list
	nodes.clear();
	strcpy(srcstr,s);
	for (c = srcstr; *c != '\0'; c++)
	{
		// If the character is not '$', just add it to 'text' and continue
		if (*c != '$')
		{
			text[nchars] = *c;
			nchars ++;
			continue;
		}
		// This is the start of a variable format. Add 'text' node if not empty...
		if (nchars != 0)
		{
			text[nchars] = '\0';
			fn = nodes.add();
			fn->set(text, vlist);
			nchars = 0;
		}
		// Clear the variable string and start adding characters
		// Add characters to 'varstr' until we find the end of the format
		vchars = 1;
		varstr[0] = '$';
		c ++;
		done = FALSE;
		while (*c != '\0')
		{
			switch (*c)
			{
				case ('{'):
					c++;
					break;
				case ('\0'):
					c--;
				case ('}'):
					done = TRUE;
					break;
				case (','):
				case ('('):
				case (')'):
				case (' '):
					done = TRUE;
					c--;
					break;
				default:
					varstr[vchars] += *c;
					vchars ++;
					c++;
					break;
			}
			if (done) break;
		}
		// Now have variable (and format) in 'var'
		varstr[vchars] = '\0';
		fn = nodes.add();
		fn->set(varstr, vlist);
		if (!fn->set(varstr, vlist))
		{
			printf("Failed to add format node '%s'.\n", varstr);
			dbg_end(DM_PARSE,"format::create_exact");
			return FALSE;
		}
	}
	dbg_end(DM_PARSE,"format::create_exact");
	return TRUE;
}

// Create string
const char *format::create_string()
{
	// Creates a formatted output string from the model supplied
	dbg_begin(DM_PARSE,"format::create_string");
	static char result[8096], bit[1024], fmt[16];
	result[0] = '\0';
	// Step through each formatting node, adding on the specified data from the model/atom
	format_node *fn = nodes.first();
	while (fn != NULL)
	{
		// First, create format string for sprintf
		//fmt.clear();
		if (fn->get_length() == 0) strcpy(bit,fn->get_variable()->get_as_char());
		else
		{
			// Need to create formatter
			// If a precision has been specified, assume its an real.
			if (fn->get_precision() != 0)
			{
				strcpy(fmt,"%");
				strcat(fmt,itoa(fn->get_length()));
				strcat(fmt,".");
				strcat(fmt,itoa(fn->get_precision()));
				strcat(fmt,"f");
				sprintf(bit,fmt,fn->get_variable()->get_as_double());
			}
			else
			{
				strcpy(fmt,"%-");
				strcat(fmt,itoa(fn->get_length()));
				strcat(fmt,"s");
				sprintf(bit,fmt,fn->get_variable()->get_as_char());
			}
			msg(DM_PARSE,"Format string is [%s] - char value is [%s]\n",fmt,fn->get_variable()->get_as_char());
		}
		strcat(result,bit);
		fn = fn->next;
	}
	dbg_end(DM_PARSE,"format::create_string");
	return result;
}
