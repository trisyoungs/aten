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
	dbg_begin(DM_PARSE,"format_node::set");
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
	msg(DM_PARSE,"format::set : Parsed specifier[%s] length[%s] precision[%s]\n", specifier, len, pre);
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
	dbg_end(DM_PARSE,"format_node::set");
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
	static char text[512], varstr[512];
	int nchars = 0, vchars = 0, n;
	bool done;
	format_node *fn;
	// Clear any existing node list
	nodes.clear();
	n = 0;
	while (s[n] != '\0')
	{
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
			fn = nodes.add();
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
					done = TRUE;
					break;
				case (','):
				case ('('):
				case (')'):
				case (' '):
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
		fn = nodes.add();
		fn->set(varstr, vlist);
		if (!fn->set(varstr, vlist))
		{
			printf("Failed to add format node '%s'.\n", varstr);
			dbg_end(DM_PARSE,"format::create_exact");
			return FALSE;
		}
	}
	// Need to check here to see if vchars or nchars != 0
	if (nchars != 0)
	{
		text[nchars] = '\0';
		fn = nodes.add();
		fn->set(text, vlist);
	}
	else if (vchars != 0)
	{
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
	static variable *v;
	result[0] = '\0';
	// Step through each formatting node, adding on the specified data from the model/atom
	for (format_node *fn = nodes.first(); fn != NULL; fn = fn->next)
	{
		v = fn->get_variable();
		// For each format node in the list, check the type of the argument and create a relevant format node
		switch (v->get_type())
		{
			case (VT_CHAR):
				if (fn->get_length() == 0) strcpy(fmt,"%s");
				else sprintf(fmt,"%%-%is",fn->get_length());
				sprintf(bit,fmt,v->get_as_char());
				break;
			case (VT_INTEGER):
				if (fn->get_length() == 0) strcpy(fmt,"%i");
				else sprintf(fmt,"%%-%ii",fn->get_length());
				sprintf(bit,fmt,v->get_as_int());
				break;
			case (VT_DOUBLE):
				if (fn->get_length() == 0) strcpy(fmt,"%f");
				else sprintf(fmt,"%%-%i.%if",fn->get_length(),fn->get_precision());
				sprintf(bit,fmt,v->get_as_double());
				break;
			default:
				printf("Variables of type '%s' cannot be used in a format string.\n", text_from_VT(v->get_type()));
		}
		msg(DM_PARSE,"Format string is [%s] - value is [%s]\n", fmt, bit);
		strcat(result,bit);
	}
	dbg_end(DM_PARSE,"format::create_string");
	return result;
}
