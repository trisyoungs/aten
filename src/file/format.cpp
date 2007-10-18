/*
	*** Line / variable formatting
	*** src/file/format.cpp

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

#include "file/format.h"
#include "file/parse.h"
#include "base/sysfunc.h"

// Constructors
format_node::format_node()
{
	//type = FN_UNKNOWN;
	var = NULL;
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

// Create from string
void format::create(const char *fmt, variable_list &vlist)
{
	// Create nodes from a supplied formatting string.
	// If any formatters are unrecognised, assume that they are variables instead
	dbg_begin(DM_PARSE,"format::create");
	int n, m, pos1, pos2;
	static char arg[100], specifier[32], length[32], precision[32];
	format_node *fn;
	// First, parseline the formatting string
	parser.get_args_delim(fmt,PO_DEFAULTS);
	// Now, step through the args[] array and convert the substrings into format nodes
	for (n=0; n<parser.get_nargs(); n++)
	{
		// Format of formatters is 'F@n.m': F = format quantity/variable, n.m = length,precision
		strcpy(arg,parser.argc(n));
		for (m=0; m<32; m++)
		{
			specifier[m] = '\0';
			length[m] = '\0';
			precision[m] = '\0';
		}
		// Everything up to the '@' character is the quantity / variable
		for (pos1 = 0; arg[pos1] != '\0'; pos1++)
		{
			if (arg[pos1] == '@') break;
			specifier[pos1] = arg[pos1];
		}
		if (arg[pos1] == '@')
		{
			// Everything past the '@' character (and up to a '.') is the length...
			pos1 ++;
			for (pos2 = pos1; arg[pos2] != '\0'; pos2++)
			{
				if (arg[pos2] == '.') break;
				length[pos2-pos1] = arg[pos2];
			}
			// Lastly, if a decimal point exists then the last part is the precision
			if (arg[pos2] == '.') for (pos1=pos2+1; arg[pos1] != '\0'; pos1++) precision[pos1-(pos2+1)] = arg[pos1];
		}
		msg(DM_PARSE,"format::create : Parsed specifier[%s] length[%s] precision[%s]\n",specifier,length,precision);
		// Create our new format node and store the information in it
		fn = nodes.add();
		fn->set_variable(vlist.get(specifier));
		fn->set_length((length[0] == '\0' ? 0 : atoi(length)));
		fn->set_precision((precision[0] == '\0' ? 0 : atoi(precision)));
	}
	//if (nfailed != 0) msg(DM_NONE,"Warning : Format string contained %i unrecognised identifiers.\n",nfailed);
	dbg_end(DM_PARSE,"format::create");
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
