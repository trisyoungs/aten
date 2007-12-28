/*
	*** System functions
	*** src/base/sysfunc.cpp
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

#include <iostream>
#include <sstream>
#include "base/constants.h"
#include "parse/parse.h"
#include "base/debug.h"
#include "classes/variables.h"
#include <math.h>

const char *upper_case(const char *s)
{
	static char result[256];
	static int i;
	for (i=0; s[i] != '\0'; i++) result[i] = toupper(s[i]);
	result[i] = '\0';
	return result;
}

const char *lower_case(const char *s)
{
	static char result[256];
	static int i;
	for (i=0; s[i] != '\0'; i++) result[i] = tolower(s[i]);
	result[i] = '\0';
	return result;
}

// Get characters before comma
const char *get_before_comma(const char *s)
{
	static char result[128];
	static int i, count;
	count = 0;
	for (i = 0; s[i] != '\0'; i++)
	{
		if (s[i] == ',') break;
		result[count] = s[i];
		count ++;
	}
	result[count] = '\0';
	return result;
}

// Get characters after comma
const char *get_after_comma(const char *s)
{
	static char result[128];
	static int i, count;
	static bool foundcomma;
	foundcomma = FALSE;
	count = 0;
	for (i = 0; s[i] != '\0'; i++)
	{
		if (s[i] == ',') foundcomma = TRUE;
		else if (foundcomma)
		{
			result[count] = s[i];
			count ++;
		}
	}
	result[count] = '\0';
	return result;
}

// Search enum list for text
int enum_search(const char *name, int maxn, const char **itemlist, const char *query)
{
	static char lowerq[50], lowers[50];
	int result = maxn, i;
	strcpy(lowerq,lower_case(query));
	for (i=0; i<maxn; i++)
	{
		strcpy(lowers,lower_case(itemlist[i]));
		if (strcmp(lowerq,lowers) == 0)
		{
			result = i;
			break;
		}
	}
	if ((result == maxn) && (name[0] != '#')) printf("Unrecognised %s '%s'\n",name,query);
	return result;
}

// Search enum list for text
int enum_search_data(const char *name, int maxn, const char **itemlist, const char *query)
{
	static char lowerq[50], lowers[50];
	int result = maxn, i;
	strcpy(lowerq,lower_case(query));
	for (i=0; i<maxn; i++)
	{
		strcpy(lowers,lower_case(get_before_comma(itemlist[i])));
		if (strcmp(lowerq,lowers) == 0)
		{
			result = i;
			break;
		}
	}
	if ((result == maxn) && (name[0] != '#')) printf("Unrecognised %s '%s'\n",name,query);
	return result;
}

const char *itoa(int n)
{
	// Convert the number 'n' to a string representation.
	static char result[30];
	sprintf(result,"%i",n);
	return result;
}

const char *ftoa(double f)
{
	// Convert the real number 'f' to a string representation
	static char result[30];
	sprintf(result,"%f",f);
	return result;
}

const char *ftoa(double f,const char *fmt)
{
	// Convert the real number 'f' to a string representation
	static char result[30];
	sprintf(result,fmt,f);
	return result;
}

const char *remove_path(const char *s)
{
	const char *lastslash = strrchr(s,'/');
	lastslash == 0 ? lastslash = s : lastslash ++;
	return lastslash;
}

// Search for string in delimited string list
int sinlist(const char *search, const char *list)
{
	dbg_begin(DM_PARSE,"sinlist");
	static int cpos, count, result;
	static dnchar arg, arglist;
	arglist.set(list);
	count = 0;
	result = -1;
	// Find position of next comma and cut that part of the string
	while (!arglist.empty())
	{
		//printf("Size of arglist = %li\n",arglist.length());
		cpos = arglist.find(',');
		if (cpos != -1)
		{
			arglist.cutstart(cpos+1,arg);
			arg.eraseend(1);
		}
		else
		{
			arg.set(arglist.get());
			arglist.clear();
		}
		if (arg == search)
		{
			msg(DM_PARSE,"sinlist [%s->%s==%s]\n",list,arg.get(),search);
			result = count;
			break;
		}
		else msg(DM_PARSE,"sinlist [%s->%s<>%s]\n",list,arg.get(),search);
		count ++;
	}
	dbg_end(DM_PARSE,"sinlist");
	return result;
}

// Evaluate numerical expression and return simplified result
// This is a recursive routine, to be called on sub-expressions (i.e. bracketed parts)
char operators[] = "^*/%+-";
const char *evaluate(const char *s, variable_list *vars)
{
	int lbracket, rbracket, n, m, leftarg, rightarg;
	char scopy[128], substr[128], subresult[128], arg[128];
	char *c;
	static double a, b, x;
	static bool isop;
	variable *v;
	// Grab original string and work on scopy from now on
	strcpy(scopy,s);
	// Resolve brackets into results
	do
	{
		lbracket = -1;
		rbracket = -1;
		n=0;
		// Store position of each '(' we find, and break when we get the first ')'
		for (c = scopy; *c != '\0'; c++)
		{
			if (*c == '(') lbracket = n;
			if (*c == ')')
			{
				rbracket = n;
				break;
			}
			n++;
		}
		// Check for mis-matched brackets
		if (((lbracket == -1) && (rbracket != -1)) || ((lbracket != -1) && (rbracket == -1)))
		{
			printf("evaluate - Mis-matched brackets in expression.\n");
			return "0";
		}
		// Get substring and evaluate it
		m = rbracket-lbracket;
		//printf("Bracketpos = %i %i len=%i\n",lbracket,rbracket, m);
		// If we've found brackets, recursively evaluate the sub expression first
		if ((lbracket != -1) && (rbracket != -1))
		{
			strncpy(substr, &scopy[lbracket+1], m);
			substr[(rbracket-lbracket)-1] = '\0';
			//printf("SUBSTRING to evaluate = %s\n",substr);
			strcpy(subresult, evaluate(substr, vars));
			//printf("RESULT of SUBSTRING EVALUATE = %s\n",subresult);
			// Grab part of expression before left bracket
			strncpy(substr, scopy, lbracket);
			substr[lbracket] = '\0';
			strcat(substr, subresult);
			strcat(substr, &scopy[rbracket+1]);
			// Copy final string back to scopy
			strcpy(scopy, substr);
		}
	} while ((lbracket != -1) && (rbracket != -1));
	// Evaluate now bracketless expression
	// Parse the string into operators and values
	if (!parser.get_args_expression(scopy))
	{
		printf("evaluate - Error parsing expression.\n");
		return "0";
	}
	// Cycle over operators (in precedence) and replace with evaluated result.
	// Store position of last non-blank argument we encounter to use with token
	for (c = operators; *c != '\0'; c++)
	{
		leftarg = -1;
		for (n=0; n<parser.get_nargs()-1; n++)
		{
			isop = parser.is_operator(n, *c);
			if ((!parser.is_blank(n)) && (!isop)) leftarg = n;
			if (!isop) continue;
			// Find next non-blank argument
			rightarg = -1;
			for (m=n+1; m<parser.get_nargs(); m++)
				if (!parser.is_blank(m))
				{
					rightarg = m;
					break;
				}
			// Check argument availability
			if ((leftarg != -1) && (rightarg != -1))
			{
				//printf("left / right args %i %i\n",leftarg,rightarg);
				// If either argument is a variable, grab its value
				strcpy(arg,parser.argc(leftarg));
				if (arg[0] == '$')
				{
					v = vars->get(&arg[1]);
					a = (v == NULL ? 0.0 : v->get_as_double());
				}
				else a = atof(arg);
				strcpy(arg,parser.argc(rightarg));
				if (arg[0] == '$')
				{
					v = vars->get(&arg[1]);
					b = (v == NULL ? 0.0 : v->get_as_double());
				}
				else b = atof(arg);
				switch (*c)
				{
					case ('^'):
						x = pow(a,b);
						break;
					case ('*'):
						x = a * b;
						break;
					case ('/'):
						x = a / b;
						break;
					case ('%'):
						x = int(a)%(int(b));
						break;
					case ('+'):
						x = a + b;
						break;
					case ('-'):
						x = a - b;
						break;
				}
				// Store result and blank arguments
				parser.set_arg(n,ftoa(x));
				parser.set_arg(leftarg,"");
				parser.set_arg(rightarg,"");
				// Set new leftmost and rightmost arguments
				leftarg = n;
				rightarg = -1;
			}
			else
			{
				printf("evaluate - Operator was missing an argument.\n");
				return "0";
			} 
		}
	}
	// Look for non-blank argument and set it as result
	for (n=0; n<parser.get_nargs(); n++)
		if (!parser.is_blank(n)) return parser.argc(n);
}

// Strip trailing whitespace from string
const char *strip_trailing(const char *s)
{
	int len, n;
	static char result[512];
	len = strlen(s);
	// Go backwards through string and find first non-whitespace character
	for (n=len-1; n>=0; n--) if (s[n] != ' ') break;
	strncpy(result,s,n+1);
	result[n+1] = '\0';
	return result;
}
