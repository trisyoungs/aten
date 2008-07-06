/*
	*** System functions
	*** src/base/sysfunc.cpp
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

#include <iostream>
#include <sstream>
#include "base/constants.h"
#include "parse/parser.h"
#include "base/messenger.h"
#include "classes/variablelist.h"
#include <math.h>
#include <string.h>

const char *upperCase(const char *s)
{
	static char result[256];
	static int i;
	for (i = 0; s[i] != '\0'; i++) result[i] = toupper(s[i]);
	result[i] = '\0';
	return result;
}

const char *lowerCase(const char *s)
{
	static char result[256];
	static int i;
	for (i = 0; s[i] != '\0'; i++) result[i] = tolower(s[i]);
	result[i] = '\0';
	return result;
}

// Get characters before designated character
const char *beforeChar(const char *s, char delim)
{
	static char result[128];
	static int i, count;
	count = 0;
	for (i = 0; s[i] != '\0'; i++)
	{
		if (s[i] == delim) break;
		result[count] = s[i];
		count ++;
	}
	result[count] = '\0';
	return result;
}

// Get characters after designated character
const char *afterChar(const char *s, char delim)
{
	static char result[128];
	static int i, count;
	static bool foundcomma;
	foundcomma = FALSE;
	count = 0;
	for (i = 0; s[i] != '\0'; i++)
	{
		if (s[i] == delim) foundcomma = TRUE;
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
int enumSearch(const char *name, int maxn, const char **itemlist, const char *query)
{
	static char lowerq[50], lowers[50];
	int result = maxn, i;
	strcpy(lowerq,lowerCase(query));
	for (i=0; i<maxn; i++)
	{
		strcpy(lowers,lowerCase(itemlist[i]));
		if (strcmp(lowerq,lowers) == 0)
		{
			result = i;
			break;
		}
	}
	if ((result == maxn) && (name[0] != '\0')) printf("Unrecognised %s '%s'\n",name,query);
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

const char *removePath(const char *s)
{
	const char *lastslash = strrchr(s,'/');
	lastslash == 0 ? lastslash = s : lastslash ++;
	return lastslash;
}

// // Evaluate numerical expression and return simplified result
// // This is a recursive routine, to be called on sub-expressions (i.e. bracketed parts)
// char operators[] = "^*/%+-";
// const char *evaluate(const char *s, VariableList *vars)
// {
// 	int lbracket, rbracket, n, m, leftarg, rightarg;
// 	char scopy[128], substr[128], subresult[128], arg[128];
// 	char *c;
// 	static double a, b, x;
// 	static bool isop;
// 	Variable *v;
// 	// Grab original string and work on scopy from now on
// 	strcpy(scopy,s);
// 	// Resolve brackets into results
// 	do
// 	{
// 		lbracket = -1;
// 		rbracket = -1;
// 		n=0;
// 		// Store position of each '(' we find, and break when we get the first ')'
// 		for (c = scopy; *c != '\0'; c++)
// 		{
// 			if (*c == '(') lbracket = n;
// 			if (*c == ')')
// 			{
// 				rbracket = n;
// 				break;
// 			}
// 			n++;
// 		}
// 		// Check for mis-matched brackets
// 		if (((lbracket == -1) && (rbracket != -1)) || ((lbracket != -1) && (rbracket == -1)))
// 		{
// 			printf("evaluate - Mis-matched brackets in expression.\n");
// 			return "0";
// 		}
// 		// Get substring and evaluate it
// 		m = rbracket-lbracket;
// 		//printf("Bracketpos = %i %i len=%i\n",lbracket,rbracket, m);
// 		// If we've found brackets, recursively evaluate the sub expression first
// 		if ((lbracket != -1) && (rbracket != -1))
// 		{
// 			strncpy(substr, &scopy[lbracket+1], m);
// 			substr[(rbracket-lbracket)-1] = '\0';
// 			//printf("SUBSTRING to evaluate = %s\n",substr);
// 			strcpy(subresult, evaluate(substr, vars));
// 			//printf("RESULT of SUBSTRING EVALUATE = %s\n",subresult);
// 			// Grab part of expression before left bracket
// 			strncpy(substr, scopy, lbracket);
// 			substr[lbracket] = '\0';
// 			strcat(substr, subresult);
// 			strcat(substr, &scopy[rbracket+1]);
// 			// Copy final string back to scopy
// 			strcpy(scopy, substr);
// 		}
// 	} while ((lbracket != -1) && (rbracket != -1));
// 	// Evaluate now bracketless expression
// 	// Parse the string into operators and values
// //	if (!parser.getArgsExpression(scopy))
// //	{
// //		printf("evaluate - Error parsing expression.\n");
// //		return "0";
// //	}
// 	// Cycle over operators (in precedence) and replace with evaluated result.
// 	// Store position of last non-blank argument we encounter to use with token
// 	for (c = operators; *c != '\0'; c++)
// 	{
// 		leftarg = -1;
// 		for (n=0; n<parser.nArgs()-1; n++)
// 		{
// //			isop = parser.isOperator(n, *c);
// 			if ((!parser.isBlank(n)) && (!isop)) leftarg = n;
// 			if (!isop) continue;
// 			// Find next non-blank argument
// 			rightarg = -1;
// 			for (m=n+1; m<parser.nArgs(); m++)
// 				if (!parser.isBlank(m))
// 				{
// 					rightarg = m;
// 					break;
// 				}
// 			// Check argument availability
// 			if ((leftarg != -1) && (rightarg != -1))
// 			{
// 				//printf("left / right args %i %i\n",leftarg,rightarg);
// 				// If either argument is a variable, grab its value
// 				strcpy(arg,parser.argc(leftarg));
// 				if (arg[0] == '$')
// 				{
// 					v = vars->get(&arg[1]);
// 					a = (v == NULL ? 0.0 : v->asDouble());
// 					if (v == NULL) msg.print("Warning: Unrecognised variable '%s' in expression - using zero...\n",&arg[1]);
// 				}
// 				else a = atof(arg);
// 				strcpy(arg,parser.argc(rightarg));
// 				if (arg[0] == '$')
// 				{
// 					v = vars->get(&arg[1]);
// 					b = (v == NULL ? 0.0 : v->asDouble());
// 					if (v == NULL) msg.print("Warning: Unrecognised variable '%s' in expression - using zero...\n",&arg[1]);
// 				}
// 				else b = atof(arg);
// 				switch (*c)
// 				{
// 					case ('^'):
// 						x = pow(a,b);
// 						break;
// 					case ('*'):
// 						x = a * b;
// 						break;
// 					case ('/'):
// 						x = a / b;
// 						break;
// 					case ('%'):
// 						x = int(a)%(int(b));
// 						break;
// 					case ('+'):
// 						x = a + b;
// 						break;
// 					case ('-'):
// 						x = a - b;
// 						break;
// 				}
// 				// Store result and blank arguments
// 				parser.setArg(n,ftoa(x));
// 				parser.setArg(leftarg,"");
// 				parser.setArg(rightarg,"");
// 				// Set new leftmost and rightmost arguments
// 				leftarg = n;
// 				rightarg = -1;
// 			}
// 			else
// 			{
// 				printf("evaluate - Operator was missing an argument.\n");
// 				return "0";
// 			} 
// 		}
// 	}
// 	// Look for non-blank argument and set it as result
// 	for (n=0; n<parser.nArgs(); n++) if (!parser.isBlank(n)) break;
// 	if (n < parser.nArgs()) return parser.argc(n);
// 	// Failed to find a return value!
// 	printf("evaluate - failed to find return argument.\n");
// 	return ".0";
// }

// Strip trailing whitespace from string
const char *stripTrailing(const char *s)
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

bool fileExists(const char *filename)
{
	fstream f(filename,ios::in);
	if (f.good())
	{
		f.close();
		return TRUE;
	}
	else return FALSE;
}
