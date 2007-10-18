/*
	*** System functions
	*** src/base/sysfunc.cpp

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
#include "file/parse.h"
#include "base/debug.h"

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
	if ((result == maxn) && (strcmp(name,"NULL") != 0)) printf("Unrecognised %s '%s'\n",name,query);
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
	if ((result == maxn) && (strcmp(name,"NULL") != 0)) printf("Unrecognised %s '%s'\n",name,query);
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
