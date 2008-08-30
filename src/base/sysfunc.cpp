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

#include "base/constants.h"
#include <fstream>
#include <iostream>
#include <string.h>

using namespace std;

// Convert string to uppercase
const char *upperCase(const char *s)
{
	static char result[256];
	static int i;
	for (i = 0; s[i] != '\0'; i++) result[i] = toupper(s[i]);
	result[i] = '\0';
	return result;
}

// Convert string to lowercase
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


// Convert the number 'n' to a string representation.
const char *itoa(int n)
{
	static char result[30];
	sprintf(result,"%i",n);
	return result;
}

// Convert the real number 'f' to a string representation
const char *ftoa(double f)
{
	static char result[30];
	sprintf(result,"%f",f);
	return result;
}

// Convert the real number 'f' to a string representation with supplied format
const char *ftoa(double f,const char *fmt)
{
	static char result[30];
	sprintf(result,fmt,f);
	return result;
}

// Remove path in front of filename
const char *removePath(const char *s)
{
	const char *lastslash = strrchr(s,'/');
	lastslash == 0 ? lastslash = s : lastslash ++;
	return lastslash;
}

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

// Count number of times that supplied characters occur in supplied string
int countChars(const char *s, const char *chars, int offset)
{
	int total = 0, n, count = 0;
	const char *c;
	while (*s != '\0')
	{
		if (count >= offset)
		{
			for (n=0; chars[n] != '\0'; n++) if (chars[n] == *s) total ++;
		}
		s++;
		count++;
	}
	return total;
}

// Return whether file exists
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
