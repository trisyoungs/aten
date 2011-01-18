/*
	*** System functions
	*** src/base/sysfunc.cpp
	Copyright T. Youngs 2007-2010

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
#include "base/dnchar.h"
#include "base/messenger.h"
#include <fstream>
#include <iostream>
#include <string.h>
#include <QtCore/QFileInfo>

using namespace std;

// Return full, absolute path of file
const char *filePath(const char *s)
{
	QFileInfo info(s);
	return qPrintable(info.absoluteFilePath());
}

// Return full, absolute filename
const char *absoluteFilePath(const char *s)
{
	QFileInfo info(s);
	return qPrintable(info.absoluteFilePath());
}

// Remove path in front of filename
const char *removePath(const char *s)
{
	QFileInfo info(s);
	return qPrintable(info.fileName());
}

// Convert string to uppercase
const char *upperCase(const char *s)
{
	static Dnchar result(1024);
	result.clear();
	for (int i = 0; s[i] != '\0'; i++) result += toupper(s[i]);
	return result;
}

// Convert string to lowercase
const char *lowerCase(const char *s)
{
	static Dnchar result(1024);
	result.clear();
	for (int i = 0; s[i] != '\0'; i++) result += tolower(s[i]);
	return result;
}

// Get characters before first occurrence of designated character
const char *beforeChar(const char *s, char delim)
{
	static Dnchar result(1024);
	result.clear();
	for (int i = 0; s[i] != '\0'; i++)
	{
		if (s[i] == delim) break;
		result += s[i];
	}
	return result;
}

// Get characters after first occurrence of designated character
const char *afterChar(const char *s, char delim)
{
	static Dnchar result(1024);
	result.clear();
	bool found = FALSE;
	for (int i = 0; s[i] != '\0'; i++)
	{
		if (found) result += s[i];
		if (s[i] == delim) found = TRUE;
	}
	return result;
}

// Get characters after last occurrence of designated character
const char *afterLastChar(const char *s, char delim)
{
	static Dnchar result;
	result.clear();
	const char *c, *d = NULL;
	for (c = &s[0]; *c != '\0'; ++c) if (*c == delim) d = c;
	if (d != NULL) result = ++d;
	return result;
}

// Get characters before first occurrence of designated string
const char *beforeStr(const char *s, const char *search)
{
	// Search for first occurrence of string
	static char result[8096];
	strcpy(result, s);
	char *c = strstr(result, search);
	if (c == NULL) return "";
	*c = '\0';
	return result;
}

// Get characters after first occurrence of designated character
const char *afterStr(const char *s, const char *search)
{
	const char *c = strstr(s, search);
	if (c == NULL) return "";
	for (const char *d = &search[0]; *d != '\0'; ++d) c++;
	return c;
}

// Search enum list for text
int enumSearch(const char *name, int maxn, const char **itemlist, const char *query, bool reporterror)
{
	static Dnchar lowerq, lowers;
	int result = maxn, i;
	lowerq = lowerCase(query);
	for (i=0; i<maxn; i++)
	{
		lowers = lowerCase(itemlist[i]);
		if (lowerq == lowers)
		{
			result = i;
			break;
		}
	}
	if ((result == maxn) && (name[0] != '\0') && reporterror) printf("Unrecognised %s '%s'\n",name,query);
	return result;
}

// Print valid enum values
void enumPrintValid(int nitems, const char **list)
{
	msg.print("Valid values are:\n    ");
	for (int i=0; i < nitems; i++)
	{
		if ((strcmp(list[i],"_NULL_") == 0) || (list[i][0] == '_')) continue;
		msg.print("%s ", lowerCase(list[i]));
	}
	msg.print("\n");
}

// Convert the number 'n' to a string representation.
const char *itoa(int n)
{
	static Dnchar result;
	result.sprintf("%i",n);
	return result;
}

// Convert the real number 'f' to a string representation
const char *ftoa(double f)
{
	static Dnchar result;
	result.sprintf("%f",f);
	return result;
}

// Convert the real number 'f' to a string representation with supplied format
const char *ftoa(double f,const char *fmt)
{
	static Dnchar result;
	result.sprintf(fmt,f);
	return result;
}

// Strip trailing whitespace from string
const char *stripTrailing(const char *s)
{
	int n;
	static char result[1024];
	// Go backwards through string and find first non-whitespace character
	for (n=strlen(s)-1; n>=0; n--) if (s[n] != ' ') break;
	strncpy(result,s,n+1);
	result[n+1] = '\0';
	return result;
}

// Replace all of the supplied characters in the source string
const char *replaceChars(const char *s, const char *charstoreplace, char r)
{
	static Dnchar result;
	bool found;
	char const *c1, *c2;
	result.clear();
	for (c1 = &s[0]; *c1 != '\0'; c1++)
	{
		found = FALSE;
		for (c2 = charstoreplace; *c2 != '\0'; c2++)
		{
			if (*c1 == *c2)
			{
				found = TRUE;
				break;
			}
		}
		if (found) result += r;
		else result += *c1;
	}
	return result.get();
}

// Strip all of the supplied characters from the source string
const char *stripChars(const char *s, const char *charstostrip)
{
	static Dnchar result;
	char const *c1, *c2;
	bool found;
	result.clear();
	for (c1 = &s[0]; *c1 != '\0'; c1++)
	{
		found = FALSE;
		for (c2 = &charstostrip[0]; *c2 != '\0'; c2++)
		{
			if (*c1 == *c2)
			{
				found = TRUE;
				break;
			}
		}
		if (!found) result += *c1;
	}
	return result.get();
}

// Count number of times that supplied characters occur in supplied string
int countChars(const char *s, const char *chars, int offset)
{
	int total = 0, n, count = 0;
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
	if (f.is_open())
	{
		f.close();
		return TRUE;
	}
	else return FALSE;
}
