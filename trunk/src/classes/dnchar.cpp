/*
	*** Dynamic character array
	*** src/classes/dnchar.cpp
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

#include "base/debug.h"
#include "base/constants.h"
#include "base/sysfunc.h"
#include "classes/dnchar.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// Constructor
dnchar::dnchar()
{
	data = NULL;
	size = 0;
	endpos = -1;
	#ifdef MEMDEBUG
		memdbg.create[MD_DNCHAR] ++;
	#endif
}

// Destructor
dnchar::~dnchar()
{
	if (data != NULL) delete[] data;
	#ifdef MEMDEBUG
		memdbg.destroy[MD_DNCHAR] ++;
	#endif
}

// Print
void dnchar::print() const
{
	printf("DnChar len = %2i, end = %2i : '%s'\n",size,endpos,data);
}

// Clear
void dnchar::clear()
{
	if (data == NULL) return;
	//if (data != NULL) delete[] data;
	//data = NULL;
	//size = 1;
	endpos = 0;
	data[0] = '\0';
}

// Set
void dnchar::set(const char *s)
{
	// Check if array has already been initialised
	if (data != NULL) delete[] data;
	// Get length of string to copy
	// Must check if passed string is empty
	if (s == NULL)
	{
		size = 1;
		endpos = 0;
		data = new char[1];
		data[0] = '\0';
	}
	else
	{
		size = strlen(s) + 1;
		endpos = size-1;
		data = new char[size];
		strcpy(data,s);
	}

}

// Get
const char *dnchar::get() const
{
	return (data != NULL ? data : "");
}

// Get length
int dnchar::length() const
{
	return (endpos < size ? endpos : size);
}

// Create empty array
void dnchar::create_empty(int newsize)
{
	// Check if array has already been initialised
	if (data != NULL) delete[] data;
	// Create new, empty array
	size = newsize;
	data = new char[newsize];
	endpos = 0;
	data[0] = '\0';
}

// Create empty array
void dnchar::create_empty(dnchar &s)
{
	create_empty(s.size);
}

// Empty?
bool dnchar::empty() const
{
	return (endpos <= 0 ? TRUE : FALSE);
}	

// Erase range
void dnchar::erase(int start, int end)
{
	// Retain original memory length of string, but move '\0' and decrease 'size'
	// Check range given
	if (start >= endpos) return;
	if (end >= endpos) end = endpos - 1;
	int count = endpos - end;
	//printf("Range to erase is %i to %i.\n",start,end);
	//printf("Characters after endpoint = %i\n",count);
	// Copy the character in position 'n' to position 'start + (n-last-1)'
	//printf("   DNCHAR - Before erase(%i,%i) = '%s', After = ",start,end,data);
	for (int n=0; n<count; n++) data[start+n] = data[end+n+1];
	size -= (1 + end - start);
	endpos -= (1 + end - start);
	//printf("'%s'\n",data);
}

// Erase from start
void dnchar::erasestart(int n)
{
	//printf("erasestart - n = %i, endpos = %i\n",n,endpos);
	if ((n - 1) > endpos)
	{
		printf("new (old) n = (%i) %i\n",n,endpos);
		n = endpos; 
	}
	if (n > 0) erase(0,n-1);
}

// Erase from end
void dnchar::eraseend(int n)
{
	if ((n - 1) >= endpos) n = endpos;
	if (n > 0) erase(endpos-n,endpos-1);
}

// Assignment operator (const char*)
void dnchar::operator=(const char *s)
{
	set(s);
}

// Assignment operator (const dnchar&)
void dnchar::operator=(const dnchar &source)
{
	set(source.data);
}

// Equality Operator (const char*)
bool dnchar::operator==(const char *s) const
{
	if (data == NULL) return FALSE;
	return (strcmp(data,s) == 0 ? TRUE : FALSE);
}

// Equality Operator
bool dnchar::operator==(const dnchar &s) const
{
	if ((data == NULL) || (s.data == NULL)) return FALSE;
	return (strcmp(data,s.data) == 0 ? TRUE : FALSE);
}

// Inequality Operator
bool dnchar::operator!=(const dnchar &s) const
{
	if ((data == NULL) || (s.data == NULL)) return TRUE;
	return (strcmp(data,s.data) == 0 ? FALSE : TRUE);
}

// Subscript operator
char dnchar::operator[](int n) const
{
	if ((n < 0) || (n >= size))
	{
		printf("dnchar::operator[] <<<< Array subscript %i out of range (0-%i) >>>>\n",n,size-1);
		return 0;
	}
	return data[n];
}

// Character addition
void dnchar::operator+=(char c)
{
	// If we're passed \0, ignore it (since we already have one)
	// Check size of array
	if ((endpos == (size - 1)) && (c != '\0'))
	{
		printf("dnchar::operator+= <<<< No space left to add character >>>>\n");
		return;
	}
	if (c != '\0')
	{
		data[endpos] = c;
		endpos ++;
	}
	data[endpos] = '\0';
}

// String addition
void dnchar::cat(const char *s)
{
	for (int n = 0; s[n] != '\0'; n++) *this += s[n];
}

// Find character
int dnchar::find(char search)
{
	int result = 0;
	char *c;
	for (c = data; *c != '\0'; c++)
	{
	//printf("dnchar %c %c\n",*c,search);
		if (*c == search) break;
		result ++;
	}
	if (result >= endpos) result = -1;
	return result;
}

// Cut characters from start
void dnchar::cutstart(int len, dnchar &target)
{
	// Set new size of target string
	target.create_empty(len+1);
	for (int n=0; n<len; n++) target += data[n];
	erase(0,len-1);
}

// Return as double
double dnchar::as_double() const
{
	return (data != NULL ? atof(data) : 0.0);
}

// Return as integer
int dnchar::as_integer() const
{
	return (data != NULL ? atoi(data) : 0);
}

// Return as bool
bool dnchar::as_bool() const
{
	// Convert string to boolean
	bool result = TRUE;
	static char lcase[512];
	strcpy(lcase,lower_case(data));
	if (strcmp(lcase,"off") == 0) result = FALSE;
	else if (strcmp(lcase,"no") == 0) result = FALSE;
	else if (strcmp(lcase,"false") == 0) result = FALSE;
	return result;
}

// Is Number?
bool dnchar::is_numeric() const
{
	// Go through string - if we find a 'non-number' character, return false
	for (char *c = data; *c != '\0'; c++)
		switch (*c)
		{
			case (' '): case ('0'): case ('1'): case ('2'): case ('3'): case ('4'): 
			case ('.'): case ('5'): case ('6'): case ('7'): case ('8'): case ('9'): 
			case ('e'): case ('E'): case ('+'): case ('-'):
				break;
			default:
				return FALSE;
		}
	return TRUE;
}

