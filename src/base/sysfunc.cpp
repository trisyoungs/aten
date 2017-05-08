/*
	*** System functions
	*** src/base/sysfunc.cpp
	Copyright T. Youngs 2007-2016

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

#include "math/constants.h"
#include "base/messenger.h"
#include <fstream>
#include <iostream>
#include <string.h>
#include <QFileInfo>

ATEN_USING_NAMESPACE

/*
 * String functions
 */

// Remove comments from line
void removeComments(QString& line)
{
	QChar c, quoteChar = '\0';
	static const QChar nullChar = '\0', doubleQuoteChar = '"', singleQuoteChar = '\'', escapeChar = '\\', slashChar = '/', hashChar = '#';
	bool escaped = false;

	for (int n=0; n<line.length(); ++n)
	{
		// Get current char
		c = line.at(n);

		// Remember current quoting info...
		if (c == doubleQuoteChar)
		{
			if (quoteChar == nullChar) quoteChar = '"';
			else if (quoteChar == doubleQuoteChar) quoteChar = '\0';
		}
		if (c == singleQuoteChar)
		{
			if (quoteChar == nullChar) quoteChar = '\'';
			else if (quoteChar == singleQuoteChar) quoteChar = '\0';
		}
		if ((c == hashChar) && (!escaped) && (quoteChar == nullChar))
		{
			line.chop(line.length()-n);
			break;
		}
		else if ((c == slashChar) && (!escaped) && (quoteChar == nullChar))
		{
			if (line.at(n+1) == slashChar)
			{
				line.chop(line.length()-n);
				break;
			}
		}
		escaped = (c == escapeChar);
	}
}

/*
 * Enum Searching
 */

// Search enum list for text
int enumSearch(QString enumName, int nItems, const char* itemArray[], QString query, bool reportError)
{
	QString lowerQuery;
	lowerQuery = query.toLower();
	for (int i=0; i<nItems; ++i)
	{
		if (lowerQuery == QString(itemArray[i]).toLower()) return i;
	}

	// No match
	if ((!enumName.isEmpty()) && reportError) Messenger::print("Unrecognised %s '%s'", qPrintable(enumName), qPrintable(query));
	return nItems;
}

// Print valid enum values
void enumPrintValid(int nItems, const char* itemArray[])
{
	Messenger::print("Valid values are:");
	for (int i=0; i<nItems; ++i)
	{
		if (itemArray[i][0] == '_') continue;
		Messenger::print("%s ", itemArray[i]);
	}
	Messenger::print("");
}
