/*
	*** System functions
	*** src/base/sysfunc.h
	Copyright T. Youngs 2007-2018

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

#ifndef ATEN_SYSFUNC_H
#define ATEN_SYSFUNC_H

#include <QString>

// String functions
void removeComments(QString& line);

// Enum search and print
int enumSearch(QString enumName, int nItems, const char* itemArray[], QString name, bool reportError = true);
void enumPrintValid(int nItems, const char* itemArray[]);

#endif
