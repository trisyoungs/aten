/*
	*** Variable Types
	*** src/parser/types.h
	Copyright T. Youngs 2007-2009

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

#include "parser/vtypes.h"
#include "base/constants.h"
#include "base/sysfunc.h"
#include <string.h>
#include <stdio.h>

// Variable Types
const char *NuDataTypeNames[NuVTypes::nDataTypes] = { "no data", "integer", "real", "character", "vector", "aten&", "atom&", "bond&", "cell&", "elements&", "forcefield&", "ffatom&", "ffbound&", "grid&", "model&", "pattern&" };
const char *NuDataTypeKeywords[NuVTypes::nDataTypes] = { "_NODATA", "integer", "real", "character", "vector", "_ATEN", "atom", "bond", "cell", "_ELEMENTS", "forcefield", "ffatom", "ffbound", "grid", "model", "pattern" };
NuVTypes::DataType NuVTypes::dataType(const char *s)
{
	return (NuVTypes::DataType) enumSearch("", NuVTypes::nDataTypes, NuDataTypeKeywords, s);
}
const char *NuVTypes::dataType(NuVTypes::DataType dt)
{
	return NuDataTypeNames[dt];
}
bool NuVTypes::isPointer(NuVTypes::DataType dt)
{
	if (dt > NuVTypes::VectorData) return TRUE;
	return FALSE;
}
NuVTypes::DataType NuVTypes::determineType(const char *s)
{
	// Try to determine type_ of the argument
	int ch, nn = 0, nch = 0, ndp = 0, npm = 0, ne = 0;
	unsigned int i;
	for (i = 0; i < strlen(s); i++)
	{
		ch = s[i];
		if ((ch > 47) && (ch < 58)) nn ++;
		else if (ch == '.') ndp ++;
		else if ((ch == '-') || (ch == '+')) npm ++;
		else if ((ch == 'e') || (ch == 'E')) ne ++;
		else nch ++;
	}
	// Based on the numbers we calculated, try to determine its type
	if ((nch != 0) || (ndp > 1) || (npm > 2) || (ne > 1) || (nn == 0)) return NuVTypes::CharacterData;
	else if (ndp == 1) return NuVTypes::RealData;
	else return NuVTypes::IntegerData;
}
