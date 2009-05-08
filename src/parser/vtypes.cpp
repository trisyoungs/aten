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
#include "base/messenger.h"
#include <string.h>
#include <stdio.h>

// Variable Types
const char *DataTypeNames[VTypes::nDataTypes] = { "no data", "int", "double", "string", "vector", "aten&", "atom&", "bond&", "unitcell&", "elements&", "forcefield&", "ffatom&", "ffbound&", "grid&", "model&", "pattern&", "patternbound&", "prefs&"};
const char *DataTypePhrases[VTypes::nDataTypes] = { "no data", "an integer", "a double", "a string", "a vector", "aten&", "an atom&", "a bond&", "a unitcell&", "an element&", "a forcefield&", "a ffatom&", "a ffbound&", "a grid&", "a model&", "a pattern&", "a patternbound&", "the prefs&" };
const char *DataTypeArrayPhrases[VTypes::nDataTypes] = { "no data", "an integer array", "a double array", "a string array", "a vector array", "an aten& array", "an atom& array", "a bond& array", "a unitcell& array", "an element& array", "a forcefield& array", "a ffatom& array", "a ffbound& array", "a grid& array", "a model& array", "a pattern& array", "a patternbound& array", "a prefs& array" };
const char *DataTypeKeywords[VTypes::nDataTypes] = { "_NODATA", "int", "double", "string", "vector", "_ATEN", "atom", "bond", "unitcell", "_ELEMENTS", "forcefield", "ffatom", "ffbound", "grid", "model", "pattern", "patternbound", "_PREFS"};
VTypes::DataType VTypes::dataType(const char *s)
{
	return (VTypes::DataType) enumSearch("", VTypes::nDataTypes, DataTypeKeywords, s);
}
const char *VTypes::dataType(VTypes::DataType dt)
{
	return DataTypeNames[dt];
}
const char *VTypes::aDataType(VTypes::DataType dt, int arraysize)
{
	return arraysize == -1 ? DataTypePhrases[dt] : DataTypeArrayPhrases[dt];
}
bool VTypes::isPointer(VTypes::DataType dt)
{
	if (dt > VTypes::VectorData) return TRUE;
	return FALSE;
}
VTypes::DataType VTypes::determineType(const char *s)
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
	if ((nch != 0) || (ndp > 1) || (npm > 2) || (ne > 1) || (nn == 0)) return VTypes::StringData;
	else if (ndp == 1) return VTypes::DoubleData;
	else return VTypes::IntegerData;
}

int VTypes::dataPair(DataType type1, int arraysize1, DataType type2, int arraysize2)
{
	if ((type1 == VTypes::NoData) || (type2 == VTypes::NoData))
	{
		msg.print("One or both arguments have no data type.\n");
		return UntypedData;
	}
	if ((arraysize1 != -1) && (arraysize2 != -1) && (arraysize1 != arraysize2))
	{
		msg.print("Array sizes do not conform.\n");
		return ArrayMisMatch;
	}
	int bit1 = (1 << (type1 < AtenData ? type1-1 : AtenData-1)) << (arraysize1 != -1 ? AtenData : 0);
	int bit2 = (1 << (type2 < AtenData ? type2-1 : AtenData-1)) << (arraysize2 != -1 ? AtenData : 0);
	return (bit1 + (bit2 << (AtenData*2)));
}