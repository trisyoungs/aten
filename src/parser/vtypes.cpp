/*
	*** Variable Types
	*** src/parser/vtypes.cpp
	Copyright T. Youngs 2007-2015

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
#include "math/constants.h"
#include "base/sysfunc.h"
#include "base/messenger.h"
#include <string.h>

ATEN_USING_NAMESPACE

// Variable Types
const char* DataTypeNames[VTypes::nDataTypes] = { "no data", "int", "double", "string", "vector", "matrix", "Aten&", "Atom&", "BasisPrimitive&", "BasisShell&", "Bond&", "UnitCell&", "ColourScale&", "ColourScalePoint&", "Dialog&", "Eigenvector&", "Element&", "EnergyStore&", "Forcefield&", "FFAtom&", "FFBound&", "Glyph&", "GlyphData&", "Grid&", "MC&", "Measurement&", "Model&", "Pattern&", "Bound&", "Prefs&", "Site&", "Vibration&", "Widget&", "ZMatrix&", "ZMatrixElement&" };
const char* DataTypePhrases[VTypes::nDataTypes] = { "no data", "an integer", "a double", "a string", "a vector", "a matrix", "Aten&", "an Atom&", "a BasisPrimitive&", "a BasisShell&", "a Bond&", "a UnitCell&", "a Colourscale&", "a ColourScalePoint&", "a Dialog&", "an Eigenvector&", "an Element&", "an EnergyStore&", "a Forcefield&", "a FFAtom&", "a FFBound&", "a Glyph&", "a GlyphData&", "a Grid&", "MC&", "a Measurement&", "a Model&", "a Pattern&", "a Bound&", "some Prefs&", "a Site", "a Vibration&", "a Widget&", "a ZMatrix&", "a ZMatrix element&" };
const char* DataTypeArrayPhrases[VTypes::nDataTypes] = { "no data", "an integer array", "a double array", "a string array", "a vector array", "a matrix array", "an Aten& array", "an Atom& array", "a BasisPrimitive& array", "a BasisShell& array", "a Bond& array", "a UnitCell& array", "a Colourscale& array", "a ColourScalePoint& array", "a Dialog& array", "an Eigenvector& array", "an Element& array", "an EnergyStore& array", "a Forcefield& array", "a FFAtom& array", "a FFBound& array", "a Glyph& array", "a GlyphData& array", "a Grid& array", "a MC& array", "a Measurement& array", "a Model& array", "a Pattern& array", "a Bound& array", "a Prefs& array", "a Site& array", "a Vibration& array","a Widget& array", "a ZMatrix& array", "a ZMatrixElement& array" };
const char* DataTypeKeywords[VTypes::nDataTypes] = { "_NODATA", "int", "double", "string", "vector", "matrix", "_ATEN", "Atom", "BasisPrimitive", "BasisShell", "Bond", "UnitCell", "ColourScale", "ColourScalePoint", "Dialog", "EigenVector", "Element", "EnergyStore", "Forcefield", "FFAtom", "FFBound", "Glyph", "_GLYPHDATA", "Grid", "_MC", "Measurement", "Model", "Pattern", "Bound", "_PREFS", "Site", "Vibration", "Widget", "ZMatrix", "ZMatrixElement" };
bool DataTypeUserCreatable[VTypes::nDataTypes] = { false, false, false, false, false, false, /*Aten*/ false, true, true, true, true, true, true, true, /*Dialog*/ false, true, /*Element*/ false, /*EnergyStore*/ false, true, true, true, true, /*GlyphData*/ false, true, /*MC*/ false, /*Measurement*/ false, /*Model*/ false, /*Pattern*/ false, /*PatternBound*/ false, /*Prefs*/ false, /*Site*/ false, true, /*Widget*/ false, true, /*ZMatrixElement*/ false };

// Return datatype based on supplied name
VTypes::DataType VTypes::dataType(QString s, bool reportError)
{
	VTypes::DataType dt = (VTypes::DataType) enumSearch("", VTypes::nDataTypes, DataTypeKeywords, s);
	if ((dt == VTypes::nDataTypes) && reportError) enumPrintValid(VTypes::nDataTypes,DataTypeKeywords);
	return dt;
}

// Return name based on supplied datatype
const char* VTypes::dataType(VTypes::DataType dt)
{
	return DataTypeNames[dt];
}

// Return phrased name based on supplied datatype
const char* VTypes::aDataType(VTypes::DataType dt, int arraysize)
{
	return arraysize == -1 ? DataTypePhrases[dt] : DataTypeArrayPhrases[dt];
}

// Return whether specified datatype is inherently a class (pointer) datatype
bool VTypes::isPointer(VTypes::DataType dt)
{
	if (dt > VTypes::VectorData) return true;
	return false;
}

// Return whether the datatype can be user-created
bool VTypes::userCanCreate(DataType dt)
{
	return DataTypeUserCreatable[dt];
}

// Determine datatype based on literal value of string
VTypes::DataType VTypes::determineType(QString s)
{
	// Try to determine type_ of the argument
	int ch, nn = 0, nch = 0, ndp = 0, npm = 0, ne = 0;
	for (int i = 0; i < s.length(); ++i)
	{
		ch = s.at(i).toLatin1();
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
		Messenger::print(Messenger::Verbose, "One or both arguments have no data type.");
		return UntypedData;
	}
	if ((arraysize1 > 0) && (arraysize2 > 0) && (arraysize1 != arraysize2))
	{
		Messenger::print("Array sizes do not conform.");
		return ArrayMisMatch;
	}
	int bit1 = (1 << (type1 < AtenData ? type1-1 : AtenData-1)) << (arraysize1 != -1 ? AtenData : 0);
	int bit2 = (1 << (type2 < AtenData ? type2-1 : AtenData-1)) << (arraysize2 != -1 ? AtenData : 0);
	return (bit1 + (bit2 << (AtenData*2)));
}

int VTypes::dataSinglet(DataType type1, int arraysize1)
{
	if (type1 == VTypes::NoData)
	{
		Messenger::print("Argument has no data type.");
		return UntypedData;
	}
	return ((1 << (type1 < AtenData ? type1-1 : AtenData-1)) << (arraysize1 != -1 ? AtenData : 0));
}
