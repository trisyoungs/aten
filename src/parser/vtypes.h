/*
	*** Variable Types
	*** src/parser/vtypes.h
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

#ifndef ATEN_VTYPES_H
#define ATEN_VTYPES_H

#include <QString>
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Variable description
class VTypes
{
	public:
	// Data Types
	enum DataType { NoData, IntegerData, DoubleData, StringData, VectorData, MatrixData, AtenData, AtomData, BasisPrimitiveData, BasisShellData, BondData, CellData, ColourScaleData, ColourScalePointData, DialogData, EigenvectorData, ElementData, EnergyStoreData, ForcefieldData, ForcefieldAtomData, ForcefieldBoundData, GlyphData, GlyphDataData, GridData, MonteCarloData, MeasurementData, ModelData, PatternData, PatternBoundData, PreferencesData, SiteData, VibrationData, WidgetData, ZMatrixData, ZMatrixElementData, nDataTypes };
	enum DataPair { ArrayMisMatch = -2, UntypedData = -1, Int = 1, Dbl = 2, Str = 4, Vec = 8, Mat = 16, Ptr = 32, IntA = 64, DblA = 128, StrA = 256, VecA = 512, MatA = 1024, PtrA = 2048, IntInt = 4097, IntDbl = 8193, IntStr = 16385, IntVec = 32769, IntMat = 65537, IntPtr = 131073, IntIntA = 262145, IntDblA = 524289, IntStrA = 1048577, IntVecA = 2097153, IntMatA = 4194305, IntPtrA = 8388609, DblInt = 4098, DblDbl = 8194, DblStr = 16386, DblVec = 32770, DblMat = 65538, DblPtr = 131074, DblIntA = 262146, DblDblA = 524290, DblStrA = 1048578, DblVecA = 2097154, DblMatA = 4194306, DblPtrA = 8388610, StrInt = 4100, StrDbl = 8196, StrStr = 16388, StrVec = 32772, StrMat = 65540, StrPtr = 131076, StrIntA = 262148, StrDblA = 524292, StrStrA = 1048580, StrVecA = 2097156, StrMatA = 4194308, StrPtrA = 8388612, VecInt = 4104, VecDbl = 8200, VecStr = 16392, VecVec = 32776, VecMat = 65544, VecPtr = 131080, VecIntA = 262152, VecDblA = 524296, VecStrA = 1048584, VecVecA = 2097160, VecMatA = 4194312, VecPtrA = 8388616, MatInt = 4112, MatDbl = 8208, MatStr = 16400, MatVec = 32784, MatMat = 65552, MatPtr = 131088, MatIntA = 262160, MatDblA = 524304, MatStrA = 1048592, MatVecA = 2097168, MatMatA = 4194320, MatPtrA = 8388624, PtrInt = 4128, PtrDbl = 8224, PtrStr = 16416, PtrVec = 32800, PtrMat = 65568, PtrPtr = 131104, PtrIntA = 262176, PtrDblA = 524320, PtrStrA = 1048608, PtrVecA = 2097184, PtrMatA = 4194336, PtrPtrA = 8388640, IntAInt = 4160, IntADbl = 8256, IntAStr = 16448, IntAVec = 32832, IntAMat = 65600, IntAPtr = 131136, IntAIntA = 262208, IntADblA = 524352, IntAStrA = 1048640, IntAVecA = 2097216, IntAMatA = 4194368, IntAPtrA = 8388672, DblAInt = 4224, DblADbl = 8320, DblAStr = 16512, DblAVec = 32896, DblAMat = 65664, DblAPtr = 131200, DblAIntA = 262272, DblADblA = 524416, DblAStrA = 1048704, DblAVecA = 2097280, DblAMatA = 4194432, DblAPtrA = 8388736, StrAInt = 4352, StrADbl = 8448, StrAStr = 16640, StrAVec = 33024, StrAMat = 65792, StrAPtr = 131328, StrAIntA = 262400, StrADblA = 524544, StrAStrA = 1048832, StrAVecA = 2097408, StrAMatA = 4194560, StrAPtrA = 8388864, VecAInt = 4608, VecADbl = 8704, VecAStr = 16896, VecAVec = 33280, VecAMat = 66048, VecAPtr = 131584, VecAIntA = 262656, VecADblA = 524800, VecAStrA = 1049088, VecAVecA = 2097664, VecAMatA = 4194816, VecAPtrA = 8389120, MatAInt = 5120, MatADbl = 9216, MatAStr = 17408, MatAVec = 33792, MatAMat = 66560, MatAPtr = 132096, MatAIntA = 263168, MatADblA = 525312, MatAStrA = 1049600, MatAVecA = 2098176, MatAMatA = 4195328, MatAPtrA = 8389632, PtrAInt = 6144, PtrADbl = 10240, PtrAStr = 18432, PtrAVec = 34816, PtrAMat = 67584, PtrAPtr = 133120, PtrAIntA = 264192, PtrADblA = 526336, PtrAStrA = 1050624, PtrAVecA = 2099200, PtrAMatA = 4196352, PtrAPtrA = 8390656 };
	static VTypes::DataType dataType(QString s, bool reportError = false);
	static const char* dataType(DataType dt);
	static const char* aDataType(DataType dt, int arraysize = -1);
	static bool isPointer(DataType dt);
	static bool userCanCreate(DataType dt);
	static VTypes::DataType determineType(QString s);
	static int dataPair(DataType type1, int arraysize1, DataType type2, int arraysize2);
	static int dataSinglet(DataType type1, int arraysize1);
};

ATEN_END_NAMESPACE

#endif
