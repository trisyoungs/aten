/*
	*** Variable Types
	*** src/parser/vtypes.h
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

#ifndef ATEN_VTYPES_H
#define ATEN_VTYPES_H

// Variable description
class VTypes
{
	public:
	// Data Types
	enum DataType { NoData, IntegerData, DoubleData, StringData, VectorData, AtenData, AtomData, BondData, CellData, ElementData, ForcefieldData, ForcefieldAtomData, ForcefieldBoundData, GridData, ModelData, PatternData, PatternBoundData, PreferencesData, nDataTypes };
	enum DataPair { ArrayMisMatch = -2, UntypedData = -1, IntInt = 1025, IntDbl = 2049, IntStr = 4097, IntVec = 8193, IntPtr = 16385, IntIntA = 32769, IntDblA = 65537, IntStrA = 131073, IntVecA = 262145, IntPtrA = 524289, DblInt = 1026, DblDbl = 2050, DblStr = 4098, DblVec = 8194, DblPtr = 16386, DblIntA = 32770, DblDblA = 65538, DblStrA = 131074, DblVecA = 262146, DblPtrA = 524290, StrInt = 1028, StrDbl = 2052, StrStr = 4100, StrVec = 8196, StrPtr = 16388, StrIntA = 32772, StrDblA = 65540, StrStrA = 131076, StrVecA = 262148, StrPtrA = 524292, VecInt = 1032, VecDbl = 2056, VecStr = 4104, VecVec = 8200, VecPtr = 16392, VecIntA = 32776, VecDblA = 65544, VecStrA = 131080, VecVecA = 262152, VecPtrA = 524296, PtrInt = 1040, PtrDbl = 2064, PtrStr = 4112, PtrVec = 8208, PtrPtr = 16400, PtrIntA = 32784, PtrDblA = 65552, PtrStrA = 131088, PtrVecA = 262160, PtrPtrA = 524304, IntAInt = 1056, IntADbl = 2080, IntAStr = 4128, IntAVec = 8224, IntAPtr = 16416, IntAIntA = 32800, IntADblA = 65568, IntAStrA = 131104, IntAVecA = 262176, IntAPtrA = 524320, DblAInt = 1088, DblADbl = 2112, DblAStr = 4160, DblAVec = 8256, DblAPtr = 16448, DblAIntA = 32832, DblADblA = 65600, DblAStrA = 131136, DblAVecA = 262208, DblAPtrA = 524352, StrAInt = 1152, StrADbl = 2176, StrAStr = 4224, StrAVec = 8320, StrAPtr = 16512, StrAIntA = 32896, StrADblA = 65664, StrAStrA = 131200, StrAVecA = 262272, StrAPtrA = 524416, VecAInt = 1280, VecADbl = 2304, VecAStr = 4352, VecAVec = 8448, VecAPtr = 16640, VecAIntA = 33024, VecADblA = 65792, VecAStrA = 131328, VecAVecA = 262400, VecAPtrA = 524544, PtrAInt = 1536, PtrADbl = 2560, PtrAStr = 4608, PtrAVec = 8704, PtrAPtr = 16896, PtrAIntA = 33280, PtrADblA = 66048, PtrAStrA = 131584, PtrAVecA = 262656, PtrAPtrA = 524800 };
	static VTypes::DataType dataType(const char *s);
	static const char *dataType(DataType dt);
	static const char *aDataType(DataType dt, int arraysize = -1);
	static bool isPointer(DataType dt);
	static VTypes::DataType determineType(const char *s);
	static int dataPair(DataType type1, int arraysize1, DataType type2, int arraysize2);
};

#endif
