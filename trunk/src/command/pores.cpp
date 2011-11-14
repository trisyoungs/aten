/*
	*** Pores Commands
	*** src/command/pores.cpp
	Copyright T. Youngs 2007-2011

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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "model/model.h"
#include "base/sysfunc.h"

// Create a partitioning scheme from the current model
bool Command::function_CreateScheme(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	return TRUE;
}

// Drill pores in current model
bool Command::function_DrillPores(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	
	// Grab some parameters as variables to make readability easier
	double sizeParam = c->argd(1);
	int nA = c->argi(2), nB = c->argi(3), face = c->hasArg(4) ? c->argi(4) - 1 : 2;
	Vec3<double> v;
	if (c->hasArg(5)) v = c->arg3d(5);
	else v.set(0.0,0.0,1.0);
	
	// Determine origin face vectors, and determine first pore centre coordinates
	if ((face < 0) || (face > 2))
	{
		msg.print("Error: Origin face must be specified as 1 (YZ plane), 2 (XZ plane) or 3 (XY plane).\n");
		return FALSE;
	}
	Vec3<double> faceA = obj.rs()->cell()->axes().columnAsVec3((face+1)%3);
	Vec3<double> faceB = obj.rs()->cell()->axes().columnAsVec3((face+2)%3);
	Vec3<double> deltaA = faceA / nA, deltaB = faceB / nB;
	Vec3<double> origin = (deltaA + deltaB) * 0.5;
	
	obj.rs()->beginUndoState("Drill pores");
	for (int a = 0; a < nA; ++a)
	{
		for (int b = 0; b < nB; ++b)
		{
			obj.rs()->selectLine(v, origin + deltaA*a + deltaB*b, sizeParam);
		}
	}
	obj.rs()->selectionDelete();
	obj.rs()->endUndoState();

	return TRUE;
}

// Select pores atoms
bool Command::function_SelectPores(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Grab some parameters as variables to make readability easier
	double sizeParam = c->argd(1);
	int nA = c->argi(2), nB = c->argi(3), face = c->hasArg(4) ? c->argi(4) - 1 : 2;
	Vec3<double> v;
	if (c->hasArg(5)) v = c->arg3d(5);
	else v.set(0.0,0.0,1.0);
	
	// Determine origin face vectors, and determine first pore centre coordinates
	if ((face < 0) || (face > 2))
	{
		msg.print("Error: Origin face must be specified as 1 (YZ plane), 2 (XZ plane) or 3 (XY plane).\n");
		return FALSE;
	}
	Vec3<double> faceA = obj.rs()->cell()->axes().columnAsVec3((face+1)%3);
	Vec3<double> faceB = obj.rs()->cell()->axes().columnAsVec3((face+2)%3);
	Vec3<double> deltaA = faceA / nA, deltaB = faceB / nB;
	Vec3<double> origin = (deltaA + deltaB) * 0.5;
	
	obj.rs()->beginUndoState("Select pore atoms");
	for (int a = 0; a < nA; ++a)
	{
		for (int b = 0; b < nB; ++b)
		{
			obj.rs()->selectLine(v, origin + deltaA*a + deltaB*b, sizeParam);
		}
	}
	obj.rs()->endUndoState();

	return TRUE;
}

// Terminate atoms with OH
bool Command::function_Terminate(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	return TRUE;
}
