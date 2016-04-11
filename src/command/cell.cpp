/*
	*** Cell Commands
	*** src/command/cell.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "model/bundle.h"
#include "model/model.h"
#include "sg/generator.h"

ATEN_USING_NAMESPACE

// Add manual spacegroup generator
bool Commands::function_AddGenerator(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Convert argument to generator
	Generator* gen = obj.rs()->cell().addGenerator();
	if (!gen->set(c->argc(0)))
	{
		Messenger::print("Failed to create new generator definition.");
		return false;
	}
	rv.reset();
	return true;
}

// Adjust parameter of unit cell
bool Commands::function_AdjustCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	UnitCell::CellParameter cp = UnitCell::cellParameter(c->argc(0));
	if (cp != UnitCell::nCellParameters) obj.rs()->cell().setParameter(cp, c->argd(1), true);
	rv.reset();
	return true;
}

// Set/create unit cell ('cell <a b c> <alpha beta gamma>')
bool Commands::function_Cell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.rs()->cell().type() == UnitCell::NoCell) obj.rs()->beginUndoState("Add Cell");
	else obj.rs()->beginUndoState("Edit Cell");
	obj.rs()->setCell(c->arg3d(0), c->arg3d(3));
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Set/create unit cell ('cellaxes <ax ay az> <bx by bz> <cx cy cz>')
bool Commands::function_CellAxes(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Matrix mat;
	mat.setColumn(0, c->arg3d(0), 0.0);
	mat.setColumn(1, c->arg3d(3), 0.0);
	mat.setColumn(2, c->arg3d(6), 0.0);
	obj.rs()->beginUndoState("Set cell");
	obj.rs()->setCell(mat);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Fold atoms into unit cell
bool Commands::function_Fold(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->parent()->parser() == NULL)
	{
		obj.rs()->beginUndoState("Fold Atoms");
		obj.rs()->foldAllAtoms();
		obj.rs()->endUndoState();
	}
	else if (prefs.foldOnLoad() != Choice::No) obj.rs()->foldAllAtoms();
	rv.reset();
	return true;
}

// Fold molecules
bool Commands::function_FoldMolecules(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Fold Molecules");
	obj.rs()->foldAllMolecules();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Convert fractional coordinates to real coordinates
bool Commands::function_FracToReal(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Convert fractional to real coordinates");
	obj.rs()->fracToReal();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Cleave crystal along Miller plane
bool Commands::function_MillerCut(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Cleave along Miller plane (%i%i%i)", c->argi(0), c->argi(1), c->argi(2));
	obj.rs()->selectMiller(c->argi(0), c->argi(1), c->argi(2), c->hasArg(3) ? c->argb(3) : false);
	obj.rs()->selectionDelete();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Remove unit cell
bool Commands::function_NoCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Remove cell");
	obj.rs()->removeCell();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Do crystal packing in model
bool Commands::function_Pack(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (!c->parent()->isFilter())
	{
		obj.rs()->beginUndoState("Pack Cell");
		obj.rs()->pack();
		obj.rs()->foldAllAtoms();
		obj.rs()->endUndoState();
	}
	else if (prefs.packOnLoad() != Choice::No) obj.rs()->pack();
	rv.reset();
	return true;
}

// Print cell information ('printcell')
bool Commands::function_PrintCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Messenger::print("Unit cell type for model '%s' is %s", qPrintable(obj.rs()->name()), UnitCell::cellType(obj.rs()->cell().type()));
	if (obj.rs()->cell().type() != UnitCell::NoCell) obj.rs()->cell().print();
	rv.reset();
	return true;
}

// Replicate cell ('replicate <negx negy negz> <posx posy posz>')
bool Commands::function_Replicate(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	bool foldBefore = (c->hasArg(6) ? c->argb(6) : false);
	bool trimAfter = (c->hasArg(7) ? c->argb(7) : false);

	obj.rs()->beginUndoState("Replicate cell");
	obj.rs()->replicateCell(c->arg3d(0), c->arg3d(3), foldBefore, trimAfter);
	obj.rs()->endUndoState();

	rv.reset();
	return true;
}

// Rotate cell and contents ('rotatecell <axis> <angle>')
bool Commands::function_RotateCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Determine supplied axis and get angle
	int axis = -1;
	char ch = c->argc(0).at(0).toLatin1();
	switch (ch)
	{
		case ('X'):
		case ('x'):
		case ('1'):
			axis = 0;
			break;
		case ('Y'):
		case ('y'):
		case ('2'):
			axis = 1;
			break;
		case ('Z'):
		case ('z'):
		case ('3'):
			axis = 2;
			break;
		default:
			Messenger::print("Unrecognised axis '%c' given to 'rotatecell'.",ch);
			return false;
			break;
	}
	double angle = c->argd(1);
	obj.rs()->beginUndoState("Rotate cell %fdeg about %c-axis", angle, 88+axis);
	obj.rs()->rotateCell(axis, angle);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Scale cell and atom positions ('scale <x y z>')
bool Commands::function_Scale(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Scale cell and atoms");
	obj.rs()->scaleCell(c->arg3d(0), false);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Scale cell and molecule COGs ('scalemolecules <x y z>')
bool Commands::function_ScaleMolecules(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Scale cell and molecule centres");
	bool result = obj.rs()->scaleCell(c->arg3d(0), true);
	obj.rs()->endUndoState();
	rv.reset();
	return (result);
}

// Set parameter of unit cell
bool Commands::function_SetCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	UnitCell::CellParameter cp = UnitCell::cellParameter(c->argc(0));
	if (cp != UnitCell::nCellParameters) obj.rs()->cell().setParameter(cp, c->argd(1));
	rv.reset();
	return true;
}

// Run SGInfo on supplied string
bool Commands::function_SGInfo(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	T_SgInfo sg;
	sg.MaxList = 192;
	sg.ListSeitzMx = new T_RTMx[192];
	sg.ListRotMxInfo = new T_RotMxInfo[192];
	rv.set(0);
	// Do a table lookup of the sg text (assume volume is 'A')
	const T_TabSgName *tsgn = FindTabSgNameEntry(qPrintable(c->argc(0)), 'A');
	if (tsgn == NULL)
	{
		Messenger::print("Unable to find spacegroup '%s'.", qPrintable(c->argc(0)));
		return false;
	}
// 	SgName = tsgn->HallSymbol;
	
	// Initialize the SgInfo structure
	InitSgInfo(&sg);
	sg.TabSgName = tsgn;
	
	// Translate the Hall symbol and generate the whole group
	ParseHallSymbol(tsgn->HallSymbol, &sg);
	if (SgError != NULL) return true;
	
	/* Do some book-keeping and derive crystal system, point group,
	and - if not already set - find the entry in the internal
	table of space group symbols
	*/
	CompleteSgInfo(&sg);

	ListSgInfo(&sg, 1, 0, stdout);
	
	rv.set(tsgn->SgNumber);
	delete[] sg.ListSeitzMx;
	delete[] sg.ListRotMxInfo;
	return true;
}

// Set spacegroup
bool Commands::function_Spacegroup(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// If argument passed is an integer, set by integer. If a character, search by spacegroup name
	obj.rs()->cell().setSpacegroup(c->argc(0), prefs.forceRhombohedral());
	rv.reset();
	return true;
}

