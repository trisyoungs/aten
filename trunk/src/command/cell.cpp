/*
	*** Cell Commands
	*** src/command/cell.cpp
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
#include "parser/tree.h"
#include "model/model.h"
#include "classes/prefs.h"
#include "base/messenger.h"
#include "base/generator.h"
#include "base/spacegroup.h"

// Add manual spacegroup generator
bool Command::function_AddGenerator(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Convert argument to generator
	Generator *gen = obj.rs->cell()->addGenerator();
	if (!gen->set(c->argc(0)))
	{
		msg.print("Failed to create new generator definition.\n");
		return FALSE;
	}
	rv.reset();
	return TRUE;
}

// Adjust parameter of unit cell
bool Command::function_AdjustCell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Cell::CellParameter cp = Cell::cellParameter(c->argc(0));
	if (cp != Cell::nCellParameters) obj.rs->cell()->setParameter(cp, c->argd(1), TRUE);
	rv.reset();
	return TRUE;
}

// Set/create unit cell ('cell <a b c> <alpha beta gamma>')
bool Command::function_Cell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.rs->cell()->type() == Cell::NoCell) obj.rs->beginUndoState("Add Cell");
	else obj.rs->beginUndoState("Edit Cell");
	obj.rs->setCell(c->arg3d(0), c->arg3d(3));
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Set/create unit cell ('cellaxes <ax ay az> <bx by bz> <cx cy cz>')
bool Command::function_CellAxes(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Matrix mat;
	mat.setColumn(0, c->arg3d(0), 0.0);
	mat.setColumn(1, c->arg3d(3), 0.0);
	mat.setColumn(2, c->arg3d(6), 0.0);
	obj.rs->beginUndoState("Set cell");
	obj.rs->setCell(mat);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Fold atoms into unit cell
bool Command::function_Fold(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (c->parent()->parser() == NULL)
	{
		obj.rs->beginUndoState("Fold Atoms");
		obj.rs->foldAllAtoms();
		obj.rs->endUndoState();
	}
	else if (prefs.foldOnLoad() != Prefs::SwitchOff) obj.rs->foldAllAtoms();
	rv.reset();
	return TRUE;
}

// Fold molecules
bool Command::function_FoldMolecules(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Fold Molecules");
	obj.rs->foldAllMolecules();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Convert fractional coordinates to real coordinates
bool Command::function_FracToReal(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Convert fractional to real coordinates");
	obj.rs->fracToReal();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Cleave crystal along Miller plane
bool Command::function_MillerCut(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Cleave along Miller plane (%i%i%i)", c->argi(0), c->argi(1), c->argi(2));
	obj.rs->selectMiller(c->argi(0), c->argi(1), c->argi(2), c->hasArg(3) ? c->argb(3) : FALSE);
	obj.rs->selectionDelete();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Remove unit cell
bool Command::function_NoCell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Remove cell");
	obj.rs->removeCell();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Do crystal packing in model
bool Command::function_Pack(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (!c->parent()->isFilter())
	{
		obj.rs->beginUndoState("Pack Cell");
		obj.rs->pack();
		obj.rs->endUndoState();
	}
	else if (prefs.packOnLoad() != Prefs::SwitchOff) obj.rs->pack();
	rv.reset();
	return TRUE;
}

// Print cell information ('printcell')
bool Command::function_PrintCell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	msg.print("Unit cell type for model '%s' is %s\n", obj.rs->name(), Cell::cellType(obj.rs->cell()->type()));
	if (obj.rs->cell()->type() != Cell::NoCell) obj.rs->cell()->print();
	rv.reset();
	return TRUE;
}

// Replicate cell ('replicate <negx negy negz> <posx posy posz>')
bool Command::function_Replicate(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Replicate cell");
	obj.rs->replicateCell(c->arg3d(0), c->arg3d(3));
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Rotate cell and contents ('rotatecell <axis> <angle>')
bool Command::function_RotateCell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Determine supplied axis and get angle
	int axis = -1;
	char ch = c->argc(0)[0];
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
			msg.print("Unrecognised axis '%c' given to 'rotatecell'.\n",ch);
			return FALSE;
			break;
	}
	double angle = c->argd(1);
	obj.rs->beginUndoState("Rotate cell %fdeg about %c-axis", angle, 88+axis);
	obj.rs->rotateCell(axis, angle);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Scale cell and atom positions ('scale <x y z> [energy?]')
bool Command::function_Scale(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Scale cell and atoms");
	obj.rs->scaleCell(c->arg3d(0), FALSE, c->hasArg(3) ? c->argb(3) : FALSE);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Scale cell and molecule COGs ('scalemolecules <x y z> [energy?]')
bool Command::function_ScaleMolecules(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Scale cell and molecule centres");
	bool result = obj.rs->scaleCell(c->arg3d(0), TRUE, c->hasArg(3) ? c->argb(3) : FALSE);
	obj.rs->endUndoState();
	rv.reset();
	return (result ? TRUE : FALSE);
}

// Set parameter of unit cell
bool Command::function_SetCell(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Cell::CellParameter cp = Cell::cellParameter(c->argc(0));
	if (cp != Cell::nCellParameters) obj.rs->cell()->setParameter(cp, c->argd(1));
	rv.reset();
	return TRUE;
}

// Run SGInfo on supplied string
bool Command::function_SGInfo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	T_SgInfo sg;
	sg.MaxList = 192;
	sg.ListSeitzMx = new T_RTMx[192];
	sg.ListRotMxInfo = new T_RotMxInfo[192];
	rv.set(0);
	// Do a table lookup of the sg text (assume volume is 'A')
	const T_TabSgName *tsgn = FindTabSgNameEntry(c->argc(0), 'A');
	if (tsgn == NULL)
	{
		msg.print("Unable to find spacegroup '%s'.\n", c->argc(0));
		return FALSE;
	}
// 	SgName = tsgn->HallSymbol;
	
	// Initialize the SgInfo structure
	InitSgInfo(&sg);
	sg.TabSgName = tsgn;
	
	// Translate the Hall symbol and generate the whole group
	ParseHallSymbol(tsgn->HallSymbol, &sg);
	if (SgError != NULL) return TRUE;
	
	/* Do some book-keeping and derive crystal system, point group,
	and - if not already set - find the entry in the internal
	table of space group symbols
	*/
	CompleteSgInfo(&sg);

	ListSgInfo(&sg, 1, 0, stdout);
	
	rv.set(tsgn->SgNumber);
	delete[] sg.ListSeitzMx;
	delete[] sg.ListRotMxInfo;
	return TRUE;
}

// Set spacegroup
bool Command::function_Spacegroup(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// If argument passed is an integer, set by integer. If a character, search by spacegroup name
	obj.rs->setSpacegroup(c->argc(0));
	rv.reset();
	return TRUE;
}

