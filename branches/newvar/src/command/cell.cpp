/*
	*** Cell command functions
	*** src/command/cell.cpp
	Copyright T. Youngs 2007,2008

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

#include "command/commandlist.h"
#include "base/messenger.h"
#include "base/spacegroup.h"
#include "model/model.h"

// Adjust parameter of unit cell
int CommandData::function_CA_ADJUSTCELL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	Cell::CellParameter cp = Cell::cellParameter(c->argc(0));
	if (cp != Cell::nCellParameters) obj.rs->cell()->setParameter(cp, c->argd(1), TRUE);
	return CR_SUCCESS;
}

// Fold atoms into unit cell
int CommandData::function_CA_FOLD(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	if (c->parent()->inputFile() == NULL)
	{
		obj.rs->beginUndoState("Fold Atoms");
		obj.rs->foldAllAtoms();
		obj.rs->endUndoState();
	}
	else if (prefs.foldOnLoad() != Prefs::SwitchOff) obj.rs->foldAllAtoms();
	return CR_SUCCESS;
}

// Fold molecules
int CommandData::function_CA_FOLDMOLECULES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Fold Molecules");
	obj.rs->foldAllMolecules();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Convert fractional coordinates to real coordinates
int CommandData::function_CA_FRACTOREAL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Convert fractional to real coordinates");
	obj.rs->fracToReal();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Do crystal packing in model
int CommandData::function_CA_PACK(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	if (c->parent()->inputFile() == NULL)
	{
		obj.rs->beginUndoState("Pack Cell");
		obj.rs->pack();
		obj.rs->endUndoState();
	}
	else if (prefs.packOnLoad() != Prefs::SwitchOff) obj.rs->pack();
	return CR_SUCCESS;
}

// Print cell information ('printcell')
int CommandData::function_CA_PRINTCELL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	msg.print("Unit cell type for model '%s' is %s\n", obj.rs->name(), Cell::cellType(obj.rs->cell()->type()));
	if (obj.rs->cell()->type() != Cell::NoCell) obj.rs->cell()->print();
	return CR_SUCCESS;
}

// Replicate cell ('replicate <negx negy negz> <posx posy posz>')
int CommandData::function_CA_REPLICATE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Replicate cell");
	obj.rs->replicateCell(c->arg3d(0), c->arg3d(3));
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Scale cell and molecule COGs ('scale <x y z>')
int CommandData::function_CA_SCALE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Scale cell");
	obj.rs->scaleCell(c->arg3d(0));
	return CR_SUCCESS;
}

// Set/create unit cell ('cell <a b c> <alpha beta gamma>')
int CommandData::function_CA_CELL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->setCell(c->arg3d(0), c->arg3d(3));
	obj.rs->endUndoState();
	obj.rs->calculateDensity();
	return CR_SUCCESS;
}

// Set/create unit cell ('cellaxes <ax ay az> <bx by bz> <cx cy cz>')
int CommandData::function_CA_CELLAXES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	Mat3<double> mat;
	mat.rows[0] = c->arg3d(0);
	mat.rows[1] = c->arg3d(3);
	mat.rows[2] = c->arg3d(6);
	obj.rs->beginUndoState("Set cell");
	obj.rs->setCell(mat);
	obj.rs->endUndoState();
	obj.rs->calculateDensity();
	return CR_SUCCESS;
}

// Remove unit cell
int CommandData::function_CA_NOCELL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Remove cell");
	obj.rs->removeCell();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Set parameter of unit cell
int CommandData::function_CA_SETCELL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	Cell::CellParameter cp = Cell::cellParameter(c->argc(0));
	if (cp != Cell::nCellParameters) obj.rs->cell()->setParameter(cp, c->argd(1));
	return CR_SUCCESS;
}

// Set spacegroup
int CommandData::function_CA_SPACEGROUP(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	// If argument passed is an integer, set by integer. If a character, search by spacegroup name
	if (c->argt(0) == Variable::IntegerVariable) obj.rs->setSpacegroup(c->argi(0));
	else
	{
		msg.print("Searching for spacegroup '%s'...",c->argc(0));
		int sg = spacegroups.spacegroup(c->argc(0));
		if (sg == 0) msg.print(" not found - no spacegroup set.\n");
		else msg.print(" found, id = %i.\n",sg);
		obj.rs->setSpacegroup(sg);
	}
	return CR_SUCCESS;
}
