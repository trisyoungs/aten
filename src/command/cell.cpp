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
#include "model/model.h"
#include "classes/prefs.h"
#include "base/messenger.h"
#include "base/spacegroup.h"

// Adjust parameter of unit cell
int Command::function_CA_ADJUSTCELL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Cell::CellParameter cp = Cell::cellParameter(c->argc(0));
	if (cp != Cell::nCellParameters) obj.rs->cell()->setParameter(cp, c->argd(1), TRUE);
	return Command::Success;
}

// Fold atoms into unit cell
int Command::function_CA_FOLD(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->parent()->inputFile() == NULL)
	{
		obj.rs->beginUndoState("Fold Atoms");
		obj.rs->foldAllAtoms();
		obj.rs->endUndoState();
	}
	else if (prefs.foldOnLoad() != Prefs::SwitchOff) obj.rs->foldAllAtoms();
	return Command::Success;
}

// Fold molecules
int Command::function_CA_FOLDMOLECULES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Fold Molecules");
	obj.rs->foldAllMolecules();
	obj.rs->endUndoState();
	return Command::Success;
}

// Convert fractional coordinates to real coordinates
int Command::function_CA_FRACTOREAL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Convert fractional to real coordinates");
	obj.rs->fracToReal();
	obj.rs->endUndoState();
	return Command::Success;
}

// Do crystal packing in model
int Command::function_CA_PACK(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->parent()->inputFile() == NULL)
	{
		obj.rs->beginUndoState("Pack Cell");
		obj.rs->pack();
		obj.rs->endUndoState();
	}
	else if (prefs.packOnLoad() != Prefs::SwitchOff) obj.rs->pack();
	return Command::Success;
}

// Print cell information ('printcell')
int Command::function_CA_PRINTCELL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	msg.print("Unit cell type for model '%s' is %s\n", obj.rs->name(), Cell::cellType(obj.rs->cell()->type()));
	if (obj.rs->cell()->type() != Cell::NoCell) obj.rs->cell()->print();
	return Command::Success;
}

// Replicate cell ('replicate <negx negy negz> <posx posy posz>')
int Command::function_CA_REPLICATE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Replicate cell");
	obj.rs->replicateCell(c->arg3d(0), c->arg3d(3));
	obj.rs->endUndoState();
	return Command::Success;
}

// Scale cell and molecule COGs ('scale <x y z>')
int Command::function_CA_SCALE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Scale cell");
	obj.rs->scaleCell(c->arg3d(0));
	return Command::Success;
}

// Set/create unit cell ('cell <a b c> <alpha beta gamma>')
int Command::function_CA_CELL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->setCell(c->arg3d(0), c->arg3d(3));
	obj.rs->endUndoState();
	obj.rs->calculateDensity();
	return Command::Success;
}

// Set/create unit cell ('cellaxes <ax ay az> <bx by bz> <cx cy cz>')
int Command::function_CA_CELLAXES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Mat3<double> mat;
	mat.rows[0] = c->arg3d(0);
	mat.rows[1] = c->arg3d(3);
	mat.rows[2] = c->arg3d(6);
	obj.rs->beginUndoState("Set cell");
	obj.rs->setCell(mat);
	obj.rs->endUndoState();
	obj.rs->calculateDensity();
	return Command::Success;
}

// Remove unit cell
int Command::function_CA_NOCELL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Remove cell");
	obj.rs->removeCell();
	obj.rs->endUndoState();
	return Command::Success;
}

// Set parameter of unit cell
int Command::function_CA_SETCELL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Cell::CellParameter cp = Cell::cellParameter(c->argc(0));
	if (cp != Cell::nCellParameters) obj.rs->cell()->setParameter(cp, c->argd(1));
	return Command::Success;
}

// Set spacegroup
int Command::function_CA_SPACEGROUP(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// If argument passed is an integer, set by integer. If a character, search by spacegroup name
	if (c->argt(0) == VTypes::IntegerData) obj.rs->setSpacegroup(c->argi(0));
	else
	{
		msg.print("Searching for spacegroup '%s'...",c->argc(0));
		int sg = spacegroups.spacegroup(c->argc(0));
		if (sg == 0) msg.print(" not found - no spacegroup set.\n");
		else msg.print(" found, id = %i.\n",sg);
		obj.rs->setSpacegroup(sg);
	}
	return Command::Success;
}
