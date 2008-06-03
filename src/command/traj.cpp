/*
	*** Trajectory command functions
	*** src/command/traj.cpp
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

#include "command/commands.h"
#include "base/master.h"
#include "parse/filter.h"
#include "model/model.h"
#include "gui/gui.h"

// Finalise current trajectory frame
int CommandData::function_CA_FINALISEFRAME(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.rs == obj.m)
	{
		msg(Debug::None, "Current model does not appear to be a trajectory frame.\n");
		return CR_FAIL;
	}
	// Do various necessary calculations
	if (prefs.coordsInBohr()) obj.rs->bohrToAngstrom();
	obj.rs->renumberAtoms();
	obj.rs->calculateViewMatrix();
	obj.rs->resetView();
	obj.rs->calculateMass();
	obj.rs->calculateDensity();
	obj.rs->selectNone();
	obj.rs->resetLogs();
	obj.rs->updateSavePoint();
	//if (frame->cell()->type() != Cell::NoCell) frame->cell()->print();
	return CR_SUCCESS;
}

// Skip to first frame ('firstframe')
int CommandData::function_CA_FIRSTFRAME(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.m->totalFrames() == 0)
	{
		msg(Debug::None,"No trajectory associated to model '%s'.\n",obj.m->name());
		return CR_FAIL;
	}
	obj.m->seekFirstFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	return CR_SUCCESS;
}

// Skip to last frame ('lastframe')
int CommandData::function_CA_LASTFRAME(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.m->totalFrames() == 0)
	{
		msg(Debug::None,"No trajectory associated to model '%s'.\n",obj.m->name());
		return CR_FAIL;
	}
	obj.m->seekLastFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	return CR_SUCCESS;
}

// Open and associate trajectory ('loadtrajectory <file>')
int CommandData::function_CA_LOADTRAJECTORY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Filter *f = master.probeFile(c->argc(0), Filter::TrajectoryImport);
	if (f == NULL) return CR_FAIL;
	return (obj.m->initialiseTrajectory(c->argc(0),f) ? CR_SUCCESS : CR_FAIL);
}

// Go to next frame ('nextframe')
int CommandData::function_CA_NEXTFRAME(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.m->totalFrames() == 0)
	{
		msg(Debug::None,"No trajectory associated to model '%s'.\n",obj.m->name());
		return CR_FAIL;
	}
	obj.m->seekNextFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	return CR_SUCCESS;
}

// Go to previous frame ('prevframe')
int CommandData::function_CA_PREVFRAME(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.m->totalFrames() == 0)
	{
		msg(Debug::None,"No trajectory associated to model '%s'.\n",obj.m->name());
		return CR_FAIL;
	}
	obj.m->seekPreviousFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	return CR_SUCCESS;
}

// Seek to specified frame ('seekframe <n>')
int CommandData::function_CA_SEEKFRAME(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.m->totalFrames() == 0)
	{
		msg(Debug::None,"No trajectory associated to model '%s'.\n",obj.m->name());
		return CR_FAIL;
	}
	obj.m->seekFrame(c->argi(0));
	gui.modelChanged(FALSE, FALSE, FALSE);
	return CR_SUCCESS;
}
