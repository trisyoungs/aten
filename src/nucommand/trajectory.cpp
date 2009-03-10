/*
	*** Trajectory functions
	*** src/parser/trajectory.cpp
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

#include "command/commands.h"
#include "main/aten.h"
#include "model/model.h"
#include "gui/gui.h"

// Finalise current trajectory frame
bool NuCommand::function_Finaliseframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.rs == obj.m)
	{
		msg.print( "Current model does not appear to be a trajectory frame.\n");
		return FALSE;
	}
	// Do various necessary calculations
	if (prefs.coordsInBohr()) obj.rs->bohrToAngstrom();
	obj.rs->renumberAtoms();
	obj.rs->calculateViewMatrix();
	obj.rs->resetView();
	obj.rs->calculateMass();
	obj.rs->calculateDensity();
	obj.rs->selectNone();
	obj.rs->changeLog.reset();
	obj.rs->changeLog.updateSavePoint();
	obj.rs->setFilter(NULL);
	obj.rs->setFilename("frame");
	obj.rs->enableUndoRedo();
	//if (frame->cell()->type() != Cell::NoCell) frame->cell()->print();
	return TRUE;
}

// Skip to first frame ('firstframe')
bool NuCommand::function_Firstframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekFirstFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	return TRUE;
}

// Skip to last frame ('lastframe')
bool NuCommand::function_Lastframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekLastFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	return TRUE;
}

// Open and associate trajectory ('loadtrajectory <file>')
bool NuCommand::function_Loadtrajectory(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Filter *f = aten.probeFile(c->argc(0), Filter::TrajectoryImport);
	if (f == NULL) return FALSE;
	return (obj.m->initialiseTrajectory(c->argc(0),f) ? TRUE : FALSE);
}

// Go to next frame ('nextframe')
bool NuCommand::function_Nextframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekNextFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	return TRUE;
}

// Go to previous frame ('prevframe')
bool NuCommand::function_Prevframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekPreviousFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	return TRUE;
}

// Seek to specified frame ('seekframe <n>')
bool NuCommand::function_Seekframe(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekFrame(c->argi(0));
	gui.modelChanged(FALSE, FALSE, FALSE);
	return TRUE;
}
