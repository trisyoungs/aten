/*
	*** Trajectory Commands
	*** src/nucommand/trajectory.cpp
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

#include "nucommand/commands.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "model/model.h"
#include "gui/gui.h"

// Finalise current trajectory frame
bool NuCommand::function_FinaliseFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
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
	rv.reset();
	return TRUE;
}

// Skip to first frame ('firstframe')
bool NuCommand::function_FirstFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekFirstFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	rv.reset();
	return TRUE;
}

// Skip to last frame ('lastframe')
bool NuCommand::function_LastFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekLastFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	rv.reset();
	return TRUE;
}

// Open and associate trajectory ('loadtrajectory <file>')
bool NuCommand::function_LoadTrajectory(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Tree *filter = aten.probeFile(c->argc(0), FilterData::TrajectoryImport);
	if (filter == NULL) return FALSE;
	rv.reset();
	return (obj.m->initialiseTrajectory(c->argc(0),filter) ? TRUE : FALSE);
}

// Go to next frame ('nextframe')
bool NuCommand::function_NextFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekNextFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	rv.reset();
	return TRUE;
}

// Go to previous frame ('prevframe')
bool NuCommand::function_PrevFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekPreviousFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
	rv.reset();
	return TRUE;
}

// Seek to specified frame ('seekframe <n>')
bool NuCommand::function_SeekFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return FALSE;
	}
	obj.m->seekFrame(c->argi(0));
	gui.modelChanged(FALSE, FALSE, FALSE);
	rv.reset();
	return TRUE;
}
