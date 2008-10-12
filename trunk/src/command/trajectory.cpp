/*
	*** Trajectory command functions
	*** src/command/trajectory.cpp
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
#include "main/aten.h"
#include "model/model.h"
#include "gui/gui.h"

// Finalise current trajectory frame
int Command::function_CA_FINALISEFRAME(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.rs == obj.m)
	{
		msg.print( "Current model does not appear to be a trajectory frame.\n");
		return Command::Fail;
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
	return Command::Success;
}

// Skip to first frame ('firstframe')
int Command::function_CA_FIRSTFRAME(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return Command::Fail;
	}
	obj.m->seekFirstFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
// 	c->parent()->setModelVariables("",obj.m); TGAY
	return Command::Success;
}

// Skip to last frame ('lastframe')
int Command::function_CA_LASTFRAME(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return Command::Fail;
	}
	obj.m->seekLastFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
// 	c->parent()->setModelVariables("",obj.m); TGAY
	return Command::Success;
}

// Open and associate trajectory ('loadtrajectory <file>')
int Command::function_CA_LOADTRAJECTORY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Filter *f = aten.probeFile(c->argc(0), Filter::TrajectoryImport);
	if (f == NULL) return Command::Fail;
	return (obj.m->initialiseTrajectory(c->argc(0),f) ? Command::Success : Command::Fail);
}

// Go to next frame ('nextframe')
int Command::function_CA_NEXTFRAME(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return Command::Fail;
	}
	obj.m->seekNextFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
// 	c->parent()->setModelVariables("",obj.m); TGAY
	return Command::Success;
}

// Go to previous frame ('prevframe')
int Command::function_CA_PREVFRAME(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return Command::Fail;
	}
	obj.m->seekPreviousFrame();
	gui.modelChanged(FALSE, FALSE, FALSE);
// 	c->parent()->setModelVariables("",obj.m); TGAY
	return Command::Success;
}

// Seek to specified frame ('seekframe <n>')
int Command::function_CA_SEEKFRAME(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		msg.print("No trajectory associated to model '%s'.\n",obj.m->name());
		return Command::Fail;
	}
	obj.m->seekFrame(c->argi(0));
	gui.modelChanged(FALSE, FALSE, FALSE);
// 	c->parent()->setModelVariables("",obj.m); TGAY
	return Command::Success;
}
