/*
	*** Trajectory Commands
	*** src/command/trajectory.cpp
	Copyright T. Youngs 2007-2018

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
#include "gui/mainwindow.h"
#include "gui/opentrajectory.h"

ATEN_USING_NAMESPACE

// Add new frame to the current model's trajectory
bool Commands::function_AddFrame(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->addTrajectoryFrame();
	obj.m->setRenderSource(Model::TrajectorySource);
	if (c->hasArg(0)) obj.rs()->setName(c->argc(0));
	rv.set(VTypes::ModelData, obj.rs());
	return true;
}

// Clear any trajectory data in the current model
bool Commands::function_ClearTrajectory(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.rs() != obj.m)
	{
		Messenger::print("Current model is a trajectory frame - resetting to the parent model...");
		obj.m->setRenderSource(Model::ModelSource);
	}
	obj.m->clearTrajectory();
	rv.reset();
	return true;
}

// Skip to first frame ('firstframe')
bool Commands::function_FirstFrame(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory associated to model '%s'.", qPrintable(obj.m->name()));
		return false;
	}
	obj.m->seekFirstTrajectoryFrame();
	rv.reset();
	return true;
}

// Skip to last frame ('lastframe')
bool Commands::function_LastFrame(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory associated to model '%s'.", qPrintable(obj.m->name()));
		return false;
	}
	obj.m->seekLastTrajectoryFrame();
	rv.reset();
	return true;
}

// Open and associate trajectory ('loadtrajectory <file>')
bool Commands::function_LoadTrajectory(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	bool result = aten_.importTrajectory(obj.m, c->argc(0), NULL, FilePluginStandardImportOptions());

	rv.set(result);
	return true;
}

// Go to next frame ('nextframe')
bool Commands::function_NextFrame(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory associated to model '%s'.", qPrintable(obj.m->name()));
		return false;
	}
	obj.m->seekNextTrajectoryFrame();
	rv.reset();
	return true;
}

// Go to previous frame ('prevframe')
bool Commands::function_PrevFrame(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory associated to model '%s'.", qPrintable(obj.m->name()));
		return false;
	}
	obj.m->seekPreviousTrajectoryFrame();
	rv.reset();
	return true;
}

// Seek to specified frame ('seekframe <n>')
bool Commands::function_SeekFrame(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.m->nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory associated to model '%s'.", qPrintable(obj.m->name()));
		return false;
	}
	obj.m->seekTrajectoryFrame(c->argi(0)-1);
	rv.reset();
	return true;
}
