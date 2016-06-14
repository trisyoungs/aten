/*
	*** Model trajectory functions
	*** src/model/trajectory.cpp
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

#include "model/model.h"
#include "parser/tree.h"
#include "plugins/interfaces/fileplugin.h"

ATEN_USING_NAMESPACE

// Set the plugin for the trajectory
void Model::setTrajectoryPlugin(FilePluginInterface* plugin)
{
	trajectoryPlugin_ = plugin;
}

// Return whether a trajectory for this model exists
bool Model::hasTrajectory() const
{
	return (trajectoryFrames_.nItems() != 0);
}

// Return whether the trajectory is cached (if there is one)
bool Model::isTrajectoryCached() const
{
	return trajectoryFramesAreCached_;
}

// Return the current frame pointer
Model* Model::trajectoryCurrentFrame() const
{
	return trajectoryCurrentFrame_;
}

// Return pointer to trajectory frames
Model* Model::trajectoryFrames() const
{
	return trajectoryFrames_.first();
}

// Return pointer to specified frame number
Model* Model::trajectoryFrame(int n)
{
	Model* frame = NULL;
	if (trajectoryFramesAreCached_)
	{
		if ((n < 0) || (n >= nTrajectoryFrames())) Messenger::print("Frame %i is out of range for trajectory associated to model '%s'.", n, qPrintable(name_));
		else frame = trajectoryFrames_[n];
	}
	else Messenger::print("Trajectory for model '%s' is not cached: individual frames not available.", qPrintable(name_));
	return frame;
}

// Return the total number of frames in the trajectory (file or cached)
int Model::nTrajectoryFrames() const
{
	if (trajectoryFramesAreCached_) return trajectoryFrames_.nItems();
	else if (trajectoryPlugin_) return trajectoryPlugin_->nDataParts();
	else return 0;
}

// Return the current integer frame position
int Model::trajectoryFrameIndex() const
{
	return trajectoryFrameIndex_;
}

// Clear trajectory
void Model::clearTrajectory()
{
	Messenger::enter("Model::clearTrajectory");

	renderSource_ = Model::ModelSource;

	if (trajectoryPlugin_ && (!trajectoryFramesAreCached_)) trajectoryPlugin_->closeFiles();

	trajectoryFrames_.clear();

	trajectoryFrameIndex_ = -1;
	trajectoryFramesAreCached_ = false;
	trajectoryCurrentFrame_ = NULL;
	trajectoryPlaying_ = false;
	trajectoryPlugin_ = NULL;

	Messenger::exit("Model::clearTrajectory");
}

// Add frame to trajectory
Model* Model::addTrajectoryFrame()
{
	Messenger::enter("Model::addFrame");	

	Model* newFrame = trajectoryFrames_.add();
	newFrame->setType(Model::TrajectoryFrameType);

	// Set trajectoryCurrentFrame_ here (always points to the last added frame)
	trajectoryCurrentFrame_ = newFrame;
	newFrame->setParent(this);
	if (trajectoryFrames_.nItems() > 1) trajectoryFramesAreCached_ = true;
	trajectoryFrameIndex_ = trajectoryFrames_.nItems()-1;

	Messenger::exit("Model::addFrame");	
	return newFrame;
}

// Delete cached frame from trajectory
void Model::removeTrajectoryFrame(Model* frame)
{
	Messenger::enter("Model::removeTrajectoryFrame");

	if (frame == trajectoryCurrentFrame_) trajectoryCurrentFrame_ = (frame->next == NULL ? frame->prev : frame->next);
	trajectoryFrames_.remove(frame);

	Messenger::exit("Model::removeTrajectoryFrame");
}

// Seek to first frame
void Model::seekFirstTrajectoryFrame()
{
	Messenger::enter("Model::seekFirstTrajectoryFrame");

	// Check that a trajectory exists!
	if (hasTrajectory() == 0)
	{
		Messenger::print("No trajectory is available.");
		Messenger::exit("Model::seekFirstTrajectoryFrame");
		return;
	}

	seekTrajectoryFrame(0);

	Messenger::exit("Model::seekFirstTrajectoryFrame");
}

// Seek to next frame
void Model::seekNextTrajectoryFrame()
{
	Messenger::enter("Model::seekNextTrajectoryFrame");

	// Check that a trajectory exists!
	if (hasTrajectory() == 0)
	{
		Messenger::print("No trajectory is available.");
		Messenger::exit("Model::seekNextTrajectoryFrame");
		return;
	}

	seekTrajectoryFrame(trajectoryFrameIndex_+1);

	Messenger::exit("Model::seekNextTrajectoryFrame");
}

// Seek to previous frame
void Model::seekPreviousTrajectoryFrame()
{
	Messenger::enter("Model::seekPreviousTrajectoryFrame");

	// Check that a trajectory exists!
	if (hasTrajectory() == 0)
	{
		Messenger::print("No trajectory is available.");
		Messenger::exit("Model::seekPreviousTrajectoryFrame");
		return;
	}

	seekTrajectoryFrame(trajectoryFrameIndex_-1);

	Messenger::exit("Model::seekPreviousTrajectoryFrame");
}

// Seek to last frame
void Model::seekLastTrajectoryFrame()
{
	Messenger::enter("Model::seekLastTrajectoryFrame");

	// Check that a trajectory exists!
	if (hasTrajectory() == 0)
	{
		Messenger::print("No trajectory is available.");
		Messenger::exit("Model::seekLastTrajectoryFrame");
		return;
	}

	seekTrajectoryFrame(nTrajectoryFrames()-1);

	Messenger::exit("Model::seekLastTrajectoryFrame");
}

// Seek to specified frame
void Model::seekTrajectoryFrame(int frameno, bool quiet)
{
	Messenger::enter("Model::seekTrajectoryFrame");

	// Check that a trajectory exists!
	if (hasTrajectory() == 0)
	{
		Messenger::print("No trajectory is available.");
		Messenger::exit("Model::seekTrajectoryFrame");
		return;
	}
	if ((frameno < 0) || (frameno > nTrajectoryFrames()-1))
	{
		Messenger::print("Frame %i is out of range for current trajectory (which has %i frames).", frameno+1, nTrajectoryFrames());
		Messenger::exit("Model::seekTrajectoryFrame");
		return;
	}
	if (trajectoryFrameIndex_ == frameno)
	{
		Messenger::print("Already at specified frame (%i).", frameno+1);
		Messenger::exit("Model::seekTrajectoryFrame");
		return;
	}
	if (trajectoryFramesAreCached_) trajectoryCurrentFrame_ = trajectoryFrames_[frameno];
	else
	{
		if (!trajectoryPlugin_)
		{
			printf("Fatal Error - Trajectory is not cached, but no plugin set.\n");
			Messenger::exit("Model::seekTrajectoryFrame");
			return;
		}

		// Clear the current trajectory frame (should be the only one we have) and use the importPart() function of the plugin.
		trajectoryCurrentFrame_->clear();
		trajectoryPlugin_->setTargetModel(this);
		trajectoryPlugin_->setTargetFrame(trajectoryCurrentFrame_);
		trajectoryPlugin_->importPart(frameno);

		trajectoryCurrentFrame_->logChange(Log::Structure);
	}
	trajectoryFrameIndex_ = frameno;
	if (!quiet) Messenger::print("Seek to frame %i", trajectoryFrameIndex_+1);

	Messenger::exit("Model::seekTrajectoryFrame");
}

// Return whether to propagate atom sty	les and colours from parent model to trajectory frames
bool Model::trajectoryPropagateParentStyle()
{
	return trajectoryPropagateParentStyle_;
}

// Set whether to propagate atom styles and colours from parent model to trajectory frames
void Model::setTrajectoryPropagateParentStyle(bool b)
{
	trajectoryPropagateParentStyle_ = b;
}

// Copy style of the supplied model to all trajectory frames
void Model::trajectoryCopyAtomStyle(Model* source)
{
	Messenger::enter("Model::trajectoryCopyAtomStyle");
	if (source == NULL)
	{
		Messenger::print("Internal Error: NULL model pointer passed to Model::trajectoryCopyAtomStyle.");
		Messenger::exit("Model::trajectoryCopyAtomStyle");
		return;
	}

	Task* task = Messenger::initialiseTask("Applying style to trajectory frames...", trajectoryFrames_.nItems());
	for (Model* m = trajectoryFrames_.first(); m != NULL; m = m->next)
	{
		if (m != source) m->copyAtomStyle(source);
		Messenger::incrementTaskProgress(task);
	}
	Messenger::terminateTask(task);

	Messenger::exit("Model::trajectoryCopyAtomStyle");
}
