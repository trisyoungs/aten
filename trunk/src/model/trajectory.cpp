/*
	*** Model trajectory functions
	*** src/model/trajectory.cpp
	Copyright T. Youngs 2007-2015

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
// #include "base/prefs.h"
#include "base/progress.h"	
// #include <fstream>

ATEN_USING_NAMESPACE

// Set the format of the trajectory
void Model::setTrajectoryFilter(Tree* filter)
{
	trajectoryFilter_ = filter;
}

// Return whether a trajectory for this model exists
bool Model::hasTrajectory() const
{
	if (trajectoryFramesAreCached_) return (trajectoryFrames_.nItems() != 0);
	else return (nTrajectoryFileFrames_ != 0);
}

// Return whether the trajectory is cached (if there is one)
bool Model::trajectoryIsCached() const
{
	return trajectoryFramesAreCached_;
}

// Return the current frame pointer
Model* Model::trajectoryCurrentFrame() const
{
	return trajectoryCurrentFrame_;
}

// Return pointer to specified frame number
Model* Model::trajectoryFrame(int n)
{
	Model* frame = NULL;
	if (trajectoryFramesAreCached_)
	{
		if ((n < 0) || (n >= nTrajectoryFrames())) Messenger::print("Frame %i is out of range for trajectory associated to model '%s'.\n", n, name_.get());
		else frame = trajectoryFrames_[n];
	}
	else Messenger::print("Trajectory for model '%s' is not cached: individual frames not available.\n", name_.get());
	return frame;
}

// Return the total number of frames in the trajectory (file or cached)
int Model::nTrajectoryFrames() const
{
	return (trajectoryFramesAreCached_ ? trajectoryFrames_.nItems() : nTrajectoryFileFrames_);
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
	trajectoryFrames_.clear();
	// Close file in parser
	trajectoryParser_.closeFiles();
	if (trajectoryOffsets_ != NULL) delete[] trajectoryOffsets_;
	trajectoryOffsets_ = NULL;
	trajectoryHighestFrameOffset_ = -1;
	trajectoryFilename_ = "Unnamed";
	nTrajectoryFileFrames_ = 0;
	trajectoryFrameIndex_ = -1;
	trajectoryFramesAreCached_ = FALSE;
	trajectoryFilter_ = NULL;
	trajectoryCurrentFrame_ = NULL;
	trajectoryHeaderFunction_ = NULL;
	trajectoryFrameFunction_ = NULL;
	trajectoryPlaying_ = FALSE;
	Messenger::exit("Model::clearTrajectory");
}

// Initialise trajectory
bool Model::initialiseTrajectory(const char* fname, Tree* f)
{
	// Associate the supplied trajectory file with the model
	Messenger::enter("Model::initialiseTrajectory");
	bool success;
	
	// Delete old frames and unset old file
	clearTrajectory();
	
	// Open the specified file
	if (!trajectoryParser_.openInput(fname))
	{
		Messenger::print("Trajectory file '%s' couldn't be opened.\n",fname);
		clearTrajectory();
		Messenger::exit("Model::initialiseTrajectory");
		return FALSE;
	}
	
	// Associate the file with the trajectory, and grab header and frame read functions
	trajectoryFilename_ = fname;
	trajectoryFilter_ = f;
	trajectoryHeaderFunction_ = f->filter.trajectoryHeaderFunction();
	trajectoryFrameFunction_ = f->filter.trajectoryFrameFunction();
	if (trajectoryHeaderFunction_ == NULL)
	{
		Messenger::print("Error initialising trajectory: Filter '%s' contains no 'int readHeader()' function.\n", trajectoryFilter_->filter.name());
		clearTrajectory();
		Messenger::exit("Model::initialiseTrajectory");
		return FALSE;
	}
	if (trajectoryFrameFunction_ == NULL)
	{
		Messenger::print("Error initialising trajectory: Filter '%s' contains no 'int readFrame()' function.\n", trajectoryFilter_->filter.name());
		clearTrajectory();
		Messenger::exit("Model::initialiseTrajectory");
		return FALSE;
	}
	ReturnValue rv;
	
	// Execute main filter program (in case there are dialog options etc.)
	if (!f->execute(rv))
	{
		Messenger::exit("Model::initialiseTrajectory");
		return FALSE;
	}

	// Read header
	if (!trajectoryHeaderFunction_->execute(&trajectoryParser_, rv) || (rv.asInteger() != 1))
	{
		Messenger::print("Error reading header of trajectory file.\n");
		clearTrajectory();
		Messenger::exit("Model::initialiseTrajectory");
		return FALSE;
	}
	
	// Store this file position, since it should represent the start of the frame data
	std::streampos firstframe = trajectoryParser_.tellg();

	// Determine frame size and number of frames in file
	Messenger::print(Messenger::Verbose, "Testing trajectory frame read...\n");
	//printf("Initialised config\n");
	Model* newframe = addTrajectoryFrame();
	setRenderSource(Model::TrajectorySource);

	// Read first frame
	if (!trajectoryFrameFunction_->execute(&trajectoryParser_, rv) || (rv.asInteger() != 1))
	{
		Messenger::print("Error testing frame read from trajectory.\n");
		clearTrajectory();
		setRenderSource(Model::ModelSource);
		Messenger::exit("Model::initialiseTrajectory");
		return FALSE;
	}
	newframe->enableUndoRedo();
	std::streampos secondframe = trajectoryParser_.tellg();
	trajectoryFrameSize_ = secondframe - firstframe;
	if ((trajectoryFrameSize_/1024) < 10) Messenger::print("Single frame is %i bytes.\n", trajectoryFrameSize_);
	else Messenger::print("Single frame is (approximately) %i kb.\n", trajectoryFrameSize_/1024);
	trajectoryParser_.seekg(0, std::ios::end);
	std::streampos endoffile = trajectoryParser_.tellg();
	nTrajectoryFileFrames_ = (endoffile - firstframe) / trajectoryFrameSize_;

	// Skip back to end of first frame ready to read in next frame...
	trajectoryParser_.seekg(secondframe);

	// Pre-Cache frame(s)
	Messenger::print("Successfully associated trajectory.\n"); 
	Messenger::print("Number of frames in file : %i\n", nTrajectoryFileFrames_);
	trajectoryFrameIndex_ = 0;

	// If we are caching the trajectory, read in all remaining frames here. Otherwise, we're happy with just the first
	Messenger::print("Estimated trajectory size is %i kb, cache limit = %i kb\n", nTrajectoryFileFrames_ * trajectoryFrameSize_/1024, prefs.cacheLimit());
	if ((nTrajectoryFileFrames_ * trajectoryFrameSize_)/1024 < prefs.cacheLimit())
	{
		Messenger::print("Caching all frames from trajectory...\n");
 		int pid = progress.initialise("Caching Frames", nTrajectoryFileFrames_);
		// Read all frames from trajectory file
		for (int n=1; n<nTrajectoryFileFrames_; n++)
		{
 			if (!progress.update(pid,n)) break;
			newframe = addTrajectoryFrame();
			success = trajectoryFrameFunction_->execute(&trajectoryParser_, rv);
			if ((!success) || (rv.asInteger() != 1))
			{
				removeTrajectoryFrame(newframe);
				Messenger::print("Error during read of frame %i.\n", n);
				break;
			}
			newframe->enableUndoRedo();
		}
 		progress.terminate(pid);
		nTrajectoryFileFrames_ = 0;
		trajectoryFramesAreCached_ = TRUE;
		trajectoryParser_.closeFiles();
		Messenger::print("Cached %i frames from file.\n", nTrajectoryFrames());
	}
	else
	{
		Messenger::print( "Trajectory will not be cached in memory.\n");
		trajectoryOffsets_ = new std::streampos[nTrajectoryFileFrames_+10];
		trajectoryOffsets_[0] = firstframe;
		trajectoryOffsets_[1] = secondframe;
		trajectoryHighestFrameOffset_ = 1;
	}
	Messenger::exit("Model::initialiseTrajectory");
	return TRUE;
}

// Add frame to trajectory
Model* Model::addTrajectoryFrame()
{
	Messenger::enter("Model::addFrame");	
	Model* newframe = trajectoryFrames_.add();
	newframe->setType(Model::TrajectoryFrameType);
	// Set trajectoryCurrentFrame_ here (always points to the last added frame)
	trajectoryCurrentFrame_ = newframe;
	newframe->setParent(this);
	if (trajectoryFrames_.nItems() > 1) trajectoryFramesAreCached_ = TRUE;
	trajectoryFrameIndex_ = trajectoryFrames_.nItems()-1;
	Messenger::exit("Model::addFrame");	
	return newframe;
}

// Delete cached frame from trajectory
void Model::removeTrajectoryFrame(Model* xframe)
{
	// Delete the specified frame from the trajectory structure
	Messenger::enter("Model::removeTrajectoryFrame");
	if (xframe == trajectoryCurrentFrame_) trajectoryCurrentFrame_ = (xframe->next == NULL ? xframe->prev : xframe->next);
	trajectoryFrames_.remove(xframe);
	Messenger::exit("Model::removeTrajectoryFrame");
}

// Seek to first frame
void Model::seekFirstTrajectoryFrame()
{
	// Seek to the first frame in the trajectory
	Messenger::enter("Model::seekFirstTrajectoryFrame");
	// Check that a trajectory exists!
	if (nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory is available.\n");
		Messenger::exit("Model::seekFirstTrajectoryFrame");
		return;
	}
	if (trajectoryFrameIndex_ == 0)
	{
		Messenger::print("Already at start of trajectory.\n");
		Messenger::exit("Model::seekFirstTrajectoryFrame");
		return;
	}
	if (trajectoryFramesAreCached_)
	{
		trajectoryCurrentFrame_ = trajectoryFrames_.first();
		trajectoryFrameIndex_ = 0;
	}
	else seekTrajectoryFrame(0);
	changeLog.add(Log::Camera);
	Messenger::exit("Model::seekFirstTrajectoryFrame");
}

// Seek to next frame
void Model::seekNextTrajectoryFrame()
{
	// Seek to the next frame in the trajectory
	Messenger::enter("Model::seekNextTrajectoryFrame");
	// Check that a trajectory exists!
	if (nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory is available.\n");
		Messenger::exit("Model::seekNextTrajectoryFrame");
		return;
	}
	if (trajectoryFrameIndex_ == nTrajectoryFrames()-1)
	{
		Messenger::print("Already at end of trajectory (frame %i).\n", trajectoryFrameIndex_+1);
		Messenger::exit("Model::seekNextTrajectoryFrame");
		return;
	}
	if (trajectoryFramesAreCached_)
	{
		trajectoryFrameIndex_++;
		trajectoryCurrentFrame_ = trajectoryCurrentFrame_->next;
	}
	else seekTrajectoryFrame(trajectoryFrameIndex_+1);
	changeLog.add(Log::Camera);
	//printf("Frame = %p, parent = %p (model = %p)\n",trajectoryCurrentFrame_,trajectoryCurrentFrame_->trajectoryParent_,this);
	Messenger::exit("Model::seekNextTrajectoryFrame");
}

// Seek to previous frame
void Model::seekPreviousTrajectoryFrame()
{
	// Seek to the previous frame in the trajectory
	Messenger::enter("Model::seekPreviousTrajectoryFrame");
	// Check that a trajectory exists!
	if (nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory is available.\n");
		Messenger::exit("Model::seekPreviousTrajectoryFrame");
		return;
	}
	if (trajectoryFrameIndex_ == 0)
	{
		Messenger::print("Already at start of trajectory.\n");
		Messenger::exit("Model::seekPreviousTrajectoryFrame");
		return;
	}
	if (trajectoryFramesAreCached_)
	{
		trajectoryFrameIndex_--;
		trajectoryCurrentFrame_ = trajectoryCurrentFrame_->prev;
	}
	else seekTrajectoryFrame(trajectoryFrameIndex_-1);
	changeLog.add(Log::Camera);
	Messenger::exit("Model::seekPreviousTrajectoryFrame");
}

// Seek to last frame
void Model::seekLastTrajectoryFrame()
{
	// Seek to the last frame in the trajectory
	Messenger::enter("Model::seekLastTrajectoryFrame");
	// Check that a trajectory exists!
	if (nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory is available.\n");
		Messenger::exit("Model::seekLastTrajectoryFrame");
		return;
	}
	if (trajectoryFrameIndex_ == nTrajectoryFrames()-1)
	{
		Messenger::print("Already at end of trajectory (frame %i).\n", trajectoryFrameIndex_+1);
		Messenger::exit("Model::seekNextTrajectoryFrame");
		return;
	}
	if (trajectoryFramesAreCached_)
	{
		trajectoryCurrentFrame_ = trajectoryFrames_.last();
		trajectoryFrameIndex_ = nTrajectoryFrames()-1;
	}
	else seekTrajectoryFrame(nTrajectoryFrames()-1);
	changeLog.add(Log::Camera);
	Messenger::exit("Model::seekLastTrajectoryFrame");
}

// Seek to specified frame
void Model::seekTrajectoryFrame(int frameno, bool quiet)
{
	// Seek to the previous frame in the trajectory
	Messenger::enter("Model::seekTrajectoryFrame");
	// Check that a trajectory exists!
	if (nTrajectoryFrames() == 0)
	{
		Messenger::print("No trajectory is available.\n");
		Messenger::exit("Model::seekTrajectoryFrame");
		return;
	}
	if ((frameno < 0) || (frameno > nTrajectoryFrames()-1))
	{
		Messenger::print("Frame %i is out of range for current trajectory (which has %i frames).\n", frameno+1, nTrajectoryFrames());
		Messenger::exit("Model::seekTrajectoryFrame");
		return;
	}
	if (trajectoryFrameIndex_ == frameno)
	{
		Messenger::print("Already at specified frame (%i).\n", frameno+1);
		Messenger::exit("Model::seekTrajectoryFrame");
		return;
	}
	if (trajectoryFramesAreCached_) trajectoryCurrentFrame_ = trajectoryFrames_[frameno];
	else
	{
		// Seek to specified frame in file
		// If the desired frame is less than (or equal to) the highest stored offset, just seek to and read it.
		// Otherwise we need to read up to the specified frame number, storing offsets as we go.
		bool success;
		ReturnValue rv;
		if (frameno <= trajectoryHighestFrameOffset_)
		{
			trajectoryCurrentFrame_->clear();
			trajectoryParser_.seekg(trajectoryOffsets_[frameno]);
			success = trajectoryFrameFunction_->execute(&trajectoryParser_, rv);
			if ((!success) || (rv.asInteger() != 1))
			{
				Messenger::print("Failed to seek to frame %i.\n", frameno+1);
				Messenger::exit("Model::seekTrajectoryFrame");
				return;
			}
			// If this was the highest offset stored, the file position now corresponds to the next frame
			if ((frameno == trajectoryHighestFrameOffset_) && (trajectoryHighestFrameOffset_ < nTrajectoryFileFrames_))
			{
				trajectoryOffsets_[frameno+1] = trajectoryParser_.tellg();
				trajectoryHighestFrameOffset_ ++;
			}
		}
		else
		{
			// Seek to last frame position stored
			trajectoryParser_.seekg(trajectoryOffsets_[trajectoryHighestFrameOffset_]);
			// Skip consecutive frames until we get to the desired point, storing pointers as we go.
			for (trajectoryFrameIndex_ = trajectoryHighestFrameOffset_; trajectoryFrameIndex_ <= frameno; trajectoryFrameIndex_++)
			{
				trajectoryCurrentFrame_->clear();
				// Read a frame, and store its stream position
				ReturnValue rv;
				success = trajectoryFrameFunction_->execute(&trajectoryParser_, rv);
				if ((!success) || (rv.asInteger() != 1))
				{
					Messenger::print("Failed to read frame %i in trajectory.\n",trajectoryFrameIndex_+1);
					Messenger::exit("Model::seekTrajectoryFrame");
					return;
				}
				// Apply styling from parent (if required)
				if (trajectoryPropagateParentStyle_) trajectoryCurrentFrame_->copyAtomStyle(parent_);
				// Store new frame offset
				trajectoryHighestFrameOffset_++;
				trajectoryOffsets_[trajectoryHighestFrameOffset_] = trajectoryParser_.tellg();
			}
		}
		trajectoryCurrentFrame_->changeLog.add(Log::Camera);
		trajectoryCurrentFrame_->changeLog.add(Log::Structure);
	}
	trajectoryFrameIndex_ = frameno;
	if (!quiet) Messenger::print("Seek to frame %i\n", trajectoryFrameIndex_+1);
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
		Messenger::print("Internal Error: NULL model pointer passed to Model::trajectoryCopyAtomStyle.\n");
		Messenger::exit("Model::trajectoryCopyAtomStyle");
		return;
	}
	int pid = progress.initialise("Applying style to trajectory frames...", trajectoryFrames_.nItems());
	for (Model* m = trajectoryFrames_.first(); m != NULL; m = m->next)
	{
		if (m != source) m->copyAtomStyle(source);
		progress.update(pid);
	}
	progress.terminate(pid);
	Messenger::exit("Model::trajectoryCopyAtomStyle");
}
