/*
	*** Model trajectory functions
	*** src/model/trajectory.cpp
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

#include "base/master.h"
#include "parse/filter.h"
#include "model/model.h"
#include <fstream>

// Set parent model of trajectory
void Model::setTrajectoryParent(Model *m)
{
	trajectoryParent_ = m;
}

// Return parent model of trajectory
Model *Model::trajectoryParent()
{
	return trajectoryParent_;
}

// Set the format of the trajectory
void Model::setTrajectoryFilter(Filter *f)
{
	trajectoryFilter_ = f;
}

// Return the trajectory file pointer
ifstream *Model::trajectoryFile()
{
	return trajectoryFile_;
}

// Return the current frame pointer
Model *Model::currentFrame()
{
	return currentFrame_;
}

// Return the total number of frames in the trajectory (file or cached)
int Model::totalFrames()
{
	return totalFrames_;
}

// Return the current integer frame position
int Model::framePosition()
{
	return framePosition_;
}

// Clear trajectory
void Model::clearTrajectory()
{
	// Clear frames - can simply delete the master config pointed to by 'frames_head'
	dbgBegin(Debug::Calls,"Model::clearTrajectory");
	frames_.clear();
	if (trajectoryFile_ != NULL)
	{
		trajectoryFile_->close();
		delete trajectoryFile_;
	}
	trajectoryFilename_ = "Unnamed";
	nCachedFrames_ = 0;
	totalFrames_ = 0;
	framePosition_ = 0;
	trajectoryCached_ = FALSE;
	dbgEnd(Debug::Calls,"Model::clearTrajectory");
}

// Initialise trajectory
bool Model::initialiseTrajectory(const char *fname, Filter *f)
{
	// Associate the supplied trajectory file with the model
	dbgBegin(Debug::Calls,"Model::initialiseTrajectory");
	bool success;
	// Delete old frames and unset old file
	if (trajectoryFile_ != NULL) trajectoryFile_->close();
	clearTrajectory();
	// Check that we can open the specified file
	trajectoryFile_ = new ifstream(fname,ios::in);
	if (!trajectoryFile_->good())
	{
		msg(Debug::None,"Trajectory file '%s' couldn't be opened.\n",fname);
		trajectoryFile_->close();
		trajectoryFile_ = NULL;
		trajectoryFilter_ = NULL;
		dbgEnd(Debug::Calls,"Model::initialiseTrajectory");
		return FALSE;
	}
	// Associate the file with the trajectory
	trajectoryFilename_ = fname;
	trajectoryFilter_ = f;
	// Read header
	if (!trajectoryFilter_->execute("",trajectoryFile_,TRUE))
	{
		msg(Debug::None,"Error reading header of trajectory file.\n");
		trajectoryFile_->close();
		trajectoryFile_ = NULL;
		trajectoryFilter_ = NULL;
		dbgEnd(Debug::Calls,"Model::initialiseTrajectory");
		return FALSE;
	}
	// Store this file position, since it should represent the start of the frame data
	trajectoryFirstFrame_ = trajectoryFile_->tellg();
	// Determine frame size and number of frames in file
	msg(Debug::Verbose,"Testing trajectory frame read...\n");
	//printf("Initialised config\n");
	Model *newframe = addFrame();
	setRenderFromFrames();
	if (!trajectoryFilter_->execute("",trajectoryFile_,FALSE))
	{
		msg(Debug::None,"Error testing frame read from trajectory.\n");
		trajectoryFile_->close();
		trajectoryFile_ = NULL;
		trajectoryFilter_ = NULL;
		clearTrajectory();
		setRenderFromSelf();
		dbgEnd(Debug::Calls,"Model::initialiseTrajectory");
		return FALSE;
	}
	streampos endofframe = trajectoryFile_->tellg();
	frameSize_ = endofframe - trajectoryFirstFrame_;
	if ((frameSize_/1024) < 10) msg(Debug::None,"Single frame is %i bytes.\n", frameSize_);
	else msg(Debug::None,"Single frame is %i kb.\n", frameSize_/1024);
	trajectoryFile_->seekg(0,ios::end);
	streampos endoffile = trajectoryFile_->tellg();
	totalFrames_ = (endoffile - trajectoryFirstFrame_) / frameSize_;
	trajectoryLastFrame_ = trajectoryFirstFrame_ + streampos((totalFrames_ - 1) * frameSize_);
	msg(Debug::Verbose,"File position of first = %lu, frameSize_ = %i, nframes =%i\n", int(trajectoryFirstFrame_), frameSize_, totalFrames_);
	// Skip back to end of first frame ready to read in next frame...
	trajectoryFile_->seekg(endofframe);
	// Pre-Cache frame(s)
	msg(Debug::None,"Successfully associated trajectory.\n"); 
	msg(Debug::None,"Number of frames in file : %i\n", totalFrames_);
	// If we are caching the trajectory, read in all remaining frames here. Otherwise, we're happy with just the first
	msg(Debug::None,"Estimated trajectory size is %li kb, cache limit = %i kb\n", totalFrames_ * frameSize_/1024, prefs.cacheLimit());
	if ((totalFrames_ * frameSize_)/1024 < prefs.cacheLimit())
	{
		msg(Debug::None,"Caching all frames from trajectory...\n");
		// Read all frames from trajectory file
		for (int n=1; n<totalFrames_; n++)
		{
			newframe = addFrame();
			success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
			if (success) msg(Debug::None,"Read frame %i from file.\n", n+1);
			else
			{
				frames_.remove(newframe);
				msg(Debug::None,"Cached %i frames from trajectory before fail.\n", n-1);
				break;
			}
		}
		trajectoryCached_ = TRUE;
		trajectoryFile_->close();
	}
	else msg(Debug::None, "Trajectory will not be cached in memory.\n");
	dbgEnd(Debug::Calls,"Model::initialiseTrajectory");
	return TRUE;
}

// Add frame to trajectory
Model *Model::addFrame()
{
	dbgBegin(Debug::Calls,"Model::addFrame");	
	Model *newframe = frames_.add();
	nCachedFrames_ ++;
	// Set currentFrame_ here (always points to the last added frame)
	currentFrame_ = newframe;
	newframe->setTrajectoryParent(this);
	framePosition_ = nCachedFrames_;
	dbgEnd(Debug::Calls,"Model::addFrame");	
	return newframe;
}

// Delete cached frame from trajectory
void Model::removeFrame(Model *xframe)
{
	// Delete the specified frame from the trajectory structure
	dbgBegin(Debug::Calls,"Model::removeFrame");
	if (xframe == currentFrame_) currentFrame_ = (xframe->next == NULL ? xframe->prev : xframe->next);
	frames_.remove(xframe);
	nCachedFrames_ --;
	dbgEnd(Debug::Calls,"trajectory::deleteFrame");
}

// Seek to first frame
void Model::seekFirstFrame()
{
	// Seek to the first frame in the trajectory
	dbgBegin(Debug::Calls,"Model::seekFirstFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg(Debug::None,"No trajectory is available.\n");
		dbgEnd(Debug::Calls,"Model::seekFirstFrame");
		return;
	}
	if (framePosition_ == 1)
	{
		msg(Debug::None,"Already at start of trajectory.\n");
		dbgEnd(Debug::Calls,"Model::seekFirstFrame");
		return;
	}
	currentFrame_ = frames_.first();
	if (!trajectoryCached_)
	{
		// Seek to position of first frame in file
		trajectoryFile_->seekg(trajectoryFirstFrame_);
		bool success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
	}
	framePosition_ = 1;
	logChange(Change::VisualLog);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg(Debug::None,"Seek to frame %i\n", framePosition_);
	dbgEnd(Debug::Calls,"Model::seekFirstFrame");
}

// Seek to next frame
void Model::seekNextFrame()
{
	// Seek to the next frame in the trajectory
	dbgBegin(Debug::Calls,"Model::seekNextFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg(Debug::None,"No trajectory is available.\n");
		dbgEnd(Debug::Calls,"Model::seekNextFrame");
		return;
	}
	bool success;
	if (framePosition_ == totalFrames_)
	{
		msg(Debug::None,"Already at end of trajectory (frame %i).\n",framePosition_);
		dbgEnd(Debug::Calls,"Model::seekNextFrame");
		return;
	}
	if (trajectoryCached_) currentFrame_ = currentFrame_->next;
	else
	{
		removeFrame(currentFrame_);
		master.current.rs = addFrame();
		success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
	}
	framePosition_ ++;
	logChange(Change::VisualLog);
	printf("Frame = %li, parent = %li (model = %li)\n",currentFrame_,currentFrame_->trajectoryParent_,this);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg(Debug::None,"Seek to frame %i\n",framePosition_);
	dbgEnd(Debug::Calls,"Model::seekNextFrame");
}

// Seek to previous frame
void Model::seekPreviousFrame()
{
	// Seek to the previous frame in the trajectory
	dbgBegin(Debug::Calls,"Model::seekPreviousFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg(Debug::None,"No trajectory is available.\n");
		dbgEnd(Debug::Calls,"Model::seekPreviousFrame");
		return;
	}
	if (framePosition_ == 1)
	{
		msg(Debug::None,"Already at start of trajectory.\n");
		dbgEnd(Debug::Calls,"Model::seekPreviousFrame");
		return;
	}
	if (trajectoryCached_) currentFrame_ = currentFrame_->prev;
	else
	{
		// Read in previous frame from file
		streampos newpos = trajectoryFirstFrame_ + streampos((framePosition_-2)*frameSize_);
		trajectoryFile_->seekg(newpos);
		bool success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
	}
	framePosition_ --;
	logChange(Change::VisualLog);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg(Debug::None,"Seek to frame %i\n",framePosition_);
	dbgEnd(Debug::Calls,"Model::seekPreviousFrame");
}

// Seek to last frame
void Model::seekLastFrame()
{
	// Seek to the last frame in the trajectory
	dbgBegin(Debug::Calls,"Model::seekLastFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg(Debug::None,"No trajectory is available.\n");
		dbgEnd(Debug::Calls,"Model::seekLastFrame");
		return;
	}
	if (framePosition_ == totalFrames_)
	{
		msg(Debug::None,"Already at end of trajectory (frame %i).\n", framePosition_);
		dbgEnd(Debug::Calls,"Model::seekNextFrame");
		return;
	}
	if (trajectoryCached_) currentFrame_ = frames_.last();
	else
	{
		// Read in last frame from file
		trajectoryFile_->seekg(trajectoryLastFrame_);
		bool success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
	}
	framePosition_ = totalFrames_;
	logChange(Change::VisualLog);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg(Debug::None,"Seek to frame %i\n",framePosition_);
	dbgEnd(Debug::Calls,"Model::seekLastFrame");
}

// Seek to specified frame
void Model::seekFrame(int frameno)
{
	// Seek to the previous frame in the trajectory
	dbgBegin(Debug::Calls,"Model::seekFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg(Debug::None,"No trajectory is available.\n");
		dbgEnd(Debug::Calls,"Model::seekFrame");
		return;
	}
	if ((frameno < 1) || (frameno > totalFrames_))
	{
		msg(Debug::None,"Frame %i is out of range for current trajectory (which has %i frames).\n", frameno, totalFrames_);
		dbgEnd(Debug::Calls,"Model::seekFrame");
		return;
	}
	if (framePosition_ == frameno)
	{
		msg(Debug::None,"Already at specified frame (%i).\n",frameno);
		dbgEnd(Debug::Calls,"Model::seekFrame");
		return;
	}
	if (trajectoryCached_) currentFrame_ = frames_[frameno - 1];
	else
	{
		// Seek to specified frame in file
		streampos newpos = trajectoryFirstFrame_ + streampos((framePosition_-1)*frameSize_);
		trajectoryFile_->seekg(newpos);
		bool success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
	}
	framePosition_ = frameno;
	logChange(Change::VisualLog);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg(Debug::None,"Seek to frame %i\n",framePosition_);
	dbgEnd(Debug::Calls,"Model::seekFrame");
}
