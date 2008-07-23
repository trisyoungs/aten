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

#include "base/aten.h"
#include "parse/filter.h"
#include "model/model.h"
#include "gui/gui.h"
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
	msg.enter("Model::clearTrajectory");
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
	msg.exit("Model::clearTrajectory");
}

// Initialise trajectory
bool Model::initialiseTrajectory(const char *fname, Filter *f)
{
	// Associate the supplied trajectory file with the model
	msg.enter("Model::initialiseTrajectory");
	bool success;
	// Delete old frames and unset old file
	if (trajectoryFile_ != NULL) trajectoryFile_->close();
	clearTrajectory();
	// Check that we can open the specified file
	trajectoryFile_ = new ifstream(fname,ios::in);
	if (!trajectoryFile_->good())
	{
		msg.print("Trajectory file '%s' couldn't be opened.\n",fname);
		trajectoryFile_->close();
		trajectoryFile_ = NULL;
		trajectoryFilter_ = NULL;
		msg.exit("Model::initialiseTrajectory");
		return FALSE;
	}
	// Associate the file with the trajectory
	trajectoryFilename_ = fname;
	trajectoryFilter_ = f;
	// Read header
	if (!trajectoryFilter_->execute("",trajectoryFile_,TRUE))
	{
		msg.print("Error reading header of trajectory file.\n");
		trajectoryFile_->close();
		trajectoryFile_ = NULL;
		trajectoryFilter_ = NULL;
		msg.exit("Model::initialiseTrajectory");
		return FALSE;
	}
	// Store this file position, since it should represent the start of the frame data
	trajectoryFirstFrame_ = trajectoryFile_->tellg();
	// Determine frame size and number of frames in file
	msg.print(Messenger::Verbose,"Testing trajectory frame read...\n");
	//printf("Initialised config\n");
	Model *newframe = addFrame();
	setRenderFromFrames();
	if (!trajectoryFilter_->execute("",trajectoryFile_,FALSE))
	{
		msg.print("Error testing frame read from trajectory.\n");
		trajectoryFile_->close();
		trajectoryFile_ = NULL;
		trajectoryFilter_ = NULL;
		clearTrajectory();
		setRenderFromSelf();
		msg.exit("Model::initialiseTrajectory");
		return FALSE;
	}
	streampos endofframe = trajectoryFile_->tellg();
	frameSize_ = endofframe - trajectoryFirstFrame_;
	if ((frameSize_/1024) < 10) msg.print("Single frame is %i bytes.\n", frameSize_);
	else msg.print("Single frame is %i kb.\n", frameSize_/1024);
	trajectoryFile_->seekg(0,ios::end);
	streampos endoffile = trajectoryFile_->tellg();
	totalFrames_ = (endoffile - trajectoryFirstFrame_) / frameSize_;
	trajectoryLastFrame_ = trajectoryFirstFrame_ + streampos((totalFrames_ - 1) * frameSize_);
	msg.print(Messenger::Verbose,"File position of first = %lu, frameSize_ = %i, nframes =%i\n", int(trajectoryFirstFrame_), frameSize_, totalFrames_);
	// Skip back to end of first frame ready to read in next frame...
	trajectoryFile_->seekg(endofframe);
	// Pre-Cache frame(s)
	msg.print("Successfully associated trajectory.\n"); 
	msg.print("Number of frames in file : %i\n", totalFrames_);
	framePosition_ = 1;
	// If we are caching the trajectory, read in all remaining frames here. Otherwise, we're happy with just the first
	msg.print("Estimated trajectory size is %li kb, cache limit = %i kb\n", totalFrames_ * frameSize_/1024, prefs.cacheLimit());
	if ((totalFrames_ * frameSize_)/1024 < prefs.cacheLimit())
	{
		msg.print("Caching all frames from trajectory...\n");
		gui.progressCreate("Caching Frames", totalFrames_);
		// Read all frames from trajectory file
		for (int n=1; n<totalFrames_; n++)
		{
			if (!gui.progressUpdate(n)) break;
			newframe = addFrame();
			success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
			if (success)
			{
				//msg.print("Read frame %i from file.\n", n+1);
				framePosition_ ++;
			}
			else
			{
				frames_.remove(newframe);
				msg.print("Error during read of frame %i.\n", n);
				break;
			}
		}
		gui.progressTerminate();
		msg.print("Cached %i frames from file.\n", framePosition_);
		trajectoryCached_ = TRUE;
		trajectoryFile_->close();
	}
	else msg.print( "Trajectory will not be cached in memory.\n");
	msg.exit("Model::initialiseTrajectory");
	return TRUE;
}

// Add frame to trajectory
Model *Model::addFrame()
{
	msg.enter("Model::addFrame");	
	Model *newframe = frames_.add();
	nCachedFrames_ ++;
	// Set currentFrame_ here (always points to the last added frame)
	currentFrame_ = newframe;
	newframe->setTrajectoryParent(this);
	//framePosition_ = nCachedFrames_;
	msg.exit("Model::addFrame");	
	return newframe;
}

// Delete cached frame from trajectory
void Model::removeFrame(Model *xframe)
{
	// Delete the specified frame from the trajectory structure
	msg.enter("Model::removeFrame");
	if (xframe == currentFrame_) currentFrame_ = (xframe->next == NULL ? xframe->prev : xframe->next);
	frames_.remove(xframe);
	nCachedFrames_ --;
	msg.exit("Model::removeFrame");
}

// Seek to first frame
void Model::seekFirstFrame()
{
	// Seek to the first frame in the trajectory
	msg.enter("Model::seekFirstFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekFirstFrame");
		return;
	}
	if (framePosition_ == 1)
	{
		msg.print("Already at start of trajectory.\n");
		msg.exit("Model::seekFirstFrame");
		return;
	}
	currentFrame_ = frames_.first();
	if (!trajectoryCached_)
	{
		currentFrame_->clear();
		// Seek to position of first frame in file
		trajectoryFile_->seekg(trajectoryFirstFrame_);
		bool success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
		currentFrame_->logChange(Change::VisualLog);
	}
	framePosition_ = 1;
	logChange(Change::VisualLog);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.print("Seek to frame %i\n", framePosition_);
	msg.exit("Model::seekFirstFrame");
}

// Seek to next frame
void Model::seekNextFrame()
{
	// Seek to the next frame in the trajectory
	msg.enter("Model::seekNextFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekNextFrame");
		return;
	}
	bool success;
	if (framePosition_ == totalFrames_)
	{
		msg.print("Already at end of trajectory (frame %i).\n",framePosition_);
		msg.exit("Model::seekNextFrame");
		return;
	}
	if (trajectoryCached_) currentFrame_ = currentFrame_->next;
	else
	{
		currentFrame_->clear();
		success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
		currentFrame_->logChange(Change::VisualLog);
	}
	framePosition_ ++;
	logChange(Change::VisualLog);
	//printf("Frame = %li, parent = %li (model = %li)\n",currentFrame_,currentFrame_->trajectoryParent_,this);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.print("Seek to frame %i\n", framePosition_);
	msg.exit("Model::seekNextFrame");
}

// Seek to previous frame
void Model::seekPreviousFrame()
{
	// Seek to the previous frame in the trajectory
	msg.enter("Model::seekPreviousFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekPreviousFrame");
		return;
	}
	if (framePosition_ == 1)
	{
		msg.print("Already at start of trajectory.\n");
		msg.exit("Model::seekPreviousFrame");
		return;
	}
	if (trajectoryCached_) currentFrame_ = currentFrame_->prev;
	else
	{
		currentFrame_->clear();
		// Read in previous frame from file
		streampos newpos = trajectoryFirstFrame_ + streampos((framePosition_-2)*frameSize_);
		trajectoryFile_->seekg(newpos);
		bool success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
		currentFrame_->logChange(Change::VisualLog);
	}
	framePosition_ --;
	logChange(Change::VisualLog);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.print("Seek to frame %i\n",framePosition_);
	msg.exit("Model::seekPreviousFrame");
}

// Seek to last frame
void Model::seekLastFrame()
{
	// Seek to the last frame in the trajectory
	msg.enter("Model::seekLastFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekLastFrame");
		return;
	}
	if (framePosition_ == totalFrames_)
	{
		msg.print("Already at end of trajectory (frame %i).\n", framePosition_);
		msg.exit("Model::seekNextFrame");
		return;
	}
	if (trajectoryCached_) currentFrame_ = frames_.last();
	else
	{
		currentFrame_->clear();
		// Read in last frame from file
		trajectoryFile_->seekg(trajectoryLastFrame_);
		bool success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
		currentFrame_->logChange(Change::VisualLog);
	}
	framePosition_ = totalFrames_;
	logChange(Change::VisualLog);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.print("Seek to frame %i\n",framePosition_);
	msg.exit("Model::seekLastFrame");
}

// Seek to specified frame
void Model::seekFrame(int frameno)
{
	// Seek to the previous frame in the trajectory
	msg.enter("Model::seekFrame");
	// Check that a trajectory exists!
	if (totalFrames_ == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekFrame");
		return;
	}
	if ((frameno < 1) || (frameno > totalFrames_))
	{
		msg.print("Frame %i is out of range for current trajectory (which has %i frames).\n", frameno, totalFrames_);
		msg.exit("Model::seekFrame");
		return;
	}
	if (framePosition_ == frameno)
	{
		msg.print("Already at specified frame (%i).\n",frameno);
		msg.exit("Model::seekFrame");
		return;
	}
	if (trajectoryCached_) currentFrame_ = frames_[frameno - 1];
	else
	{
		currentFrame_->clear();
		// Seek to specified frame in file
		streampos newpos = trajectoryFirstFrame_ + streampos((framePosition_-1)*frameSize_);
		trajectoryFile_->seekg(newpos);
		bool success = trajectoryFilter_->execute("", trajectoryFile_, FALSE);
		currentFrame_->logChange(Change::VisualLog);
	}
	framePosition_ = frameno;
	logChange(Change::VisualLog);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.print("Seek to frame %i\n",framePosition_);
	msg.exit("Model::seekFrame");
}
