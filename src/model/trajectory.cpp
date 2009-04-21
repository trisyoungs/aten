/*
	*** Model trajectory functions
	*** src/model/trajectory.cpp
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

#include "parser/tree.h"
#include "model/model.h"
#include "gui/gui.h"
#include "classes/prefs.h"
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
void Model::setTrajectoryFilter(Tree *f)
{
	trajectoryFilter_ = f;
}

// Return whether a trajectory for this model exists
bool Model::hasTrajectory()
{
	if (framesAreCached_) return (frames_.nItems() != 0);
	else return (nFileFrames_ != 0);
}

// Return the current frame pointer
Model *Model::currentFrame()
{
	return currentFrame_;
}

// Return the total number of frames in the trajectory (file or cached)
int Model::nFrames()
{
	return (framesAreCached_ ? frames_.nItems() : nFileFrames_);
}

// Return the current integer frame position
int Model::frameIndex()
{
	return frameIndex_;
}

// Clear trajectory
void Model::clearTrajectory()
{
	msg.enter("Model::clearTrajectory");
	frames_.clear();
	// Close file in parser
	trajectoryParser_.closeFile();
	if (trajectoryOffsets_ != NULL) delete[] trajectoryOffsets_;
	trajectoryOffsets_ = NULL;
	highestFrameOffset_ = -1;
	trajectoryFilename_ = "Unnamed";
	nFileFrames_ = 0;
	frameIndex_ = -1;
	framesAreCached_ = FALSE;
	trajectoryFilter_ = NULL;
	msg.exit("Model::clearTrajectory");
}

// Initialise trajectory
bool Model::initialiseTrajectory(const char *fname, Tree *f)
{
	// Associate the supplied trajectory file with the model
	msg.enter("Model::initialiseTrajectory");
	bool success;
	// Delete old frames and unset old file
	clearTrajectory();
	// Open the specified file
	if (!trajectoryParser_.openFile(fname))
	{
		msg.print("Trajectory file '%s' couldn't be opened.\n",fname);
		clearTrajectory();
		msg.exit("Model::initialiseTrajectory");
		return FALSE;
	}
	// Associate the file with the trajectory
	trajectoryFilename_ = fname;
	trajectoryFilter_ = f;
	ReturnValue rv;
	// Read header      XXX TGAY
	if (!trajectoryFilter_->execute(&trajectoryParser_, rv))
	{
		msg.print("Error reading header of trajectory file.\n");
		clearTrajectory();
		msg.exit("Model::initialiseTrajectory");
		return FALSE;
	}
	// Store this file position, since it should represent the start of the frame data
	streampos firstframe = trajectoryParser_.tellg();
	// Determine frame size and number of frames in file
	msg.print(Messenger::Verbose,"Testing trajectory frame read...\n");
	//printf("Initialised config\n");
	Model *newframe = addFrame();
	setRenderFromFrames();
	// XXXXX TGAY   Read header
	if (!trajectoryFilter_->execute(&trajectoryParser_, rv))
	{
		msg.print("Error testing frame read from trajectory.\n");
		clearTrajectory();
		setRenderFromSelf();
		msg.exit("Model::initialiseTrajectory");
		return FALSE;
	}
	streampos secondframe = trajectoryParser_.tellg();
	frameSize_ = secondframe - firstframe;
	if ((frameSize_/1024) < 10) msg.print("Single frame is %i bytes.\n", frameSize_);
	else msg.print("Single frame is (approximately) %i kb.\n", frameSize_/1024);
	trajectoryParser_.seekg(0,ios::end);
	streampos endoffile = trajectoryParser_.tellg();
	nFileFrames_ = (endoffile - firstframe) / frameSize_;
	// Skip back to end of first frame ready to read in next frame...
	trajectoryParser_.seekg(secondframe);
	// Pre-Cache frame(s)
	msg.print("Successfully associated trajectory.\n"); 
	msg.print("Number of frames in file : %i\n", nFileFrames_);
	frameIndex_ = 0;
	// If we are caching the trajectory, read in all remaining frames here. Otherwise, we're happy with just the first
	msg.print("Estimated trajectory size is %li kb, cache limit = %i kb\n", nFileFrames_ * frameSize_/1024, prefs.cacheLimit());
	if ((nFileFrames_ * frameSize_)/1024 < prefs.cacheLimit())
	{
		msg.print("Caching all frames from trajectory...\n");
		gui.progressCreate("Caching Frames", nFileFrames_);
		// Read all frames from trajectory file
		for (int n=1; n<nFileFrames_; n++)
		{
			if (!gui.progressUpdate(n)) break;
			newframe = addFrame();
			success = trajectoryFilter_->execute(&trajectoryParser_, rv);	// TGAY Read frame
			if (!success)
			{
				frames_.remove(newframe);
				msg.print("Error during read of frame %i.\n", n);
				break;
			}
		}
		gui.progressTerminate();
		nFileFrames_ = 0;
		framesAreCached_ = TRUE;
		trajectoryParser_.closeFile();
		msg.print("Cached %i frames from file.\n", nFrames());
	}
	else
	{
		msg.print( "Trajectory will not be cached in memory.\n");
		trajectoryOffsets_ = new streampos[nFileFrames_+1];
		trajectoryOffsets_[0] = firstframe;
		trajectoryOffsets_[1] = secondframe;
		highestFrameOffset_ = 2;
	}
	msg.exit("Model::initialiseTrajectory");
	return TRUE;
}

// Add frame to trajectory
Model *Model::addFrame()
{
	msg.enter("Model::addFrame");	
	Model *newframe = frames_.add();
	// Set currentFrame_ here (always points to the last added frame)
	currentFrame_ = newframe;
	newframe->setTrajectoryParent(this);
	if (frames_.nItems() > 1) framesAreCached_ = TRUE;
	frameIndex_ = frames_.nItems()-1;
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
	msg.exit("Model::removeFrame");
}

// Seek to first frame
void Model::seekFirstFrame()
{
	// Seek to the first frame in the trajectory
	msg.enter("Model::seekFirstFrame");
	// Check that a trajectory exists!
	if (nFrames() == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekFirstFrame");
		return;
	}
	if (frameIndex_ == 0)
	{
		msg.print("Already at start of trajectory.\n");
		msg.exit("Model::seekFirstFrame");
		return;
	}
	currentFrame_ = frames_.first();
	if (!framesAreCached_) seekFrame(0);
	else frameIndex_ = 0;
	changeLog.add(Log::Visual);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.exit("Model::seekFirstFrame");
}

// Seek to next frame
void Model::seekNextFrame()
{
	// Seek to the next frame in the trajectory
	msg.enter("Model::seekNextFrame");
	// Check that a trajectory exists!
	if (nFrames() == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekNextFrame");
		return;
	}
	bool success;
	if (frameIndex_ == nFrames()-1)
	{
		msg.print("Already at end of trajectory (frame %i).\n", frameIndex_+1);
		msg.exit("Model::seekNextFrame");
		return;
	}
	frameIndex_++;
	if (framesAreCached_) currentFrame_ = currentFrame_->next;
	else seekFrame(frameIndex_);
	changeLog.add(Log::Visual);
	//printf("Frame = %li, parent = %li (model = %li)\n",currentFrame_,currentFrame_->trajectoryParent_,this);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.exit("Model::seekNextFrame");
}

// Seek to previous frame
void Model::seekPreviousFrame()
{
	// Seek to the previous frame in the trajectory
	msg.enter("Model::seekPreviousFrame");
	// Check that a trajectory exists!
	if (nFrames() == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekPreviousFrame");
		return;
	}
	if (frameIndex_ == 0)
	{
		msg.print("Already at start of trajectory.\n");
		msg.exit("Model::seekPreviousFrame");
		return;
	}
	frameIndex_--;
	if (framesAreCached_) currentFrame_ = currentFrame_->prev;
	else seekFrame(frameIndex_);
	changeLog.add(Log::Visual);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.exit("Model::seekPreviousFrame");
}

// Seek to last frame
void Model::seekLastFrame()
{
	// Seek to the last frame in the trajectory
	msg.enter("Model::seekLastFrame");
	// Check that a trajectory exists!
	if (nFrames() == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekLastFrame");
		return;
	}
	if (frameIndex_ == nFrames()-1)
	{
		msg.print("Already at end of trajectory (frame %i).\n", frameIndex_+1);
		msg.exit("Model::seekNextFrame");
		return;
	}
	frameIndex_ = nFrames()-1;
	if (framesAreCached_) currentFrame_ = frames_.last();
	else seekFrame(frameIndex_);
	changeLog.add(Log::Visual);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.exit("Model::seekLastFrame");
}

// Seek to specified frame
void Model::seekFrame(int frameno)
{
	// Seek to the previous frame in the trajectory
	msg.enter("Model::seekFrame");
	// Check that a trajectory exists!
	if (nFrames() == 0)
	{
		msg.print("No trajectory is available.\n");
		msg.exit("Model::seekFrame");
		return;
	}
	if ((frameno < 0) || (frameno > nFrames()-1))
	{
		msg.print("Frame %i is out of range for current trajectory (which has %i frames).\n", frameno+1, nFrames());
		msg.exit("Model::seekFrame");
		return;
	}
	if (frameIndex_ == frameno)
	{
		msg.print("Already at specified frame (%i).\n", frameno+1);
		msg.exit("Model::seekFrame");
		return;
	}
	if (framesAreCached_) currentFrame_ = frames_[frameno];
	else
	{
		// Seek to specified frame in file
		// If the desired frame is less than (or equal to) the highest stored offset, just seek to and read it.
		// Otherwise we need to read up to the specified frame number, storing offsets as we go.
		if (frameno <= highestFrameOffset_)
		{
			currentFrame_->clear();
			trajectoryParser_.seekg(trajectoryOffsets_[frameno]);
// 			bool success = trajectoryFilter_->executeRead(trajectoryFile_);	// TGAY read frame
			// If this was the highest offset stored, the file position now corresponds to the next frame
			if ((frameno == highestFrameOffset_) && (highestFrameOffset_ < nFileFrames_))
			{
				trajectoryOffsets_[frameno] = trajectoryParser_.tellg();
				highestFrameOffset_ ++;
			}
		}
		else
		{
			// Seek to last frame position stored
			trajectoryParser_.seekg(trajectoryOffsets_[highestFrameOffset_]);
			// Read in consecutive frames until we get to the desired point, storing pointers as we go.
			for (int i = highestFrameOffset_; i < frameno; i++)
			{
				currentFrame_->clear();
				// Read a frame, and store its stream position
				ReturnValue rv;
				bool success = trajectoryFilter_->execute(&trajectoryParser_, rv);	// TGAY Read header
				if (!success)
				{
					msg.print("Failed to read frame %i in trajectory.\n",i+1);
					msg.exit("Model::seekFrame");
					return;
				}
				// Store the next file offset (remember, array is 0 - N)
				trajectoryOffsets_[i] = trajectoryParser_.tellg();
			}
			highestFrameOffset_ = frameno;
		}
		currentFrame_->changeLog.add(Log::Visual);
	}
	frameIndex_ = frameno;
	changeLog.add(Log::Visual);
	// Recalculate the view matrix for the trajectory frame, since it may have been changed by another frame model
	currentFrame_->calculateViewMatrix();
	msg.print("Seek to frame %i\n", frameIndex_+1);
	msg.exit("Model::seekFrame");
}
