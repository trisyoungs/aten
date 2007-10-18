/*
	*** Model trajectory functions
	*** src/model/trajectory.cpp

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
#include "file/filter.h"
#include "gui/gui.h"

// Clear trajectory
void model::clear_trajectory()
{
	// Clear frames - can simply delete the master config pointed to by 'frames_head'
	dbg_begin(DM_CALLS,"model::clear_trajectory");
	frames.clear();
	if (trajfile != NULL)
	{
		trajfile->close();
		delete trajfile;
	}
	trajfilename = "Unnamed";
	ncachedframes = 0;
	totalframes = 0;
	frameposition = 0;
	trajcached = FALSE;
	dbg_end(DM_CALLS,"model::clear_trajectory");
}

// Initialise trajectory
bool model::initialise_trajectory(const char *fname, filter *f)
{
	// Associate the supplied trajectory file with the model
	dbg_begin(DM_CALLS,"model::initialise_trajectory");
	bool success;
	// Delete old frames and unset old file
	if (trajfile != NULL) trajfile->close();
	clear_trajectory();
	// Check that we can open the specified file
	trajfile = new ifstream(fname,ios::in);
	if (!trajfile->good())
	{
		msg(DM_NONE,"trajectory::initialise - File '%s' couldn't be opened.\n",fname);
		trajfile->close();
		trajfile = NULL;
		trajfilefilter = NULL;
		dbg_end(DM_CALLS,"model::initialise_trajectory");
		return FALSE;
	}
	// Associate the file with the trajectory
	trajfilename = fname;
	trajfilefilter = f;
	// Read header
	if (!trajfilefilter->read_trajectory(this, TRUE))
	{
		msg(DM_NONE,"Error reading header of trajectory file.\n");
		trajfile->close();
		trajfile = NULL;
		trajfilefilter = NULL;
		dbg_end(DM_CALLS,"model::initialise_trajectory");
		return FALSE;
	}
	// Store this file position, since it should represent the start of the frame data
	trajposfirst = trajfile->tellg();
	// Determine frame size and number of frames in file
	//printf("Testing frame read...\n");
	model testframe;
	testframe.set_trajparent(this);
	//printf("Initialised config\n");
	if (!trajfilefilter->read_trajectory(&testframe, FALSE))
	{
		msg(DM_NONE,"Error testing frame read from trajectory.\n");
		trajfile->close();
		trajfile = NULL;
		trajfilefilter = NULL;
		dbg_end(DM_CALLS,"model::initialise_trajectory");
		return FALSE;
	}
	streampos endofframe = trajfile->tellg();
	framesize = endofframe - trajposfirst;
	msg(DM_NONE,"Single frame is %i kb.\n",framesize/1024);
	trajfile->seekg(0,ios::end);
	streampos endoffile = trajfile->tellg();
	totalframes = (endoffile - trajposfirst) / framesize;
	trajposlast = trajposfirst + streampos((totalframes - 1) * framesize);
	msg(DM_VERBOSE,"File position of first = %lu, framesize = %i, nframes =%i\n",int(trajposfirst),framesize,totalframes);
	trajfile->seekg(trajposfirst);
	// Pre-Cache frame(s)
	msg(DM_NONE,"Successfully associated trajectory.\n"); 
	msg(DM_NONE,"Number of frames in file : %i\n",totalframes);
	// If we are caching the trajectory, read in all frames here. Otherwise, just the first
	msg(DM_VERBOSE,"framesize*N = %li, cache limit = %i\n",totalframes * framesize,prefs.get_cache_limit());
	if ((totalframes * framesize) < prefs.get_cache_limit())
	{
		//printf("Reading all frames..\n");
		// Read all frames from trajectory file
		do
		{
			model *newframe = add_frame();
			newframe->set_trajparent(this);
			success = trajfilefilter->read_trajectory(newframe, FALSE);
			if (!success)
			{
				frames.remove(newframe);
				msg(DM_NONE,"Cached %i frames from trajectory.\n", ncachedframes);
			}
		} while (success);
		trajcached = TRUE;	
	}
	else
	{
		//printf("Reading one frame..\n");
		// Read the first frame from the trajectory only
		model *newframe = add_frame();
		newframe->set_trajparent(this);
		if (!trajfilefilter->read_trajectory(newframe, FALSE)) 
		{
			frames.remove(newframe);
			msg(DM_NONE,"Error when reading frame data.\n");
			dbg_end(DM_CALLS,"model::initialise_trajectory");
			return FALSE;
		}
	}
	render_from_frames();
	dbg_end(DM_CALLS,"model::initialise_trajectory");
	return TRUE;
}

// Add frame to trajectory
model *model::add_frame()
{
	dbg_begin(DM_CALLS,"model::add_frame");	
	model *newframe = frames.add();
	ncachedframes ++;
	// Set currentframe here (always points to the last added frame)
	currentframe = newframe;
	frameposition = ncachedframes;
	dbg_end(DM_CALLS,"model::add_frame");	
	return newframe;
}

// Delete cached frame from trajectory
void model::remove_frame(model *xframe)
{
	// Delete the specified frame from the trajectory structure
	dbg_begin(DM_CALLS,"model::remove_frame");
	if (xframe == currentframe) currentframe = (xframe->next == NULL ? xframe->prev : xframe->next);
	frames.remove(xframe);
	ncachedframes --;
	dbg_end(DM_CALLS,"trajectory::delete_frame");
}

// Seek to first frame
void model::seek_first_frame()
{
	// Seek to the first frame in the trajectory
	dbg_begin(DM_CALLS,"model::seek_first_frame");
	if (frameposition == 1)
	{
		msg(DM_NONE,"Already at start of trajectory.\n");
		dbg_end(DM_CALLS,"model::seek_first_frame");
		return;
	}
	currentframe = frames.first();
	if (!trajcached)
	{
		// Seek to position of first frame in file
		trajfile->seekg(trajposfirst);
		bool success = trajfilefilter->read_trajectory(frames.first(), FALSE);
	}
	frameposition = 1;
	log_change(LOG_VISUAL);
	msg(DM_NONE,"Seek to frame %i\n",frameposition);
	dbg_end(DM_CALLS,"model::seek_first_frame");
}

// Seek to next frame
void model::seek_next_frame()
{
	// Seek to the next frame in the trajectory
	dbg_begin(DM_CALLS,"model::seek_next_frame");
	bool success;
	if (frameposition == totalframes)
	{
		msg(DM_NONE,"Already at end of trajectory (frame %i).\n",frameposition);
		dbg_end(DM_CALLS,"model::seek_next_frame");
		return;
	}
	if (trajcached) currentframe = currentframe->next;
	else success = trajfilefilter->read_trajectory(frames.first(), FALSE);
	frameposition ++;
	log_change(LOG_VISUAL);
	msg(DM_NONE,"Seek to frame %i\n",frameposition);
	dbg_end(DM_CALLS,"model::seek_next_frame");
}

// Seek to previous frame
void model::seek_previous_frame()
{
	// Seek to the previous frame in the trajectory
	dbg_begin(DM_CALLS,"model::seek_previous_frame");
	if (frameposition == 1)
	{
		msg(DM_NONE,"Already at start of trajectory.\n");
		dbg_end(DM_CALLS,"model::seek_previous_frame");
		return;
	}
	if (trajcached) currentframe = currentframe->prev;
	else
	{
		// Read in previous frame from file
		streampos newpos = trajposfirst + streampos((frameposition-2)*framesize);
		trajfile->seekg(newpos);
		bool success = trajfilefilter->read_trajectory(frames.first(), FALSE);
	}
	frameposition --;
	log_change(LOG_VISUAL);
	msg(DM_NONE,"Seek to frame %i\n",frameposition);
	dbg_end(DM_CALLS,"model::seek_previous_frame");
}

// Seek to last frame
void model::seek_last_frame()
{
	// Seek to the last frame in the trajectory
	dbg_begin(DM_CALLS,"model::seek_last_frame");
	if (trajcached) currentframe = frames.last();
	else
	{
		// Read in last frame from file
		trajfile->seekg(trajposlast);
		bool success = trajfilefilter->read_trajectory(frames.first(), FALSE);
	}
	frameposition = totalframes;
	log_change(LOG_VISUAL);
	msg(DM_NONE,"Seek to frame %i\n",frameposition);
	dbg_end(DM_CALLS,"model::seek_last_frame");
}
