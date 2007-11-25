/*
	*** GTK thread routines
	*** src/gui-gtk/threads.cpp
	Copyright T. Youngs 2007

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

#include <gtk/gtk.h>
#include "gui-gtk/gui-gtk.h"
#include "base/master.h"
#include "gui-gtk/canvas-gtk.h"

// Playback Thread
static GStaticMutex trajectorymutex = G_STATIC_MUTEX_INIT;
GThread *trajectorythread;
GError *trajectorythreaderror = NULL;



// Trajectory playback thread
/* TODO TGAY - General threads method (to encapsulate any function)
void *thread_trajectory_playback(void *arg)
{
	// Cast the argument into the trajectory pointer
	trajectory *traj = (trajectory*) arg;
	bool exitthread = FALSE;
	bool ready;
	int msdelay = 25;
	clock_t time_last = clock(), time_current;
	while (!exitthread)
	{
		ready = FALSE;
		// Wait until the canvas drawing the model is not drawing, and the delay time has been satisfied.
		while (!ready)
		{
			g_static_mutex_lock(&trajectorymutex);
			time_current = clock();
			if ( (time_current > (time_last + master.clocks_per_ms * msdelay)) && !parent->get_rendertarget()->is_drawing()) ready = TRUE;
			g_static_mutex_unlock(&trajectorymutex);
			// Yield if we need to wait
			if (!ready)
			{
				gdk_threads_enter();
				if (gtk_events_pending()) gtk_main_iteration_do(FALSE);
				else g_thread_yield();
				gdk_flush();
				gdk_threads_leave();
			}
			if (ready) time_last = time_current;
		}
		// Check if we should end the thread
		g_static_mutex_lock(&trajectorymutex);
		if (!traj->get_playing())
		{
			g_static_mutex_unlock(&trajectorymutex);
			return NULL;
		}
		// Increase frame pointer and frame counter, unless we're already at the end of the trajectory
		if (traj->at_end())
		{
			traj->set_playing(FALSE);
			g_static_mutex_unlock(&trajectorymutex);
			gdk_threads_enter();
			gui.update_trajcontrols();
			gdk_threads_leave();
			return NULL;
		}
		else traj->seek_next();
		// Post redisplay request and update labels
		gdk_threads_enter();
		gui.mainview.postredisplay();
		gui.update_labels();
		gdk_threads_leave();
		g_static_mutex_unlock(&trajectorymutex);
	}
}

void gui_gtk::create_trajectory_thread(trajectory *traj)
{
	trajectorythread = g_thread_create((GThreadFunc)thread_trajectory_playback, (void*)this, FALSE, &trajectorythreaderror);
	if (trajectorythread == NULL)
	{
		printf("trajectory::set_playing <<<< Failed to create playback thread. Error = [%s] >>>>\n",trajectorythreaderror->message);
		g_error_free(trajectorythreaderror);
	}
}
*/
