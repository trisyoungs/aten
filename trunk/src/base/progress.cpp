/*
	*** Progress Indicator
	*** src/base/progress.cpp
	Copyright T. Youngs 2007-2013

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

#include "base/progress.h"
#include "base/messenger.h"
#include "gui/gui.h"
#include <stdio.h>

// Static singleton
ProgressIndicator progress;

// Constructor
ProgressIndicator::ProgressIndicator()
{
	// Private variables
	hasJob_ = FALSE;
	canceled_ = FALSE;
	hidden_ = FALSE;
}

// Notify that the progress indicator should be canceled
void ProgressIndicator::notifyCanceled()
{
	canceled_ = TRUE;
}

// Instantiate a new progress dialog (or a sub-job of the current one
int ProgressIndicator::initialise(const char *jobtitle, int stepstodo, bool hidden)
{
	// Check if a progress indicator job already exists
	if (!hasJob_)
	{
		// We are starting a primary progress dialog, so set up from scratch.
		// Reset our QTime object...
		time_.setHMS(0,0,0);
		time_.start();
		currentStep_ = 0;
		stepsToDo_ = stepstodo;
		percent_ = 0;
		canceled_ = FALSE;
		hidden_ = hidden;
		jobTitle_ = jobtitle;
		// If the GUI doesn't exist, call the text-based progress indicator
		if (gui.exists()) gui.initialiseProgressDialog();
		else if (!hidden_)
		{
			// Don't print anything if we're in quiet mode
			if (!msg.isQuiet())
			{
				printf("%s\n", jobtitle);
				// Print out the empty progress indicator
// 				printf("Progress [-]                              (  0%%)");
// 				fflush(stdout);
			}
		}
		hasJob_ = TRUE;
		return 1;
	}
	else
	{
		subTitle_ = jobtitle;
		return 2;
	}
}

// Update the progress dialog
bool ProgressIndicator::update(int id, int currentstep, const char *subtitle)
{
	if (id != 1) return (!canceled_);
	currentStep_ = (currentstep == -1 ? currentStep_+1 : currentstep);
	double dpercent = double(currentStep_) / double(stepsToDo_);
	static QTime remtime;
	static char twister[4] = { '-', '\\', '|', '/' };
	int n, ndots, percent;
	static int c;
	
	// Show the progress bar if enough time has elapsed since the start of the operation...
	// If the GUI doesn't exist, call the text-based progress indicator instead
	
	// Calculate ETA
	remtime.setHMS(0,0,0);
	remtime = remtime.addMSecs( time_.elapsed() * ((1.0 - dpercent) / dpercent) );

	// Set new subtitle if one was specified
	if (subtitle != NULL) subTitle_ = subtitle;
	
	// Work out new percentage
	percent = int(dpercent * 100.0);
	ndots = int(dpercent * 30.0);
	dpercent *= 100.0;

	// Has job completion percentage changed much? If so, update
	if (percent != percent_)
	{
		if (gui.exists())
		{
			etaText_.sprintf("%02i:%02i:%02i)", remtime.hour(), remtime.minute(), remtime.second());
			// Has enough time elapsed for us to show the progress indicator?
			if (time_.elapsed() > 1000) gui.updateProgressDialog();
		}
		else if ((!hidden_) && (time_.elapsed() > 1000))
		{
			etaText_.sprintf("(%-3i%%, ETA %02i:%02i:%02i)", percent, remtime.hour(), remtime.minute(), remtime.second());
			printf("\rProgress [%c]", twister[c]);
			// Increase the twister character
			++c;
			c = c%4;

			for (n=0; n<ndots; n++) printf(".");
			for (n=ndots; n<30; n++) printf(" ");
			// Lastly, print percentage and ETA
			printf("%s", etaText_.get());
			fflush(stdout);
		}
	}
	
	// Update percentage completion
	percent_ = percent;

	// Return the internal status of the progress indicator
	return (!canceled_);
}

// Terminate the progress dialog
void ProgressIndicator::terminate(int id)
{
	if (id == -1) return;
	if (id == 2)
	{
		subTitle_ = "";
		return;
	}
	if (percent_ == -1) return;
	if (gui.exists()) gui.terminateProgressDialog();
	else if ((time_.elapsed() >= 250) && (!hidden_)) printf("\n");
	hasJob_ = FALSE;
	jobTitle_ = "";
	percent_ = -1;
}

// Return whether a major job is in progress
bool ProgressIndicator::hasJob()
{
	return hasJob_;
}

// Return ETA (as text)
const char *ProgressIndicator::eta()
{
	return etaText_.get();
}

// Return major job title
const char *ProgressIndicator::jobTitle()
{
	return jobTitle_.get();
}

// Return minor job title
const char *ProgressIndicator::subTitle()
{
	return subTitle_.get();
}

// Return number of steps to do
int ProgressIndicator::stepsToDo()
{
	return stepsToDo_;
}

// Return current step number
int ProgressIndicator::currentStep()
{
	return currentStep_;
}
