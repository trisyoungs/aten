/*
	*** Progress Indicator
	*** src/base/progress.cpp
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

#include "base/progress.h"
#include "base/messenger.h"
#include <stdio.h>

ATEN_BEGIN_NAMESPACE

// Static singleton
ProgressIndicator progress;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Constructor
ProgressIndicator::ProgressIndicator()
{
	// Private variables
	hasJob_ = false;
	canceled_ = false;
}

// Notify that the progress indicator should be canceled
void ProgressIndicator::notifyCanceled()
{
	canceled_ = true;
}

// Instantiate a new progress dialog (or a sub-job of the current one
int ProgressIndicator::initialise(QString jobtitle, int stepstodo)
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
		canceled_ = false;
		jobTitle_ = jobtitle;

		// If the GUI doesn't exist, call the text-based progress indicator
// 		if (gui.exists()) gui.initialiseProgressDialog();   ATEN2 TODO
		if (!Messenger::isQuiet()) printf("%s", qPrintable(jobtitle));

		hasJob_ = true;
		return 1;
	}
	else
	{
		subTitle_ = jobtitle;
		return 2;
	}
}

// Update the progress dialog
bool ProgressIndicator::update(int id, int currentstep, QString subtitle)
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
		if (true)   //gui.exists())  ATEN2 TODO
		{
			etaText_.sprintf("%02i:%02i:%02i)", remtime.hour(), remtime.minute(), remtime.second());
			// Has enough time elapsed for us to show the progress indicator?
// 			if (time_.elapsed() > 1000) gui.updateProgressDialog();  ATEN2 TODO
		}
		else if (time_.elapsed() > 1000)
		{
			etaText_.sprintf("(%-3i%%, ETA %02i:%02i:%02i)", percent, remtime.hour(), remtime.minute(), remtime.second());
			printf("\rProgress [%c]", twister[c]);
			// Increase the twister character
			++c;
			c = c%4;

			for (n=0; n<ndots; n++) printf(".");
			for (n=ndots; n<30; n++) printf(" ");

			// Lastly, print percentage and ETA
			printf("%s", qPrintable(etaText_));
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
// 	if (gui.exists()) gui.terminateProgressDialog(); ATEN2 TODO
	if (time_.elapsed() >= 250) printf("\n");
	hasJob_ = false;
	jobTitle_ = "";
	percent_ = -1;
}

// Return whether a major job is in progress
bool ProgressIndicator::hasJob()
{
	return hasJob_;
}

// Return ETA (as text)
QString ProgressIndicator::eta()
{
	return etaText_;
}

// Return major job title
QString ProgressIndicator::jobTitle()
{
	return jobTitle_;
}

// Return minor job title
QString ProgressIndicator::subTitle()
{
	return subTitle_;
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
