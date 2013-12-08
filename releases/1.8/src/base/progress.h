/*
	*** Progress Indicator
	*** src/base/progress.h
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

#ifndef ATEN_PROGRESSINDICATOR_H
#define ATEN_PROGRESSINDICATOR_H

#include "base/dnchar.h"
#include <QtCore/QTime>

extern Dnchar s;
// Progress Indicator Data
class ProgressIndicator
{
	public:
	// Constructor
	ProgressIndicator();

	private:
	// QTime object (used to prevent showing of GUI progress indicator if the operation will be quick)
	QTime time_;
	// Indicator that the 'Cancel' button was pressed
	bool canceled_;
	// Variables for the position and maximum of the text progress dialog
	int stepsToDo_, percent_, currentStep_;
	// ETA and Jobtitle texts
	Dnchar etaText_, jobTitle_, subTitle_;
	// Flags to indicate whether indicator is active
	bool hasJob_;
	// Whether text indicator should be printed out
	bool hidden_;
	
	public:
	// Notify that the progress indicator should be canceled
	void notifyCanceled();
	// Instantiate a new progress dialog (or a sub-job) of the current one
	int initialise(const char* jobtitle, int stepstodo, bool hidden);
	// Update the progress dialog
	bool update(int id, int currentstep = -1, const char *subtitle = NULL);
	// Terminate the progress dialog
	void terminate(int id);
	// Return whether a major job is in progress
	bool hasJob();
	// Return ETA (as text)
	const char *eta();
	// Return major job title
	const char *jobTitle();
	// Return minor job title
	const char *subTitle();
	// Return number of steps to do
	int stepsToDo();
	// Return current step number
	int currentStep();
};

// External declaration
extern ProgressIndicator progress;

#endif
