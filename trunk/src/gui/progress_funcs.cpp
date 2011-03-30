/*
	*** Progress Indicator Functions
	*** src/gui/progress_funcs.cpp
	Copyright T. Youngs 2007-2011

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

#include "gui/progress.h"
#include "gui/gui.h"

// Constructor
AtenProgress::AtenProgress(QWidget *parent, Qt::WindowFlags flags ) : QDialog(parent, flags)
{
	primaryJob_ = FALSE;
	secondaryJob_ = FALSE;
}

void AtenProgress::on_CancelButton_clicked(bool checked)
{
	// TGAY
}

// Notify that the progress indicator should be canceled
void AtenProgress::notifyProgressCanceled()
{
	progressCanceled_ = TRUE;
}

// Setup or update progress indicator
void AtenProgress::updateWidgets() //(bool visible, int maximum, int value, const char *title, const char *eta)
{
	ui.ProgressBar->setValue(progressCurrentStep_);
	ui.TimeRemainingLabel->setText(etaText_.get());
	if (secondaryJob_)
	{
		ui.SecondaryJobTitleLabel->setText(secondaryTitle_.get());
		ui.SecondaryJobTitleLabel->setVisible(TRUE);
	}
	else ui.SecondaryJobTitleLabel->setVisible(FALSE);
}

// Instantiate a progress dialog
int AtenProgress::create(const char *jobtitle, int stepstodo, bool allowSecondary)
{
	// We will return 1 to indicate that the primary progress indicator was created, and 2 for secondary
	// The secondary indicator is only displayed if specifically allowed by the allowSecondary flag
	// If a secondary job is already running, just return -1
	int returnvalue;
	if (secondaryJob_) return -1;
	else if (!primaryJob_)
	{
		// We are starting a primary progress dialog, so set up from scratch.
		// Reset our QTime object...
		time_.setHMS(0,0,0);
		time_.start();
		progressCurrentStep_ = 0;
		progressStepsToDo_ = stepstodo;
		progressPercent_ = 0;
		progressCanceled_ = FALSE;
		// If the GUI doesn't exist, call the text-based progress indicator
		if (!gui.exists())
		{
			// Don't print anything if we're in quiet mode
			if (!msg.isQuiet() & (!(time_.elapsed() < 2000)))
			{
				// Print out the empty progress indicator
				printf("--- %s\n", jobtitle);
				printf("Progress [-]                              (  0%%)");
				fflush(stdout);
			}
		}
		else
		{
			ui.JobTitleLabel->setText(jobtitle);
			ui.ProgressBar->setMaximum(stepstodo);
			updateWidgets();
		}
		primaryJob_ = TRUE;
		return 1;
	}
	else if (allowSecondary)
	{
		secondaryTitle_ = jobtitle;
		updateWidgets();
		secondaryJob_ = TRUE;
		return 2;
	}
	return -1;
}

// Terminate the progress dialog
void AtenProgress::terminate(int id)
{
	if (id == -1) return;
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!gui.exists())
	{
		if (id == 2) return;
		if (progressPercent_ == -1) return;
		if (time_.elapsed() >= 500) printf("\n");
		primaryJob_ = FALSE;
		secondaryJob_ = FALSE;
	}
	else
	{
		if (id == 2)
		{
			secondaryJob_ = FALSE; 
			updateWidgets();
		}
		else
		{
			// Hide the progress bar and re-enable widgets
			hide();
			gui.setWindowsEnabled(TRUE);
			primaryJob_ = FALSE;
		}
	}
	progressPercent_ = -1;
}

// Update the progress dialog
bool AtenProgress::update(int id, int currentstep, Dnchar *shorttext)
{
	if (id != 1) return TRUE;
	progressCurrentStep_ = (currentstep == -1 ? progressCurrentStep_+1 : currentstep);
	double dpercent = double(progressCurrentStep_) / double(progressStepsToDo_);
	static QTime remtime;
	// Show the progress bar if enough time has elapsed since the start of the operation...
	// If the GUI doesn't exist, call the text-based progress indicator instead
	// Calculate ETA
	remtime.setHMS(0,0,0);
	remtime = remtime.addMSecs( time_.elapsed() * ((1.0 - dpercent) / dpercent) );
	if (!gui.exists())
	{
		static char twister[4] = { '-', '\\', '|', '/' };
		static int n, ndots, c, percent;
		// Don't print anything if we're in quiet mode
		if (msg.isQuiet() || (time_.elapsed() < 500) ) return TRUE;
		// Work out percentage and print dots and spaces
		percent = int(dpercent * 100.0);
		ndots = int(dpercent * 30.0);
		dpercent *= 100.0;
		// Always print the header and twister character
		if (shorttext == NULL) printf("\rProgress [%c]", twister[c]);
		// Increase the twister character
		++c;
		c = c%4;
		// New dots or percentage to output?
		if (percent != progressPercent_)
		{
			if (shorttext == NULL)
			{
				for (n=0; n<ndots; n++) printf(".");
				for (n=ndots; n<30; n++) printf(" ");
			}
			// Lastly, print percentage and ETA
			etaText_.sprintf("(%-3i%%, ETA %02i:%02i:%02i)",percent, remtime.hour(), remtime.minute(), remtime.second());
			if (shorttext == NULL) printf("%s", etaText_.get());
			else shorttext->set(etaText_);
			fflush(stdout);
			progressPercent_ = percent;
		}
	}
	else if (time_.elapsed() >= 4000)
	{
		gui.setWindowsEnabled(FALSE);
		etaText_.sprintf("ETA %02i:%02i:%02i", remtime.hour(), remtime.minute(), remtime.second());
		show();
		updateWidgets();
		gui.app->processEvents();
	}
	gui.app->processEvents();
	// Check to see if the abort button was pressed
	return (!progressCanceled_);
}
