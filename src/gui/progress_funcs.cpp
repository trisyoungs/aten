/*
	*** Progress Indicator Functions
	*** src/gui/progress_funcs.cpp
	Copyright T. Youngs 2007-2012

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
#include "base/progress.h"

// Constructor
AtenProgress::AtenProgress(QWidget *parent, Qt::WindowFlags flags ) : QDialog(parent, flags)
{
	ui.setupUi(this);
}

void AtenProgress::on_CancelButton_clicked(bool checked)
{
	progress.notifyCanceled();
}

// Set minor job title (if one exists)
void AtenProgress::setSubTitle()
{
	if (strcmp(progress.subTitle(),"") != 0)
	{
		ui.SubTitleLabel->setText(progress.subTitle());
		ui.SubTitleLabel->setEnabled(TRUE);
	}
	else
	{
		ui.SubTitleLabel->setEnabled(FALSE);
		ui.SubTitleLabel->setText("---");
	}
}

// Show dialog, setting all widget values and titles
void AtenProgress::initialise()
{
	ui.ProgressBar->setMaximum(progress.stepsToDo());
	ui.ProgressBar->setValue(progress.currentStep());
	ui.JobTitleLabel->setText(progress.jobTitle());
	ui.TimeRemainingLabel->setText(progress.eta());
	setSubTitle();
	gui.processMessages();
}

// Update dialog, setting new secondary job title if necessary
void AtenProgress::updateProgress()
{
	ui.ProgressBar->setValue(progress.currentStep());
	ui.TimeRemainingLabel->setText(progress.eta());
	setSubTitle();
	gui.processMessages();
}

// Close dialog window
void AtenProgress::terminate()
{
	close();
}
