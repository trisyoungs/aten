/*
	*** Progress Dialog
	*** src/gui/progress.h
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

#ifndef ATEN_PROGRESSDIALOG_H
#define ATEN_PROGRESSDIALOG_H

#include "gui/ui_progress.h"
#include "base/dnchar.h"
#include <QtCore/QTime>

// Progress Dialog
class AtenProgress : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	private slots:
	void on_CancelButton_clicked(bool checked);


	/*
	// Local Objects / Variables
	*/
	private:
	// QTime object (used to prevent showing of progress indicator if the operation will be quick)
	QTime time_;
	// Indicator that the 'Cancel' button was pressed
	bool progressCanceled_;
	// Variables for the position and maximum of the text progress dialog
	int progressStepsToDo_, progressPercent_, progressCurrentStep_;
	// ETA and Jobtitle texts
	Dnchar etaText_, jobTitle_, secondaryTitle_;
	// Flags to indicate whether indicator has primary (and secondary) titles
	bool primaryJob_, secondaryJob_;
	
	private:
	// Update dialog
	void updateWidgets();

	public:
	// Notify that the progress indicator should be canceled
	void notifyProgressCanceled();
	// Instantiate a progress dialog
	int create(const char* jobtitle, int stepstodo, bool allowSecondary = FALSE);
	// Update the progress dialog
	bool update(int id, int currentstep = -1, Dnchar *shorttext = NULL);
	// Terminate the progress dialog
	void terminate(int id);

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenProgress(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::AtenProgress ui;
};

#endif
