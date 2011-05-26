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
	// Set subtitle (if one exists)
	void setSubTitle();
	
	public:
	// Initialise dialog, setting all widget values and titles (but not making it visible)
	void initialise();
	// Update dialog, setting new secondary job title if necessary
	void updateProgress();
	// Close dialog window
	void terminate();


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
