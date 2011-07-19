/*
	*** About Window
	*** src/gui/about.h
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

#ifndef ATEN_ABOUTWINDOW_H
#define ATEN_ABOUTWINDOW_H

#include "gui/ui_about.h"

// About window
class AtenAbout : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	private slots:
	void on_AboutCloseButton_clicked(bool checked);

	/*
	// Local variables
	*/
	private:

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenAbout(QWidget *parent = 0);
	// Main form declaration
	Ui::AboutDialog ui;
};

#endif
