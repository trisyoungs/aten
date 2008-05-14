/*
	*** Qt GUI: Minimiser Window
	*** src/gui/minimiser.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_MINIMISERWINDOW_H
#define ATEN_MINIMISERWINDOW_H

#include "gui/ui_minimiser.h"

// Program preferences window
class AtenMinimiser : public QWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	private slots:

	/*
	// Local variables
	*/
	private:

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenMinimiser(QWidget *parent = 0);
	// Destructor
	~AtenMinimiser();
	// Main form declaration
	Ui::MinimiserWidget ui;
	// Finalise widgets (things that couldn't be done in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
