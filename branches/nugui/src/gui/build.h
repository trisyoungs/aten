/*
	*** Build Dock Widget
	*** src/gui/build.h
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

#ifndef ATEN_BUILDWIDGET_H
#define ATEN_BUILDWIDGET_H

#include "gui/ui_build.h"

// Build (draw) window
class BuildWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWidget();
	private slots:
	void on_AddAtomButton_clicked(bool on);

	/*
	// Local variables
	*/
	private:

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	BuildWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~BuildWidget();
	// Main form declaration
	Ui::BuildWidget ui;
};

#endif
