/*
	*** Script Movie Dock Widget
	*** src/gui/scriptmovie.h
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

#ifndef ATEN_SCRIPTMOVIEWIDGET_H
#define ATEN_SCRIPTMOVIEWIDGET_H

#include "gui/ui_scriptmovie.h"

// Forward Declarations (Qt)
class AtenWindow;

// Script movie window
class ScriptMovieWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	ScriptMovieWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::ScriptMovieWidget ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	// Window Functions
	*/
	public:
	void showWidget();
	private slots:
	void on_LoadScriptButton_clicked(bool on);
	void on_SaveScriptButton_clicked(bool on);
	void on_SaveScriptedMovieButton_clicked(bool on);
	protected:
	void closeEvent(QCloseEvent* event);
};

#endif
