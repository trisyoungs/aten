/*
	*** Qt GUI: Geometry Window
	*** src/gui/geometry.h
	Copyright T. Youngs 2007-2010

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

#ifndef ATEN_GEOMETRYWINDOW_H
#define ATEN_GEOMETRYWINDOW_H

#include "gui/ui_geometry.h"

// Geometry window
class AtenGeometry : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	void refresh();
	private slots:
	void dialogFinished(int result);
	void on_SetNewDistanceButton_clicked(bool checked);
	void on_NudgeDistancePlusButton_clicked(bool checked);
	void on_NudgeDistanceMinusButton_clicked(bool checked);
	void on_SetNewAngleButton_clicked(bool checked);
	void on_NudgeAnglePlusButton_clicked(bool checked);
	void on_NudgeAngleMinusButton_clicked(bool checked);
	void on_SetNewTorsionButton_clicked(bool checked);
	void on_NudgeTorsionPlusButton_clicked(bool checked);
	void on_NudgeTorsionMinusButton_clicked(bool checked);

	/*
	// Public Functions
	*/
	public:

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenGeometry(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Destructor
	~AtenGeometry();
	// Main form declaration
	Ui::GeometryDialog ui;
	// Finalise widgets (things that couldn't be done in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
