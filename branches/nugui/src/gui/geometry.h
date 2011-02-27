/*
	*** Geometry Dock Widget
	*** src/gui/geometry.h
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

#ifndef ATEN_GEOMETRYWIDGET_H
#define ATEN_GEOMETRYWIDGET_H

#include "gui/ui_geometry.h"

// Geometry window
class GeometryWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWidget();
	void refresh();
	private slots:
	// Measure Tab
	void on_MeasureDistanceSelectionButton_clicked(bool checked);
	void on_MeasureAngleSelectionButton_clicked(bool checked);
	void on_MeasureTorsionSelectionButton_clicked(bool checked);
	void on_MeasureClearAllButton_clicked(bool checked);
	// Distance Tab
	void on_SetNewDistanceButton_clicked(bool checked);
	void on_NudgeDistancePlusButton_clicked(bool checked);
	void on_NudgeDistanceMinusButton_clicked(bool checked);
	// Angle Tab
	void on_SetNewAngleButton_clicked(bool checked);
	void on_NudgeAnglePlusButton_clicked(bool checked);
	void on_NudgeAngleMinusButton_clicked(bool checked);
	// Torsion Tab
	void on_SetNewTorsionButton_clicked(bool checked);
	void on_NudgeTorsionPlusButton_clicked(bool checked);
	void on_NudgeTorsionMinusButton_clicked(bool checked);
	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Public Functions
	*/
	public:

	/*
	// Widgets
	*/
	public:
	// Constructor
	GeometryWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Destructor
	~GeometryWidget();
	// Main form declaration
	Ui::GeometryWidget ui;
	// Finalise widgets (things that couldn't be done in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
