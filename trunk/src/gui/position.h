/*
	*** Position Dock Widget
	*** src/gui/position.h
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

#ifndef ATEN_POSITIONWIDGET_H
#define ATEN_POSITIONWIDGET_H

#include "gui/ui_position.h"

// Forward Declarations (Qt)
class AtenWindow;

// Atom position window
class PositionWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	PositionWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::PositionWidget ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	// Window Functions
	*/
	public:
	void showWidget();
	private:
	void flipSelection(int axis);
	void translateSelection(int axis, int dir);
	private slots:
	// Centre
	void on_DefineCentreButton_clicked(bool checked);
	void on_CentreSelectionButton_clicked(bool checked);
	// Flip
	void on_FlipXButton_clicked(bool checked);
	void on_FlipYButton_clicked(bool checked);
	void on_FlipZButton_clicked(bool checked);
	// Shift
	void on_ShiftPickVectorButton_clicked(bool checked);
	void on_ShiftNormaliseVectorButton_clicked(bool checked);
	void on_ShiftVectorXSpin_valueChanged(double value);
	void on_ShiftVectorYSpin_valueChanged(double value);
	void on_ShiftVectorZSpin_valueChanged(double value);
	void on_ShiftVectorPositiveButton_clicked(bool checked);
	void on_ShiftVectorNegativeButton_clicked(bool checked);
	// Translate
	void on_TranslatePosXButton_clicked(bool on);
	void on_TranslatePosYButton_clicked(bool on);
	void on_TranslatePosZButton_clicked(bool on);
	void on_TranslateNegXButton_clicked(bool on);
	void on_TranslateNegYButton_clicked(bool on);
	void on_TranslateNegZButton_clicked(bool on);
	// Reposition
	void on_RepositionSelectionButton_clicked(bool on);
	void on_RepositionDefineReferenceButton_clicked(bool on);
	void on_RepositionDefineTargetButton_clicked(bool on);

	protected:
	void closeEvent(QCloseEvent *event);
};

#endif
