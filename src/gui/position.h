/*
	*** Qt GUI: Position Window
	*** src/gui/position.h
	Copyright T. Youngs 2007-2009

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

#ifndef ATEN_POSITIONWINDOW_H
#define ATEN_POSITIONWINDOW_H

#include "gui/ui_position.h"

// Atom position window
class AtenPosition : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	private:
	void flipSelection(int axis);
	void translateSelection(int axis, int dir);
	private slots:
	void dialogFinished(int result);
	// Centre
	void on_DefineCentreButton_clicked(bool checked);
	void on_CentreSelectionButton_clicked(bool checked);
	// Flip
	void on_FlipXButton_clicked(bool checked);
	void on_FlipYButton_clicked(bool checked);
	void on_FlipZButton_clicked(bool checked);
	// Shift
	void on_DefineVectorButton_clicked(bool checked);
	void on_NormaliseVectorButton_clicked(bool checked);
	void on_VectorShiftXSpin_valueChanged(double value);
	void on_VectorShiftYSpin_valueChanged(double value);
	void on_VectorShiftZSpin_valueChanged(double value);
	void on_VectorShiftPositiveButton_clicked(bool checked);
	void on_VectorShiftNegativeButton_clicked(bool checked);
	// Translate
	void on_TranslatePosXButton_clicked(bool on);
	void on_TranslatePosYButton_clicked(bool on);
	void on_TranslatePosZButton_clicked(bool on);
	void on_TranslateNegXButton_clicked(bool on);
	void on_TranslateNegYButton_clicked(bool on);
	void on_TranslateNegZButton_clicked(bool on);
	// Reposition
	void on_RepositionSelectionButton_clicked(bool on);
	void on_DefineRepositionReferenceButton_clicked(bool on);
	void on_DefineRepositionTargetButton_clicked(bool on);

	/*
	// Local variables
	*/
	private:

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenPosition(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~AtenPosition();
	// Main form declaration
	Ui::PositionDialog ui;
};

#endif
