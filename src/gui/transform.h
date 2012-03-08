/*
	*** Transform Dock Widget
	*** src/gui/transform.h
	Copyright T. Youngs 2007-2012

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

#ifndef ATEN_TRANSFORMWIDGET_H
#define ATEN_TRANSFORMWIDGET_H

#include "gui/ui_transform.h"

// Atom transform window
class TransformWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWidget();
	private:
	void rotateSelection(double direction);
	private slots:
	void on_RotateDefineOriginButton_clicked(bool on);
	void on_RotatePickAxisButton_clicked(bool on);
	void on_RotateDefineAxisButton_clicked(bool on);
	void on_RotateClockwiseButton_clicked(bool on);
	void on_RotateAnticlockwiseButton_clicked(bool on);
	// Matrix Transform
	void on_TransformPickAButton_clicked(bool on);
	void on_TransformPickBButton_clicked(bool on);
	void on_TransformPickCButton_clicked(bool on);
	void on_TransformNormaliseAButton_clicked(bool on);
	void on_TransformNormaliseBButton_clicked(bool on);
	void on_TransformNormaliseCButton_clicked(bool on);
	void on_TransformOrthogonaliseAButton_clicked(bool on);
	void on_TransformOrthogonaliseBButton_clicked(bool on);
	void on_TransformOrthogonaliseCButton_clicked(bool on);
	void on_TransformGenerateAButton_clicked(bool on);
	void on_TransformGenerateBButton_clicked(bool on);
	void on_TransformGenerateCButton_clicked(bool on);
	void on_TransformApplyButton_clicked(bool on);
	void on_TransformOriginCellCentreButton_clicked(bool on);
	void on_TransformDefineOriginButton_clicked(bool on);
	// Matrix Convert
	void on_ConvertRotateIntoButton_clicked(bool on);
	void on_ConvertSourcePickAButton_clicked(bool on);
	void on_ConvertSourcePickBButton_clicked(bool on);
	void on_ConvertSourcePickCButton_clicked(bool on);
	void on_ConvertSourceNormaliseAButton_clicked(bool on);
	void on_ConvertSourceNormaliseBButton_clicked(bool on);
	void on_ConvertSourceNormaliseCButton_clicked(bool on);
	void on_ConvertSourceOrthogonaliseAButton_clicked(bool on);
	void on_ConvertSourceOrthogonaliseBButton_clicked(bool on);
	void on_ConvertSourceOrthogonaliseCButton_clicked(bool on);
	void on_ConvertSourceGenerateAButton_clicked(bool on);
	void on_ConvertSourceGenerateBButton_clicked(bool on);
	void on_ConvertSourceGenerateCButton_clicked(bool on);
	void on_ConvertOriginCellCentreButton_clicked(bool on);
	void on_ConvertDefineOriginButton_clicked(bool on);
	void on_ConvertTargetPickAButton_clicked(bool on);
	void on_ConvertTargetPickBButton_clicked(bool on);
	void on_ConvertTargetPickCButton_clicked(bool on);
	void on_ConvertTargetNormaliseAButton_clicked(bool on);
	void on_ConvertTargetNormaliseBButton_clicked(bool on);
	void on_ConvertTargetNormaliseCButton_clicked(bool on);
	void on_ConvertTargetOrthogonaliseAButton_clicked(bool on);
	void on_ConvertTargetOrthogonaliseBButton_clicked(bool on);
	void on_ConvertTargetOrthogonaliseCButton_clicked(bool on);
	void on_ConvertTargetGenerateAButton_clicked(bool on);
	void on_ConvertTargetGenerateBButton_clicked(bool on);
	void on_ConvertTargetGenerateCButton_clicked(bool on);
	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Local variables
	*/
	private:

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	TransformWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::TransformWidget ui;
};

#endif
