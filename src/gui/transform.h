/*
	*** Transform Dock Widget
	*** src/gui/transform.h
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

#ifndef ATEN_TRANSFORMWIDGET_H
#define ATEN_TRANSFORMWIDGET_H

#include "gui/ui_transform.h"

// Forward Declarations (Qt)
class AtenWindow;

// Atom transform window
class TransformWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	TransformWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::TransformWidget ui;


	/*
	 * Window Functions
	 */
	public:
	void showWidget();
	private:
	void rotateSelection(double direction);
	private slots:
	void on_RotateDefineOriginButton_clicked(bool checked);
	void on_RotatePickAxisButton_clicked(bool checked);
	void on_RotateDefineAxisButton_clicked(bool checked);
	void on_RotateClockwiseButton_clicked(bool checked);
	void on_RotateAnticlockwiseButton_clicked(bool checked);
	// Matrix Transform
	void on_TransformPickAButton_clicked(bool checked);
	void on_TransformPickBButton_clicked(bool checked);
	void on_TransformPickCButton_clicked(bool checked);
	void on_TransformNormaliseAButton_clicked(bool checked);
	void on_TransformNormaliseBButton_clicked(bool checked);
	void on_TransformNormaliseCButton_clicked(bool checked);
	void on_TransformOrthogonaliseAButton_clicked(bool checked);
	void on_TransformOrthogonaliseBButton_clicked(bool checked);
	void on_TransformOrthogonaliseCButton_clicked(bool checked);
	void on_TransformGenerateAButton_clicked(bool checked);
	void on_TransformGenerateBButton_clicked(bool checked);
	void on_TransformGenerateCButton_clicked(bool checked);
	void on_TransformApplyButton_clicked(bool checked);
	void on_TransformOriginCellCentreButton_clicked(bool checked);
	void on_TransformDefineOriginButton_clicked(bool checked);
	// Matrix Convert
	void on_ConvertRotateIntoButton_clicked(bool checked);
	void on_ConvertSourcePickAButton_clicked(bool checked);
	void on_ConvertSourcePickBButton_clicked(bool checked);
	void on_ConvertSourcePickCButton_clicked(bool checked);
	void on_ConvertSourceNormaliseAButton_clicked(bool checked);
	void on_ConvertSourceNormaliseBButton_clicked(bool checked);
	void on_ConvertSourceNormaliseCButton_clicked(bool checked);
	void on_ConvertSourceOrthogonaliseAButton_clicked(bool checked);
	void on_ConvertSourceOrthogonaliseBButton_clicked(bool checked);
	void on_ConvertSourceOrthogonaliseCButton_clicked(bool checked);
	void on_ConvertSourceGenerateAButton_clicked(bool checked);
	void on_ConvertSourceGenerateBButton_clicked(bool checked);
	void on_ConvertSourceGenerateCButton_clicked(bool checked);
	void on_ConvertOriginCellCentreButton_clicked(bool checked);
	void on_ConvertDefineOriginButton_clicked(bool checked);
	void on_ConvertTargetPickAButton_clicked(bool checked);
	void on_ConvertTargetPickBButton_clicked(bool checked);
	void on_ConvertTargetPickCButton_clicked(bool checked);
	void on_ConvertTargetNormaliseAButton_clicked(bool checked);
	void on_ConvertTargetNormaliseBButton_clicked(bool checked);
	void on_ConvertTargetNormaliseCButton_clicked(bool checked);
	void on_ConvertTargetOrthogonaliseAButton_clicked(bool checked);
	void on_ConvertTargetOrthogonaliseBButton_clicked(bool checked);
	void on_ConvertTargetOrthogonaliseCButton_clicked(bool checked);
	void on_ConvertTargetGenerateAButton_clicked(bool checked);
	void on_ConvertTargetGenerateBButton_clicked(bool checked);
	void on_ConvertTargetGenerateCButton_clicked(bool checked);
	protected:
	void closeEvent(QCloseEvent* event);
};

#endif
