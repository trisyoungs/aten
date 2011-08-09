/*
	*** Cell Transform Dock Widget
	*** src/gui/celltransform.h
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

#ifndef ATEN_CELLTRANSFORMWIDGET_H
#define ATEN_CELLTRANSFORMWIDGET_H

#include "gui/ui_celltransform.h"
#include "templates/vector3.h"

// Cell transform window
class CellTransformWidget : public QDockWidget
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
	// Replicate
	void on_CellReplicateButton_clicked(bool checked);
	void on_CellReplicateFoldCheck_clicked(bool checked);
	void on_CellReplicateTrimCheck_clicked(bool checked);
	void on_CellReplicateNegXSpin_valueChanged(double d);
	void on_CellReplicateNegYSpin_valueChanged(double d);
	void on_CellReplicateNegZSpin_valueChanged(double d);
	void on_CellReplicatePosXSpin_valueChanged(double d);
	void on_CellReplicatePosYSpin_valueChanged(double d);
	void on_CellReplicatePosZSpin_valueChanged(double d);
	// Scale
	void on_CellScaleButton_clicked(bool checked);
	// Rotate
	void on_CellRotateXClockwise_clicked(bool checked);
	void on_CellRotateXAnticlockwise_clicked(bool checked);
	void on_CellRotateYClockwise_clicked(bool checked);
	void on_CellRotateYAnticlockwise_clicked(bool checked);
	void on_CellRotateZClockwise_clicked(bool checked);
	void on_CellRotateZAnticlockwise_clicked(bool checked);
	// Miller
	void on_MillerCutButton_clicked(bool checked);
	void on_MillerSelectButton_clicked(bool checked);
	void on_MillerHSpin_valueChanged(int value);
	void on_MillerKSpin_valueChanged(int value);
	void on_MillerLSpin_valueChanged(int value);
	void on_MillerInRadio_clicked(bool checked);
	void on_MillerOutRadio_clicked(bool checked);
	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Local variables
	*/
	private:
	// Whether the window is refreshing
	bool refreshing_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	CellTransformWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::CellTransformWidget ui;
};

#endif
