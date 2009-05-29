/*
	*** Qt GUI: CellTransform Window
	*** src/gui/celltransform.h
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

#ifndef ATEN_CELLTRANSFORMWINDOW_H
#define ATEN_CELLTRANSFORMWINDOW_H

#include "gui/ui_celltransform.h"
#include "templates/vector3.h"
#include "templates/matrix3.h"

// Cell transform window
class AtenCellTransform : public QDialog
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
	// Replicate
	void on_CellReplicateButton_clicked(bool checked);
	void on_CellReplicateFoldCheck_clicked(bool checked);
	void on_CellReplicateTrimCheck_clicked(bool checked);
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
	void on_MillerHSpin_valueChanged(int value);
	void on_MillerKSpin_valueChanged(int value);
	void on_MillerLSpin_valueChanged(int value);
	void on_MillerCutDownRadio_clicked(bool checked);
	void on_MillerCutUpRadio_clicked(bool checked);


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
	AtenCellTransform(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~AtenCellTransform();
	// Main form declaration
	Ui::CellTransformDialog ui;
};

#endif
