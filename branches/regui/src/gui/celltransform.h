/*
	*** Qt GUI: CellTransform Window
	*** src/gui/celltransform.h
	Copyright T. Youngs 2007,2008

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

// Program preferences window
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
	void on_CellReplicateButton_clicked(bool checked);
	void on_CellReplicateFoldCheck_clicked(bool checked);
	void on_CellReplicateTrimCheck_clicked(bool checked);
	void on_CellScaleButton_clicked(bool checked);
	void dialogFinished(int result);

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
	AtenCellTransform(QWidget *parent = 0);
	~AtenCellTransform();
	// Main form declaration
	Ui::CellTransformDialog ui;
};

#endif
