/*
	*** ZMatrix Window
	*** src/gui/zmatrix.h
	Copyright T. Youngs 2007-2016

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

#ifndef ATEN_ZMATRIXWINDOW_H
#define ATEN_ZMATRIXWINDOW_H

#include "gui/ui_zmatrix.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;
class ZMatrix;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// ZMatrix Window
class AtenZMatrix : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	AtenZMatrix(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::ZMatrixDialog ui;
	// Column columns
	enum TableColumns { SymbolColumn, DistanceAtomColumn, DistanceColumn, AngleAtomColumn, AngleColumn, TorsionAtomColumn, TorsionColumn, nColumns };

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Local variables
	 */
	private:
	// Whether the widget is currently refreshing
	bool refreshing_;
	// ZMatrix target
	ZMatrix* zMatrix_;


	/*
	 * Window Functions
	 */
	public:
	void showWidget();
	void refresh(bool forceupdate = false);
	private slots:
	void dialogFinished(int result);
	void on_ZMatrixTable_cellDoubleClicked(int row, int column);
	void on_VariablesTable_itemChanged(QTableWidgetItem *w);
	void on_ShiftUpButton_clicked(bool checked);
	void on_ShiftDownButton_clicked(bool checked);
	void on_MoveToStartButton_clicked(bool checked);
	void on_MoveToEndButton_clicked(bool checked);
	void on_CloseButton_clicked(bool checked);
};

#endif
