/*
	*** Qt GUI: Grids Window
	*** src/gui/grids.h
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

#ifndef ATEN_GRIDSWINDOW_H
#define ATEN_GRIDSWINDOW_H

#include "gui/ui_grids.h"

// Forward Declarations
class QMenuBar;

// Grids window
class AtenGrids : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	void refresh();
	void loadGrid();
	private:
	void refreshGridInfo();
	void gridOriginChanged(int component, double value);
	void gridAxisChanged(int row, int component, double value);
	private slots:
	// Menu
	void on_actionGridLoad_triggered(bool checked);
	void on_actionGridCopy_triggered(bool checked);
	void on_actionGridCut_triggered(bool checked);
	void on_actionGridDelete_triggered(bool checked);
	void on_actionGridPaste_triggered(bool checked);
	// List
	void on_GridList_currentRowChanged(int row);
	void on_GridList_itemClicked(QListWidgetItem *item);
	// Cutoff Page
	void on_GridCutoffSpin_valueChanged(double d);
	void on_GridUpperCutoffSpin_valueChanged(double d);
	void on_GridSymmetricCheck_clicked(bool checked);
	// Origin / Axes Page
	void on_GridOriginXSpin_valueChanged(double d);
	void on_GridOriginYSpin_valueChanged(double d);
	void on_GridOriginZSpin_valueChanged(double d);
	void on_GridAxesAXSpin_valueChanged(double d);
	void on_GridAxesAYSpin_valueChanged(double d);
	void on_GridAxesAZSpin_valueChanged(double d);
	void on_GridAxesBXSpin_valueChanged(double d);
	void on_GridAxesBYSpin_valueChanged(double d);
	void on_GridAxesBZSpin_valueChanged(double d);
	void on_GridAxesCXSpin_valueChanged(double d);
	void on_GridAxesCYSpin_valueChanged(double d);
	void on_GridAxesCZSpin_valueChanged(double d);
	// Style Page
	void on_GridStyleCombo_currentIndexChanged(int index);
	void on_GridPositiveColourButton_clicked(bool checked);
	void on_GridNegativeColourButton_clicked(bool checked);
	void on_GridColourscaleSpin_valueChanged(int n);
	void on_GridUseInternalColoursRadio_clicked(bool checked);
	void on_GridUseColourScaleRadio_clicked(bool checked);	
	// Orbital Page
	void on_ViewBasisButton_clicked(bool checked);
	void on_ViewEigenvectorButton_clicked(bool checked);
	void on_OrbitalCalculateButton_clicked(bool checked);
	void on_OrbitalOriginXSpin_valueChanged(double d);
	void on_OrbitalOriginYSpin_valueChanged(double d);
	void on_OrbitalOriginZSpin_valueChanged(double d);
	void on_OrbitalSpacingSpin_valueChanged(double d);
	void on_OrbitalPointsSpin_valueChanged(int i);
	// Dialog
	void dialogFinished(int result);

	/*
	// Local variables
	*/
	private:
	// Whether the window is currently refreshing
	bool refreshing_;
	// Menu bar for window
	QMenuBar *menuBar_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenGrids(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~AtenGrids();
	// Main form declaration
	Ui::GridsDialog ui;
};

#endif
