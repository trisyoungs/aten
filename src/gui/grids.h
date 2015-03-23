/*
	*** Grids Dock Widget
	*** src/gui/grids.h
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

#ifndef ATEN_GRIDSWIDGET_H
#define ATEN_GRIDSWIDGET_H

#include "gui/ui_grids.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class QMenuBar;
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Grid;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Grids window
class GridsWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	GridsWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::GridsWidget ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	// Local variables
	*/
	private:
	// Whether the window is currently refreshing
	bool refreshing_;
	// Menu bar for window
	QMenuBar* menuBar_;


	/*
	// Window Functions
	*/
	public:
	void showWidget();
	void refresh();
	void loadGrid();

	private:
	Grid* getCurrentGrid();
	void addGridToList(Grid* g);
	void refreshGridInfo();
	void gridOriginChanged(int component, double value);
	void gridAxisChanged(int axis, int component, double value);
	void gridShiftChanged();

	private slots:
	// Menu
	void on_actionGridLoad_triggered(bool checked);
	void on_actionGridCopy_triggered(bool checked);
	void on_actionGridCut_triggered(bool checked);
	void on_actionGridDelete_triggered(bool checked);
	void on_actionGridPaste_triggered(bool checked);
	// List
	void on_GridList_currentRowChanged(int row);
	void on_GridList_itemSelectionChanged();
	void on_GridList_itemClicked(QListWidgetItem* item);
	void on_ShowAllGridsCheck_clicked(bool checked);
	// Data / Cutoff Page
	void on_GridLowerCutoffSpin_editingFinished();
	void on_GridUpperCutoffSpin_editingFinished();
	void on_GridLowerCutoff2Spin_editingFinished();
	void on_GridUpperCutoff2Spin_editingFinished();
	void on_GridSecondaryCutoffCheck_clicked(bool checked);
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
	void on_GridPeriodicCheck_clicked(bool checked);
	void on_GridOutlineVolumeCheck_clicked(bool checked);
	void on_GridFillEnclosedVolumeCheck_clicked(bool checked);
	void on_GridPrimaryColourButton_clicked(bool checked);
	void on_GridSecondaryColourButton_clicked(bool checked);
	void on_GridColourscaleSpin_valueChanged(int n);
	void on_GridUseInternalColoursRadio_clicked(bool checked);
	void on_GridUseColourScaleRadio_clicked(bool checked);
	// Shift Page
	void on_ShiftGridPosXButton_clicked(bool checked);
	void on_ShiftGridPosYButton_clicked(bool checked);
	void on_ShiftGridPosZButton_clicked(bool checked);
	void on_ShiftGridNegXButton_clicked(bool checked);
	void on_ShiftGridNegYButton_clicked(bool checked);
	void on_ShiftGridNegZButton_clicked(bool checked);
	void on_GridShiftXSpin_valueChanged(int i);
	void on_GridShiftYSpin_valueChanged(int i);
	void on_GridShiftZSpin_valueChanged(int i);
	// Orbital Page
	void on_ViewBasisButton_clicked(bool checked);
	void on_ViewEigenvectorButton_clicked(bool checked);
	void on_OrbitalCalculateButton_clicked(bool checked);
	void on_OrbitalOriginXSpin_valueChanged(double d);
	void on_OrbitalOriginYSpin_valueChanged(double d);
	void on_OrbitalOriginZSpin_valueChanged(double d);
	void on_OrbitalSpacingSpin_valueChanged(double d);
	void on_OrbitalPointsSpin_valueChanged(int i);

	protected:
	void closeEvent(QCloseEvent *event);
};

#endif
