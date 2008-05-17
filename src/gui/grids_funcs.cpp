/*
	*** Qt GUI: Grids functions
	*** src/gui/grids_funcs.cpp
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

#include "base/master.h"
#include "classes/grid.h"
#include "gui/grids.h"
#include "gui/gui.h"
#include "gui/tlistwidgetitem.h"
#include <QtGui/QFileDialog>
#include <QtGui/QColorDialog>

// Constructor
AtenGrids::AtenGrids(QWidget *parent)
{
	ui.setupUi(this);

	// Create open grid dialog
	QStringList filters;
	openGridDialog = new QFileDialog(this);
	openGridDialog->setWindowTitle("Open Grid");
	openGridDialog->setDirectory(master.workDir.get());
	openGridDialog->setFileMode(QFileDialog::ExistingFile);
	filters.clear();
	filters << "All files (*)";
	for (Filter *f = master.filters(Filter::GridImport); f != NULL; f = f->next) filters << f->description();
	if (filters.empty()) ui.LoadGridButton->setEnabled(FALSE);
	else openGridDialog->setFilters(filters);
}

// Destructor
AtenGrids::~AtenGrids()
{
}

void AtenGrids::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

// Refresh widget
void AtenGrids::refresh()
{
	dbgBegin(Debug::Calls,"AtenGrids::refresh");
	// Clear and refresh the grids list
	ui.GridList->clear();
	TListWidgetItem *item;
	for (Grid *g = master.grids(); g != NULL; g = g->next)
	{
		item = new TListWidgetItem(ui.GridList);
		item->setText(g->name());
		item->setCheckState(g->isVisible() ? Qt::Checked : Qt::Unchecked);
		item->setGrid(g);
	}
	// Select the first item
	if (master.nGrids() != 0) ui.GridList->setCurrentRow(0);
	refreshGridInfo();
	dbgEnd(Debug::Calls,"AtenGrids::refresh");
}

// Load grid (public function)
void AtenGrids::loadGrid()
{
	dbgBegin(Debug::Calls,"AtenGrids::loadgrid");
	Filter *f;
	Grid *g;
	QString filename;
	QStringList filenames;
	if (openGridDialog->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = openGridDialog->selectedFilter();
		// Find the corresponding Aten filter that was selected
		for (f = master.filters(Filter::GridImport); f != NULL; f = f->next)
			if (strcmp(f->description(),qPrintable(filter)) == 0) break;
		// Get selected filename list
		filenames = openGridDialog->selectedFiles();
		// Loop over selected files
		for (int i = 0; i < filenames.count(); ++i)
		{
			filename = filenames.at(i);
			// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
			if (f != NULL) f->execute(qPrintable(filename));
			else
			{
				f = master.probeFile(qPrintable(filename), Filter::GridImport);
				if (f != NULL) f->execute(qPrintable(filename));
			}
		}
		gui.gridsWindow->refresh();
		gui.mainView.postRedisplay();
	}
	dbgEnd(Debug::Calls,"AtenGrids::loadgrid");
}

void AtenGrids::on_LoadGridButton_clicked(bool checked)
{
	loadGrid();
}

void AtenGrids::on_GridOriginXSpin_valueChanged(double d)
{
	gridOriginChanged(0, d);
}

void AtenGrids::on_GridOriginYSpin_valueChanged(double d)
{
	gridOriginChanged(1, d);
}

void AtenGrids::on_GridOriginZSpin_valueChanged(double d)
{
	gridOriginChanged(2, d);
}

void AtenGrids::on_GridAxesAXSpin_valueChanged(double d)
{
	gridAxisChanged(0,0, d);
}

void AtenGrids::on_GridAxesAYSpin_valueChanged(double d)
{
	gridAxisChanged(0,1, d);
}

void AtenGrids::on_GridAxesAZSpin_valueChanged(double d)
{
	gridAxisChanged(0,2, d);
}

void AtenGrids::on_GridAxesBXSpin_valueChanged(double d)
{
	gridAxisChanged(1,0, d);
}

void AtenGrids::on_GridAxesBYSpin_valueChanged(double d)
{
	gridAxisChanged(1,1, d);
}

void AtenGrids::on_GridAxesBZSpin_valueChanged(double d)
{
	gridAxisChanged(1,2, d);
}

void AtenGrids::on_GridAxesCXSpin_valueChanged(double d)
{
	gridAxisChanged(2,0, d);
}

void AtenGrids::on_GridAxesCYSpin_valueChanged(double d)
{
	gridAxisChanged(2,1, d);
}

void AtenGrids::on_GridAxesCZSpin_valueChanged(double d)
{
	gridAxisChanged(2,2, d);
}

void AtenGrids::refreshGridInfo()
{
	dbgBegin(Debug::Calls,"AtenGrids::refreshGridInfo");
	// Get the current row selected in the grid list
	Grid *g;
	int row = ui.GridList->currentRow();
	if (row == -1)
	{
		dbgEnd(Debug::Calls,"AtenGrids::refreshGridInfo");
		return;
	}
	else g = master.grid(row);
	// Set minimum, maximum, and cutoff
	ui.GridMinimumLabel->setText(ftoa(g->minimum()));
	ui.GridCutoffSpin->setMinimum(g->minimum());
	ui.GridCutoffSpin->setValue(g->cutoff());
	ui.GridCutoffSpin->setMaximum(g->maximum());
	ui.GridMaximumLabel->setText(ftoa(g->maximum()));
	// Set origin
	Vec3<double> origin = g->origin();
	ui.GridOriginXSpin->setValue(origin.x);
	ui.GridOriginYSpin->setValue(origin.y);
	ui.GridOriginZSpin->setValue(origin.z);
	// Set axes
	Mat3<double> axes = g->axes();
	ui.GridAxesAXSpin->setValue(axes.rows[0].x);
	ui.GridAxesAYSpin->setValue(axes.rows[0].y);
	ui.GridAxesAZSpin->setValue(axes.rows[0].z);
	ui.GridAxesBXSpin->setValue(axes.rows[1].x);
	ui.GridAxesBYSpin->setValue(axes.rows[1].y);
	ui.GridAxesBZSpin->setValue(axes.rows[1].z);
	ui.GridAxesCXSpin->setValue(axes.rows[2].x);
	ui.GridAxesCYSpin->setValue(axes.rows[2].y);
	ui.GridAxesCZSpin->setValue(axes.rows[2].z);
	// Set surface style, colour and transparency
	ui.GridStyleCombo->setCurrentIndex(g->style());
	ui.GridPositiveColourFrame->setColour(g->positiveColour());
	ui.GridPositiveColourFrame->update();
	ui.GridNegativeColourFrame->setColour(g->negativeColour());
	ui.GridNegativeColourFrame->update();
	ui.GridSymmetricCheck->setChecked( g->symmetric() );
	ui.GridTransparencySpin->setValue( g->transparency() );
	dbgEnd(Debug::Calls,"AtenGrids::refreshGridInfo");
}

// Item in forcefield list has changed?
void AtenGrids::on_GridList_itemClicked(QListWidgetItem *item)
{
	// Cast item to our own TListWidgetItem
	TListWidgetItem *titem = (TListWidgetItem*) item;
	// Get forcefield associated to item
	Grid *g = titem->grid();
	// Look at checked state
	g->setVisible( (titem->checkState() == Qt::Checked ? TRUE : FALSE) );
	gui.mainView.postRedisplay();
}

void AtenGrids::gridOriginChanged(int component, double value)
{
	// Get the current row selected in the grid list
	Grid *g;
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	else g = master.grid(row);
	// Get and re-set origin
	static Vec3<double> o;
	o = g->origin();
	o.set(component, value);
	g->setOrigin(o);
	gui.mainView.postRedisplay();
}

void AtenGrids::gridAxisChanged(int r, int component, double value)
{
	// Get the current row selected in the grid list
	Grid *g;
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	else g = master.grid(row);
	// Get and re-set axes
	static Mat3<double> axes;
	axes = g->axes();
	axes.rows[r].set(component, value);
	g->setAxes(axes);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_RemoveGridButton_clicked(bool checked)
{
	// Get the current row selected in the grid list
	Grid *g;
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	else g = master.grid(row);
	master.removeGrid(g);
	refresh();
}

void AtenGrids::on_SaveGridButton_clicked(bool checked)
{
}

void AtenGrids::on_GridList_currentRowChanged(int row)
{
	// New item selected, so update the data shown in the page
	if (row != -1) refreshGridInfo();
}

void AtenGrids::on_GridCutoffSpin_valueChanged(double d)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Grid *g = master.grid(row);
	g->setCutoff(d);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridStyleCombo_currentIndexChanged(int index)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Grid *g = master.grid(row);
	g->setStyle(Grid::SurfaceStyle (index));
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridPositiveColourButton_clicked(bool checked)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Grid *g = master.grid(row);
	// Get current surface colour and convert into a QColor
	GLfloat *col = g->positiveColour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	g->setPositiveColour(newcol.redF(), newcol.greenF(), newcol.blueF());
	ui.GridPositiveColourFrame->setColour(newcol);
	ui.GridPositiveColourFrame->update();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridNegativeColourButton_clicked(bool checked)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Grid *g = master.grid(row);
	// Get current surface colour and convert into a QColor
	GLfloat *col = g->positiveColour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	g->setNegativeColour(newcol.redF(), newcol.greenF(), newcol.blueF());
	ui.GridNegativeColourFrame->setColour(newcol);
	ui.GridNegativeColourFrame->update();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridTransparencySpin_valueChanged(double value)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Grid *g = master.grid(row);
	g->setTransparency( (GLfloat) value );
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridColourscaleSpin_valueChanged(int n)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Grid *g = master.grid(row);
	g->setColourScale(n-1);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridSymmetricCheck_clicked(bool checked)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Grid *g = master.grid(row);
	g->setSymmetric(checked);
	gui.mainView.postRedisplay();
}
