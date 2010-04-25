/*
	*** Qt GUI: Grids functions
	*** src/gui/grids_funcs.cpp
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

#include "main/aten.h"
#include "model/model.h"
#include "gui/mainwindow.h"
#include "gui/grids.h"
#include "gui/gui.h"
#include "gui/tlistwidgetitem.h"
#include "classes/grid.h"
#include "base/sysfunc.h"

// Constructor
AtenGrids::AtenGrids(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;

	// Create menubar
	menuBar_ = new QMenuBar(this);
	QMenu *menu = menuBar_->addMenu("File");
	menu->addAction(ui.actionGridLoad);
	menu = menuBar_->addMenu("Edit");
	menu->addAction(ui.actionGridCut);
	menu->addAction(ui.actionGridCopy);
	menu->addAction(ui.actionGridPaste);
	menu->addAction(ui.actionGridDelete);
	QHBoxLayout *layout = new QHBoxLayout();
	layout->setMenuBar(menuBar_);
	layout->setMargin(0);
	ui.menuFrame->setLayout(layout);
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
	msg.enter("AtenGrids::refresh");
	// Clear and refresh the grids list
	refreshing_ = TRUE;
	ui.GridList->clear();
	TListWidgetItem *item;
	Model *m = aten.currentModelOrFrame();
	for (Grid *g = m->grids(); g != NULL; g = g->next)
	{
		item = new TListWidgetItem(ui.GridList);
		item->setText(g->name());
		item->setCheckState(g->isVisible() ? Qt::Checked : Qt::Unchecked);
		item->data.set(VTypes::GridData, g);
	}
	// Select the first item
	if (m->nGrids() != 0) ui.GridList->setCurrentRow(0);
	refreshGridInfo();
	refreshing_ = FALSE;
	msg.exit("AtenGrids::refresh");
}

// Load grid (public function)
void AtenGrids::loadGrid()
{
	msg.enter("AtenGrids::loadGrid");
	Tree *filter;
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Grid", currentDirectory_.path(), gui.mainWindow->loadGridFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Find the filter that was selected
		filter = aten.findFilterByDescription(FilterData::GridImport, qPrintable(selFilter));
		if (filter != NULL) filter->executeRead(qPrintable(filename));
		else
		{
			filter = aten.probeFile(qPrintable(filename), FilterData::GridImport);
			if (filter != NULL) filter->executeRead(qPrintable(filename));
		}
	}
	gui.gridsWindow->refresh();
	gui.mainView.postRedisplay();
	msg.exit("AtenGrids::loadGrid");
}

void AtenGrids::on_actionGridLoad_triggered(bool checked)
{
	loadGrid();
}

void AtenGrids::on_actionGridCopy_triggered(bool checked)
{
	int row = ui.GridList->currentRow();
	if (row == -1)
	{
		msg.print("No grid selected to copy.\n");
		return;
	}
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	aten.copyGrid(g);
}

void AtenGrids::on_actionGridCut_triggered(bool checked)
{
	int row = ui.GridList->currentRow();
	if (row == -1)
	{
		msg.print("No grid selected to cut.\n");
		return;
	}
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	aten.copyGrid(g);
	m->removeGrid(g);
	refresh();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_actionGridDelete_triggered(bool checked)
{
	// Get the current row selected in the grid list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	m->removeGrid(g);
	refresh();
	if (row == m->nGrids()) row --;
	if (m->nGrids() != 0) ui.GridList->setCurrentRow(row);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_actionGridPaste_triggered(bool checked)
{
	Grid *g = aten.gridClipboard();
	if (g == NULL)
	{
		msg.print("No grid data on clipboard.\n");
		return;
	}
	Model *m = aten.currentModelOrFrame();
	Grid *newgrid = m->addGrid();
	*newgrid = *g;
	refresh();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridOriginXSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridOriginChanged(0, d);
}

void AtenGrids::on_GridOriginYSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridOriginChanged(1, d);
}

void AtenGrids::on_GridOriginZSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridOriginChanged(2, d);
}

void AtenGrids::on_GridAxesAXSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(0,0, d);
}

void AtenGrids::on_GridAxesAYSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(0,1, d);
}

void AtenGrids::on_GridAxesAZSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(0,2, d);
}

void AtenGrids::on_GridAxesBXSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(1,0, d);
}

void AtenGrids::on_GridAxesBYSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(1,1, d);
}

void AtenGrids::on_GridAxesBZSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(1,2, d);
}

void AtenGrids::on_GridAxesCXSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(2,0, d);
}

void AtenGrids::on_GridAxesCYSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(2,1, d);
}

void AtenGrids::on_GridAxesCZSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(2,2, d);
}

void AtenGrids::refreshGridInfo()
{
	msg.enter("AtenGrids::refreshGridInfo");
	// Get the current row selected in the grid list
	Grid *g;
	Model *m = aten.currentModelOrFrame();
	int row = ui.GridList->currentRow();
	if (row == -1)
	{
		msg.exit("AtenGrids::refreshGridInfo");
		return;
	}
	else g = m->grid(row);
	refreshing_ = TRUE;
	// Set minimum, maximum, and cutoff
	ui.GridMinimumLabel->setText(ftoa(g->minimum()));
	ui.GridCutoffSpin->setMinimum(g->minimum());
	ui.GridCutoffSpin->setMaximum(g->maximum());
	ui.GridCutoffSpin->setValue(g->cutoff());
	ui.GridUpperCutoffSpin->setMinimum(g->minimum());
	ui.GridUpperCutoffSpin->setMaximum(g->maximum());
	ui.GridUpperCutoffSpin->setValue(g->upperCutoff());
	ui.GridMaximumLabel->setText(ftoa(g->maximum()));
	ui.GridSymmetricCheck->setChecked( g->isSymmetric() );
	// Set origin and axes
	Vec3<double> origin = g->origin();
	ui.GridOriginXSpin->setValue(origin.x);
	ui.GridOriginYSpin->setValue(origin.y);
	ui.GridOriginZSpin->setValue(origin.z);
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
	// Set surface style data
	ui.GridStyleCombo->setCurrentIndex(g->style());
	ui.GridPositiveColourFrame->setColour(g->positiveColour());
	ui.GridPositiveColourFrame->update();
	ui.GridNegativeColourFrame->setColour(g->negativeColour());
	ui.GridNegativeColourFrame->update();
	ui.GridColourscaleSpin->setValue( g->colourScale()+1 );
	QString scalename = "(";
	scalename += prefs.colourScale[g->colourScale()].name();
	scalename += ")";
	ui.GridColourscaleName->setText(scalename);
	g->useColourScale() ? ui.GridUseColourScaleRadio->setChecked(TRUE) : ui.GridUseInternalColoursRadio->setChecked(TRUE);
	refreshing_ = FALSE;
	msg.exit("AtenGrids::refreshGridInfo");
}

void AtenGrids::on_GridUseInternalColoursRadio_clicked(bool checked)
{
	ui.GridNegativeColourButton->setEnabled(TRUE);
	ui.GridPositiveColourButton->setEnabled(TRUE);
	ui.GridColourscaleSpin->setEnabled(FALSE);
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	ui.GridNegativeColourButton->setEnabled(g->isSymmetric());
	g->setUseColourScale(FALSE);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridUseColourScaleRadio_clicked(bool checked)
{
	ui.GridNegativeColourButton->setEnabled(FALSE);
	ui.GridPositiveColourButton->setEnabled(FALSE);
	ui.GridColourscaleSpin->setEnabled(TRUE);
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setUseColourScale(TRUE);
	gui.mainView.postRedisplay();
}

// Item in grid list has changed?
void AtenGrids::on_GridList_itemClicked(QListWidgetItem *item)
{
	// Cast item to our own TListWidgetItem
	TListWidgetItem *titem = (TListWidgetItem*) item;
	// Get grid associated to item
	Grid *g = (Grid*) titem->data.asPointer(VTypes::GridData);
	// Look at checked state
	g->setVisible( (titem->checkState() == Qt::Checked ? TRUE : FALSE) );
	gui.mainView.postRedisplay();
}

void AtenGrids::gridOriginChanged(int component, double value)
{
	// Get the current row selected in the grid list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
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
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	// Get and re-set axes
	static Mat3<double> axes;
	axes = g->axes();
	axes.rows[r].set(component, value);
	g->setAxes(axes);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridList_currentRowChanged(int row)
{
	if (refreshing_) return;
	// New item selected, so update the data shown in the page
	if (row != -1) refreshGridInfo();
}

void AtenGrids::on_GridCutoffSpin_valueChanged(double d)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setCutoff(d);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridUpperCutoffSpin_valueChanged(double d)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setUpperCutoff(d);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridStyleCombo_currentIndexChanged(int index)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setStyle(Grid::SurfaceStyle (index));
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridPositiveColourButton_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	// Get current surface colour and convert into a QColor
	double *col = g->positiveColour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour
	g->setPositiveColour(newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	ui.GridPositiveColourFrame->setColour(newcol);
	ui.GridPositiveColourFrame->update();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridNegativeColourButton_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	// Get current surface colour and convert into a QColor
	double *col = g->negativeColour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour
	g->setNegativeColour(newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	ui.GridNegativeColourFrame->setColour(newcol);
	ui.GridNegativeColourFrame->update();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridColourscaleSpin_valueChanged(int n)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setColourScale(n-1);
	QString scalename = "(";
	scalename += prefs.colourScale[g->colourScale()].name();
	scalename += ")";
	ui.GridColourscaleName->setText(scalename);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridSymmetricCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setSymmetric(checked);
	gui.mainView.postRedisplay();
}

void AtenGrids::dialogFinished(int result)
{
	gui.mainWindow->ui.actionGridsWindow->setChecked(FALSE);
}
