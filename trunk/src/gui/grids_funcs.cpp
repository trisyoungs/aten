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
#include "gui/viewbasis.h"
#include "gui/vieweigenvector.h"
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
	// Update orbital page
	QTableWidgetItem *tabitem;
	ui.OrbitalTable->clear();
	ui.OrbitalTable->setRowCount(m->nEigenvectors());
	int count = 0;
	for (Eigenvector *vec = m->eigenvectors(); vec != NULL; vec = vec->next)
	{
		tabitem = new QTableWidgetItem();
		tabitem->setText(vec->name());
		ui.OrbitalTable->setItem(count, 0, tabitem);
		tabitem = new QTableWidgetItem();
		tabitem->setText(ftoa(vec->occupancy()));
		ui.OrbitalTable->setItem(count, 1, tabitem);
		tabitem = new QTableWidgetItem();
		tabitem->setText(ftoa(vec->eigenvalue()));
		ui.OrbitalTable->setItem(count, 2, tabitem);
		count++;
	}
	ui.OrbitalTable->resizeColumnToContents(0);
	ui.OrbitalTable->resizeColumnToContents(1);
	ui.OrbitalTable->resizeColumnToContents(2);
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
	// Set minimum, maximum, and cutoff, and stepsizes for spins
	ui.GridMinimumLabel->setText(ftoa(g->minimum()));
	ui.GridLowerCutoffSpin->setMinimum(g->minimum());
	ui.GridLowerCutoffSpin->setMaximum(g->maximum());
	ui.GridLowerCutoffSpin->setValue(g->lowerPrimaryCutoff());
	ui.GridLowerCutoffSpin->setSingleStep(g->maximum() / 100.0);
	ui.GridUpperCutoffSpin->setMinimum(g->minimum());
	ui.GridUpperCutoffSpin->setMaximum(g->maximum());
	ui.GridUpperCutoffSpin->setValue(g->upperPrimaryCutoff());
	ui.GridUpperCutoffSpin->setSingleStep(g->maximum() / 100.0);
	ui.GridLowerCutoff2Spin->setMinimum(g->minimum());
	ui.GridLowerCutoff2Spin->setMaximum(g->maximum());
	ui.GridLowerCutoff2Spin->setValue(g->lowerSecondaryCutoff());
	ui.GridLowerCutoff2Spin->setSingleStep(g->maximum() / 100.0);
	ui.GridUpperCutoff2Spin->setMinimum(g->minimum());
	ui.GridUpperCutoff2Spin->setMaximum(g->maximum());
	ui.GridUpperCutoff2Spin->setValue(g->upperSecondaryCutoff());
	ui.GridUpperCutoff2Spin->setSingleStep(g->maximum() / 100.0);
	ui.GridMaximumLabel->setText(ftoa(g->maximum()));
	ui.GridSecondaryCutoffCheck->setChecked( g->useSecondary() );
	ui.GridLowerCutoff2Spin->setEnabled( g->useSecondary() );
	ui.GridUpperCutoff2Spin->setEnabled( g->useSecondary() );
	// Set sum data labels
	ui.GridTotalPositiveSumLabel->setText(ftoa(g->totalPositiveSum()));
	ui.GridTotalNegativeSumLabel->setText(ftoa(g->totalNegativeSum()));
	double total = g->totalPositiveSum() + fabs(g->totalNegativeSum());
	ui.GridTotalAbsoluteLabel->setText(ftoa(total));
	ui.GridPrimarySumLabel->setText(ftoa(100.0*g->partialPrimarySum()/total));
	ui.GridSecondarySumLabel->setText(ftoa(100.0*g->partialSecondarySum()/total));
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
	ui.GridPrimaryColourFrame->setColour(g->primaryColour());
	ui.GridPrimaryColourFrame->update();
	ui.GridSecondaryColourFrame->setColour(g->secondaryColour());
	ui.GridSecondaryColourFrame->update();
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
	ui.GridSecondaryColourButton->setEnabled(TRUE);
	ui.GridPrimaryColourButton->setEnabled(TRUE);
	ui.GridColourscaleSpin->setEnabled(FALSE);
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	ui.GridSecondaryColourButton->setEnabled(g->useSecondary());
	g->setUseColourScale(FALSE);
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridUseColourScaleRadio_clicked(bool checked)
{
	ui.GridSecondaryColourButton->setEnabled(FALSE);
	ui.GridPrimaryColourButton->setEnabled(FALSE);
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

void AtenGrids::on_GridLowerCutoffSpin_editingFinished()
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setLowerPrimaryCutoff(ui.GridLowerCutoffSpin->value());
	refreshGridInfo();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridUpperCutoffSpin_editingFinished()
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setUpperPrimaryCutoff(ui.GridUpperCutoffSpin->value());
	refreshGridInfo();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridLowerCutoff2Spin_editingFinished()
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setLowerSecondaryCutoff(ui.GridLowerCutoff2Spin->value());
	refreshGridInfo();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridUpperCutoff2Spin_editingFinished()
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setUpperSecondaryCutoff(ui.GridUpperCutoff2Spin->value());
	refreshGridInfo();
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

void AtenGrids::on_GridPrimaryColourButton_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	// Get current surface colour and convert into a QColor
	double *col = g->primaryColour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour
	g->setPrimaryColour(newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	ui.GridPrimaryColourFrame->setColour(newcol);
	ui.GridPrimaryColourFrame->update();
	gui.mainView.postRedisplay();
}

void AtenGrids::on_GridSecondaryColourButton_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	// Get current surface colour and convert into a QColor
	double *col = g->secondaryColour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour
	g->setSecondaryColour(newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	ui.GridSecondaryColourFrame->setColour(newcol);
	ui.GridSecondaryColourFrame->update();
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

void AtenGrids::on_GridSecondaryCutoffCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setUseSecondary(checked);
	gui.mainView.postRedisplay();
}

/*
// Orbital Page
*/

void AtenGrids::on_ViewBasisButton_clicked(bool checked)
{
	gui.viewBasisDialog->showWindow( aten.currentModelOrFrame() );
}

void AtenGrids::on_ViewEigenvectorButton_clicked(bool checked)
{
	int row = ui.OrbitalTable->currentRow();
	if (row == -1) 
	{
		msg.print("No orbital selected!\n");
		return;
	}
	gui.viewEigenvectorDialog->showWindow( aten.currentModelOrFrame(), row);
}

void AtenGrids::on_OrbitalCalculateButton_clicked(bool checked)
{
	int row = ui.OrbitalTable->currentRow();
	if (row == -1)
	{
		msg.print("No orbital selected!\n");
		return;
	}
	// Generate a new grid in the current model
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->addGrid();
	// Set origin
	Vec3<double> origin(ui.OrbitalOriginXSpin->value(), ui.OrbitalOriginYSpin->value(), ui.OrbitalOriginZSpin->value());
	g->setOrigin(origin);
	// Initialise grid structure
	int npoints = ui.OrbitalPointsSpin->value();
	Vec3<int> iv(npoints, npoints, npoints);
	g->initialise(Grid::RegularXYZData, iv);
	double dx = ui.OrbitalSpacingSpin->value(); 
	g->setAxes(dx);
	// Generate gridpoints data
	Vec3<double> v;
	for (int i=0; i<npoints; ++i)
	{
		for (int j=0; j<npoints; ++j)
		{
			for (int k=0; k<npoints; ++k)
			{
				v.set(i,j,k);
				v *= dx;
				v += origin;
				g->setData(i, j, k, m->eigenvectorDensityAt(row, v));
			}
		}
	}
}

void AtenGrids::on_OrbitalOriginXSpin_valueChanged(double d)
{
}

void AtenGrids::on_OrbitalOriginYSpin_valueChanged(double d)
{
}

void AtenGrids::on_OrbitalOriginZSpin_valueChanged(double d)
{
}

void AtenGrids::on_OrbitalSpacingSpin_valueChanged(double d)
{
}

void AtenGrids::on_OrbitalPointsSpin_valueChanged(int i)
{
}

void AtenGrids::dialogFinished(int result)
{
	gui.mainWindow->ui.actionGridsWindow->setChecked(FALSE);
}
