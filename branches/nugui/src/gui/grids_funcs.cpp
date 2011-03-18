/*
	*** Grids Dock Widget
	*** src/gui/grids_funcs.cpp
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

#include "main/aten.h"
#include "model/model.h"
#include "gui/mainwindow.h"
#include "gui/grids.h"
#include "gui/gui.h"
#include "gui/toolbox.h"
#include "gui/viewbasis.h"
#include "gui/vieweigenvector.h"
#include "gui/tlistwidgetitem.h"
#include "classes/grid.h"
#include "base/sysfunc.h"

// Constructor
GridsWidget::GridsWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
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

void GridsWidget::showWidget()
{
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.AtomListButton->setChecked(TRUE);
}

// Refresh widget
void GridsWidget::refresh()
{
	msg.enter("GridsWidget::refresh");
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
	msg.exit("GridsWidget::refresh");
}

Grid *GridsWidget::getCurrentGrid()
{
	Model *m = aten.currentModelOrFrame();
	if (m == NULL)
	{
		printf("Internal Error: No current model in Grids window.\n");
		return NULL;
	}
	int row = ui.GridList->currentRow();
	if (row == -1) return NULL;
	else return m->grid(row);
}

void GridsWidget::refreshGridInfo()
{
	msg.enter("GridsWidget::refreshGridInfo");
	// Get the current row selected in the grid list
	Grid *g;
	Model *m = aten.currentModelOrFrame();
	int row = ui.GridList->currentRow();
	if (row == -1)
	{
		msg.exit("GridsWidget::refreshGridInfo");
		return;
	}
	else g = m->grid(row);
	refreshing_ = TRUE;
	// Set minimum, maximum, and cutoff, and stepsizes for spins
	ui.GridMinimumLabel->setText(ftoa(g->minimum()));
	ui.GridMaximumLabel->setText(ftoa(g->maximum()));
	ui.GridNPointsLabel->setText(itoa(g->nPoints().x*g->nPoints().y*g->nPoints().z));
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
	ui.GridSecondaryCutoffCheck->setChecked( g->useSecondary() );
	ui.GridLowerCutoff2Spin->setEnabled( g->useSecondary() );
	ui.GridUpperCutoff2Spin->setEnabled( g->useSecondary() );
	// Set sum data labels
	ui.GridTotalPositiveSumLabel->setText(ftoa(g->totalPositiveSum()));
	ui.GridTotalNegativeSumLabel->setText(ftoa(g->totalNegativeSum()));
	double total = g->totalPositiveSum() + fabs(g->totalNegativeSum());
	ui.GridTotalAbsoluteLabel->setText(ftoa(total));
	ui.GridPrimaryPercentLabel->setText(ftoa(100.0*g->partialPrimarySum()/total));
	ui.GridSecondaryPercentLabel->setText(ftoa(100.0*g->partialSecondarySum()/total));
	// Set origin and axes
	Vec3<double> origin = g->origin();
	ui.GridOriginXSpin->setValue(origin.x);
	ui.GridOriginYSpin->setValue(origin.y);
	ui.GridOriginZSpin->setValue(origin.z);
	Matrix axes = g->axes();
	ui.GridAxesAXSpin->setValue(axes[0]);
	ui.GridAxesAYSpin->setValue(axes[1]);
	ui.GridAxesAZSpin->setValue(axes[2]);
	ui.GridAxesBXSpin->setValue(axes[4]);
	ui.GridAxesBYSpin->setValue(axes[5]);
	ui.GridAxesBZSpin->setValue(axes[6]);
	ui.GridAxesCXSpin->setValue(axes[8]);
	ui.GridAxesCYSpin->setValue(axes[9]);
	ui.GridAxesCZSpin->setValue(axes[10]);
	// Set surface style data
	ui.GridStyleCombo->setCurrentIndex(g->style());
	ui.GridOutlineVolumeCheck->setChecked(g->outlineVolume());
	ui.GridPeriodicCheck->setChecked(g->periodic());
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
	// Shift data
	ui.GridShiftXSpin->setValue(g->shift().x);
	ui.GridShiftYSpin->setValue(g->shift().y);
	ui.GridShiftZSpin->setValue(g->shift().z);
	refreshing_ = FALSE;
	msg.exit("GridsWidget::refreshGridInfo");
}

// Load grid (public function)
void GridsWidget::loadGrid()
{
	msg.enter("GridsWidget::loadGrid");
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
		if (filter == NULL) filter = aten.probeFile(qPrintable(filename), FilterData::GridImport);
		if (filter != NULL)
		{
			// Run any import options in the filter
			if (!filter->executeCustomDialog()) return;
			filter->executeRead(qPrintable(filename));
		}
	}
	refresh();
	gui.mainWidget->postRedisplay();
	msg.exit("GridsWidget::loadGrid");
}

/*
// Menu
*/

void GridsWidget::on_actionGridLoad_triggered(bool checked)
{
	loadGrid();
}

void GridsWidget::on_actionGridCopy_triggered(bool checked)
{
	Grid *g = getCurrentGrid();
	if (g == NULL)
	{
		msg.print("No grid selected to copy.\n");
		return;
	}
	aten.copyGrid(g);
}

void GridsWidget::on_actionGridCut_triggered(bool checked)
{
	Grid *g = getCurrentGrid();
	if (g == NULL)
	{
		msg.print("No grid selected to cut.\n");
		return;
	}
	Model *m = aten.currentModelOrFrame();
	aten.copyGrid(g);
	m->removeGrid(g);
	refresh();
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_actionGridDelete_triggered(bool checked)
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
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_actionGridPaste_triggered(bool checked)
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
	gui.mainWidget->postRedisplay();
}

/*
// Origin / Axes
*/

void GridsWidget::on_GridOriginXSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridOriginChanged(0, d);
}

void GridsWidget::on_GridOriginYSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridOriginChanged(1, d);
}

void GridsWidget::on_GridOriginZSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridOriginChanged(2, d);
}

void GridsWidget::on_GridAxesAXSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(0,0, d);
}

void GridsWidget::on_GridAxesAYSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(0,1, d);
}

void GridsWidget::on_GridAxesAZSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(0,2, d);
}

void GridsWidget::on_GridAxesBXSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(1,0, d);
}

void GridsWidget::on_GridAxesBYSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(1,1, d);
}

void GridsWidget::on_GridAxesBZSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(1,2, d);
}

void GridsWidget::on_GridAxesCXSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(2,0, d);
}

void GridsWidget::on_GridAxesCYSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(2,1, d);
}

void GridsWidget::on_GridAxesCZSpin_valueChanged(double d)
{
	if (refreshing_) return;
	gridAxisChanged(2,2, d);
}

void GridsWidget::on_GridUseInternalColoursRadio_clicked(bool checked)
{
	ui.GridSecondaryColourButton->setEnabled(TRUE);
	ui.GridPrimaryColourButton->setEnabled(TRUE);
	ui.GridColourscaleSpin->setEnabled(FALSE);
	if (refreshing_) return;

	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	ui.GridSecondaryColourButton->setEnabled(g->useSecondary());
	g->setUseColourScale(FALSE);
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridUseColourScaleRadio_clicked(bool checked)
{
	ui.GridSecondaryColourButton->setEnabled(FALSE);
	ui.GridPrimaryColourButton->setEnabled(FALSE);
	ui.GridColourscaleSpin->setEnabled(TRUE);
	if (refreshing_) return;

	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setUseColourScale(TRUE);
	gui.mainWidget->postRedisplay();
}

// Item in grid list has changed?
void GridsWidget::on_GridList_itemClicked(QListWidgetItem *item)
{
	// Cast item to our own TListWidgetItem
	TListWidgetItem *titem = (TListWidgetItem*) item;
	// Get grid associated to item
	Grid *g = (Grid*) titem->data.asPointer(VTypes::GridData);
	// Look at checked state
	g->setVisible( (titem->checkState() == Qt::Checked ? TRUE : FALSE) );
	gui.mainWidget->postRedisplay();
}

void GridsWidget::gridOriginChanged(int component, double value)
{
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	// Get and re-set origin
	static Vec3<double> o;
	o = g->origin();
	o.set(component, value);
	g->setOrigin(o);
	gui.mainWidget->postRedisplay();
}

void GridsWidget::gridAxisChanged(int axis, int component, double value)
{
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	// Get and re-set axes
	Matrix axes;
	axes = g->axes();
	axes[axis*4+component] = value;
	g->setAxes(axes);
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridList_currentRowChanged(int row)
{
	if (refreshing_) return;
	// New item selected, so update the data shown in the page
	if (row != -1) refreshGridInfo();
}

void GridsWidget::on_GridLowerCutoffSpin_editingFinished()
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setLowerPrimaryCutoff(ui.GridLowerCutoffSpin->value());
	refreshGridInfo();
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridUpperCutoffSpin_editingFinished()
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setUpperPrimaryCutoff(ui.GridUpperCutoffSpin->value());
	refreshGridInfo();
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridLowerCutoff2Spin_editingFinished()
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setLowerSecondaryCutoff(ui.GridLowerCutoff2Spin->value());
	refreshGridInfo();
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridUpperCutoff2Spin_editingFinished()
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setUpperSecondaryCutoff(ui.GridUpperCutoff2Spin->value());
	refreshGridInfo();
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridStyleCombo_currentIndexChanged(int index)
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setStyle(Grid::SurfaceStyle (index));
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridOutlineVolumeCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Grid *g = m->grid(row);
	g->setOutlineVolume(checked);
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridPeriodicCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setPeriodic(checked);
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridPrimaryColourButton_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
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
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridSecondaryColourButton_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
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
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridColourscaleSpin_valueChanged(int n)
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setColourScale(n-1);
	QString scalename = "(";
	scalename += prefs.colourScale[g->colourScale()].name();
	scalename += ")";
	ui.GridColourscaleName->setText(scalename);
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_GridSecondaryCutoffCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	g->setUseSecondary(checked);
	ui.GridLowerCutoff2Spin->setEnabled( g->useSecondary() );
	ui.GridUpperCutoff2Spin->setEnabled( g->useSecondary() );
	gui.mainWidget->postRedisplay();
}


/*
// Shift Page
*/

void GridsWidget::gridShiftChanged()
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid *g = getCurrentGrid();
	if (g == NULL) return;
	// Grab old shift values
	Vec3<int> oldshift = g->shift();
	g->setShift(ui.GridShiftXSpin->value(), ui.GridShiftYSpin->value(), ui.GridShiftZSpin->value());
	if (ui.ShiftAtomNoneRadio->isChecked() == FALSE)
	{
		Model *m = aten.currentModelOrFrame();
		// Determine shift amount...
		Vec3<int> delta = g->shift() - oldshift;
		Vec3<double> vec;
		vec += g->axes().columnAsVec3(0) * delta.x;
		vec += g->axes().columnAsVec3(1) * delta.y;
		vec += g->axes().columnAsVec3(2) * delta.z;
		// Move atoms....
		m->beginUndoState("Shift atoms with grid");
		if (ui.ShiftAtomAllRadio->isChecked())
		{
			m->markAll();
			m->translateSelectionLocal(vec, TRUE);
		}
		else m->translateSelectionLocal(vec, FALSE);
		m->endUndoState();
	}
	gui.mainWidget->postRedisplay();
}

void GridsWidget::on_ShiftGridPosXButton_clicked(bool checked)
{
	ui.GridShiftXSpin->stepUp();
	gridShiftChanged();
}

void GridsWidget::on_ShiftGridPosYButton_clicked(bool checked)
{
	ui.GridShiftYSpin->stepUp();
	gridShiftChanged();
}

void GridsWidget::on_ShiftGridPosZButton_clicked(bool checked)
{
	ui.GridShiftZSpin->stepUp();
	gridShiftChanged();
}

void GridsWidget::on_ShiftGridNegXButton_clicked(bool checked)
{
	ui.GridShiftXSpin->stepDown();
	gridShiftChanged();
}

void GridsWidget::on_ShiftGridNegYButton_clicked(bool checked)
{
	ui.GridShiftYSpin->stepDown();
	gridShiftChanged();
}

void GridsWidget::on_ShiftGridNegZButton_clicked(bool checked)
{
	ui.GridShiftZSpin->stepDown();
	gridShiftChanged();
}

void GridsWidget::on_GridShiftXSpin_valueChanged(int i)
{
	gridShiftChanged();
}

void GridsWidget::on_GridShiftYSpin_valueChanged(int i)
{
	gridShiftChanged();
}

void GridsWidget::on_GridShiftZSpin_valueChanged(int i)
{
	gridShiftChanged();
}

/*
// Orbital Page
*/

void GridsWidget::on_ViewBasisButton_clicked(bool checked)
{
	gui.viewBasisDialog->showWindow( aten.currentModelOrFrame() );
}

void GridsWidget::on_ViewEigenvectorButton_clicked(bool checked)
{
	int row = ui.OrbitalTable->currentRow();
	if (row == -1) 
	{
		msg.print("No orbital selected!\n");
		return;
	}
	gui.viewEigenvectorDialog->showWindow( aten.currentModelOrFrame(), row);
}

void GridsWidget::on_OrbitalCalculateButton_clicked(bool checked)
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

void GridsWidget::on_OrbitalOriginXSpin_valueChanged(double d)
{
}

void GridsWidget::on_OrbitalOriginYSpin_valueChanged(double d)
{
}

void GridsWidget::on_OrbitalOriginZSpin_valueChanged(double d)
{
}

void GridsWidget::on_OrbitalSpacingSpin_valueChanged(double d)
{
}

void GridsWidget::on_OrbitalPointsSpin_valueChanged(int i)
{
}

void GridsWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.GridsButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget->postRedisplay();
	event->accept();
}
