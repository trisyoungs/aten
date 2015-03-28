/*
	*** Grids Dock Widget
	*** src/gui/grids_funcs.cpp
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

#include <QtGui/QCloseEvent>
#include <QtGui/QFileDialog>
#include <QtGui/QColorDialog>
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/grids.h"
#include "gui/viewbasis.h"
#include "gui/vieweigenvector.h"
#include "gui/tlistwidgetitem.h"
#include "base/sysfunc.h"

// Constructor
GridsWidget::GridsWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
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
}

// Refresh widget
void GridsWidget::refresh()
{
	Messenger::enter("GridsWidget::refresh");

	refreshing_ = TRUE;

	// Clear and refresh the grids list
	ui.GridList->clear();

	if (!parent_.aten().currentModelOrFrame())
	{
		refreshing_ = false;
		Messenger::exit("GridsWidget::refresh");
		return;
	}

	Model* m;
	if (ui.ShowAllGridsCheck->isChecked())
	{
		// Need to loop over models, frames (if any) and grids
		for (m = parent_.aten().models(); m != NULL; m = m->next)
		{
			if (m->hasTrajectory())
			{
				// If it's *not* a cached trajectory, only do the current frame
				if (!m->trajectoryIsCached()) for (Grid* g = m->trajectoryCurrentFrame()->grids(); g != NULL; g = g->next) addGridToList(g);
				else
				{
					for (int n=0; n<m->nTrajectoryFrames(); ++n)
						for (Grid* g = m->trajectoryFrame(n)->grids(); g != NULL; g = g->next) addGridToList(g);
				}
			}
			else for (Grid* g = m->grids(); g != NULL; g = g->next) addGridToList(g);
		}
	}
	else 
	{
		m = parent_.aten().currentModelOrFrame();
		for (Grid* g = m->grids(); g != NULL; g = g->next) addGridToList(g);
	}

	// Select the first item
	ui.GridList->setCurrentRow(0);
	refreshGridInfo();

	// Update orbital page
	QTableWidgetItem *tabitem;
	ui.OrbitalTable->clear();
	m = parent_.aten().currentModelOrFrame();
	ui.OrbitalTable->setRowCount(m->nEigenvectors());
	int count = 0;
	for (Eigenvector *vec = m->eigenvectors(); vec != NULL; vec = vec->next)
	{
		tabitem = new QTableWidgetItem();
		tabitem->setText(vec->name());
		ui.OrbitalTable->setItem(count, 0, tabitem);
		tabitem = new QTableWidgetItem();
		tabitem->setText(QString::number(vec->occupancy()));
		ui.OrbitalTable->setItem(count, 1, tabitem);
		tabitem = new QTableWidgetItem();
		tabitem->setText(QString::number(vec->eigenvalue()));
		ui.OrbitalTable->setItem(count, 2, tabitem);
		count++;
	}
	ui.OrbitalTable->resizeColumnToContents(0);
	ui.OrbitalTable->resizeColumnToContents(1);
	ui.OrbitalTable->resizeColumnToContents(2);
	refreshing_ = FALSE;
	Messenger::exit("GridsWidget::refresh");
}

Grid* GridsWidget::getCurrentGrid()
{
	// Return first selected grid in widget
	QList<QListWidgetItem*> selection = ui.GridList->selectedItems();
	if (selection.size() == 0) return NULL;
	TListWidgetItem *item = (TListWidgetItem*) ui.GridList->currentItem();
	Grid* g = (Grid*) item->data.asPointer(VTypes::GridData);
	return g;
}

void GridsWidget::addGridToList(Grid* g)
{
	TListWidgetItem *item = new TListWidgetItem(ui.GridList);
	item->setText(g->name());
	item->setCheckState(g->isVisible() ? Qt::Checked : Qt::Unchecked);
	item->data.set(VTypes::GridData, g);
}

void GridsWidget::refreshGridInfo()
{
	Messenger::enter("GridsWidget::refreshGridInfo");
	// Get the current row selected in the grid list
	Grid* g = getCurrentGrid();
	if (g == NULL)
	{
		Messenger::exit("GridsWidget::refreshGridInfo");
		return;
	}

	refreshing_ = TRUE;
	// Set minimum, maximum, and cutoff, and stepsizes for spins
	ui.GridMinimumLabel->setText(QString::number(g->minimum()));
	ui.GridMaximumLabel->setText(QString::number(g->maximum()));
	ui.GridNPointsLabel->setText(QString::number(g->nXYZ().x*g->nXYZ().y*g->nXYZ().z));
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
	ui.GridTotalPositiveSumLabel->setText(QString::number(g->totalPositiveSum()));
	ui.GridTotalNegativeSumLabel->setText(QString::number(g->totalNegativeSum()));
	double total = g->totalPositiveSum() + fabs(g->totalNegativeSum());
	ui.GridTotalAbsoluteLabel->setText(QString::number(total));
	ui.GridPrimaryPercentLabel->setText(QString::number(100.0*g->partialPrimarySum()/total));
	ui.GridSecondaryPercentLabel->setText(QString::number(100.0*g->partialSecondarySum()/total));
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
	ui.GridFillEnclosedVolumeCheck->setChecked(g->fillEnclosedVolume());
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
	Messenger::exit("GridsWidget::refreshGridInfo");
}

// Load grid (public function)
void GridsWidget::loadGrid()
{
	Messenger::enter("GridsWidget::loadGrid");
	Tree* filter;
	static QDir currentDirectory_(parent_.aten().workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Grid", currentDirectory_.path(), parent_.aten().fileDialogFilters(FilterData::GridImport), &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		
		// Find the filter that was selected
		filter = parent_.aten().findFilterByDescription(FilterData::GridImport, qPrintable(selFilter));
		if (filter == NULL) filter = parent_.aten().probeFile(qPrintable(filename), FilterData::GridImport);
		if (filter != NULL)
		{
			// Run any import options in the filter
			if (!filter->executeRead(qPrintable(filename))) return;
		}
	}
	refresh();
	parent_.postRedisplay();
	Messenger::exit("GridsWidget::loadGrid");
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
	Grid* g = getCurrentGrid();
	if (g == NULL)
	{
		Messenger::print("No grid selected to copy.");
		return;
	}
	parent_.aten().copyGrid(g);
}

void GridsWidget::on_actionGridCut_triggered(bool checked)
{
	Grid* g = getCurrentGrid();
	if (g == NULL)
	{
		Messenger::print("No grid selected to cut.");
		return;
	}
	Model* m = parent_.aten().currentModelOrFrame();
	parent_.aten().copyGrid(g);
	m->removeGrid(g);
	refresh();
	parent_.postRedisplay();
}

void GridsWidget::on_actionGridDelete_triggered(bool checked)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		Model* m = g->parent();
		m->removeGrid(g);
	}
	refresh();
	parent_.postRedisplay();
}

void GridsWidget::on_actionGridPaste_triggered(bool checked)
{
	Grid* g = parent_.aten().gridClipboard();
	if (g == NULL)
	{
		Messenger::print("No grid data on clipboard.");
		return;
	}
	Model* m = parent_.aten().currentModelOrFrame();
	Grid* newgrid = m->addGrid();
	*newgrid = *g;
	refresh();
	parent_.postRedisplay();
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
	Grid* g = getCurrentGrid();
	if (g == NULL) return;
	ui.GridSecondaryColourButton->setEnabled(g->useSecondary());
	g->setUseColourScale(FALSE);
	parent_.postRedisplay();
}

void GridsWidget::on_GridUseColourScaleRadio_clicked(bool checked)
{
	ui.GridSecondaryColourButton->setEnabled(FALSE);
	ui.GridPrimaryColourButton->setEnabled(FALSE);
	ui.GridColourscaleSpin->setEnabled(TRUE);
	if (refreshing_) return;

	// Get current grid and set data
	Grid* g = getCurrentGrid();
	if (g == NULL) return;
	g->setUseColourScale(TRUE);
	parent_.postRedisplay();
}

void GridsWidget::gridOriginChanged(int component, double value)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		// Get and re-set origin
		static Vec3<double> o;
		o = g->origin();
		o.set(component, value);
		g->setOrigin(o);
	}
	parent_.postRedisplay();
}

void GridsWidget::gridAxisChanged(int axis, int component, double value)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		// Get and re-set axes
		Matrix axes;
		axes = g->axes();
		axes[axis*4+component] = value;
		g->setAxes(axes);
	}
	parent_.postRedisplay();
}

void GridsWidget::on_GridList_currentRowChanged(int row)
{
	if (refreshing_) return;
	// New item selected, so update the data shown in the page
	if (row != -1) refreshGridInfo();
}

// Item in grid list has changed?
void GridsWidget::on_GridList_itemClicked(QListWidgetItem* item)
{
	// Cast item to our own TListWidgetItem
	TListWidgetItem *titem = (TListWidgetItem*) item;
	// Get grid associated to item
	Grid* g = (Grid*) titem->data.asPointer(VTypes::GridData);
	// Look at checked state
	g->setVisible( (titem->checkState() == Qt::Checked ? TRUE : FALSE) );
	parent_.postRedisplay();
}

void GridsWidget::on_GridList_itemSelectionChanged()
{
	refreshGridInfo();
}

void GridsWidget::on_ShowAllGridsCheck_clicked(bool checked)
{
	refresh();
}

void GridsWidget::on_GridLowerCutoffSpin_editingFinished()
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setLowerPrimaryCutoff(ui.GridLowerCutoffSpin->value());
	}
	refreshGridInfo();
	parent_.postRedisplay();
}

void GridsWidget::on_GridUpperCutoffSpin_editingFinished()
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setUpperPrimaryCutoff(ui.GridUpperCutoffSpin->value());
	}
	refreshGridInfo();
	parent_.postRedisplay();
}

void GridsWidget::on_GridLowerCutoff2Spin_editingFinished()
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setLowerSecondaryCutoff(ui.GridLowerCutoff2Spin->value());
	}
	refreshGridInfo();
	parent_.postRedisplay();
}

void GridsWidget::on_GridUpperCutoff2Spin_editingFinished()
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setUpperSecondaryCutoff(ui.GridUpperCutoff2Spin->value());
	}
	refreshGridInfo();
	parent_.postRedisplay();
}

void GridsWidget::on_GridStyleCombo_currentIndexChanged(int index)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setStyle(Grid::SurfaceStyle (index));
	}
	parent_.postRedisplay();
}

void GridsWidget::on_GridOutlineVolumeCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setOutlineVolume(checked);
	}
	parent_.postRedisplay();
}

void GridsWidget::on_GridFillEnclosedVolumeCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setFillEnclosedVolume(checked);
	}
	parent_.postRedisplay();
}

void GridsWidget::on_GridPeriodicCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setPeriodic(checked);
	}
	parent_.postRedisplay();
}

void GridsWidget::on_GridPrimaryColourButton_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid* g = getCurrentGrid();
	if (g == NULL) return;
	// Get current surface colour and convert into a QColor
	double* col = g->primaryColour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour
	// Get currently selected grid(s) and set data
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setPrimaryColour(newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	}
	ui.GridPrimaryColourFrame->setColour(newcol);
	ui.GridPrimaryColourFrame->update();
	parent_.postRedisplay();
}

void GridsWidget::on_GridSecondaryColourButton_clicked(bool checked)
{
	if (refreshing_) return;
	// Get current grid and set data
	Grid* g = getCurrentGrid();
	if (g == NULL) return;
	// Get current surface colour and convert into a QColor
	double* col = g->secondaryColour();
	QColor oldcol, newcol;
	oldcol.setRgbF( col[0], col[1], col[2], col[3] );
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour
	// Get currently selected grid(s) and set data
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setSecondaryColour(newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	}
	ui.GridSecondaryColourFrame->setColour(newcol);
	ui.GridSecondaryColourFrame->update();
	parent_.postRedisplay();
}

void GridsWidget::on_GridColourscaleSpin_valueChanged(int n)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setColourScale(n-1);
	}
	QString scalename = "(";
	scalename += prefs.colourScale[g->colourScale()].name();
	scalename += ")";
	ui.GridColourscaleName->setText(scalename);
	parent_.postRedisplay();
}

void GridsWidget::on_GridSecondaryCutoffCheck_clicked(bool checked)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		g->setUseSecondary(checked);
		ui.GridLowerCutoff2Spin->setEnabled( g->useSecondary() );
		ui.GridUpperCutoff2Spin->setEnabled( g->useSecondary() );
	}
	parent_.postRedisplay();
}


/*
// Shift Page
*/

void GridsWidget::gridShiftChanged()
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		TListWidgetItem *item = (TListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) item->data.asPointer(VTypes::GridData);
		// Grab old shift values
		Vec3<int> oldshift = g->shift();
		g->setShift(ui.GridShiftXSpin->value(), ui.GridShiftYSpin->value(), ui.GridShiftZSpin->value());
		if (ui.ShiftAtomNoneRadio->isChecked() == FALSE)
		{
			Model* m = g->parent();
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
	}
	parent_.postRedisplay();
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
	AtenViewBasis basisView(this);
	basisView.showWindow( parent_.aten().currentModelOrFrame() );
}

void GridsWidget::on_ViewEigenvectorButton_clicked(bool checked)
{
	int row = ui.OrbitalTable->currentRow();
	if (row == -1) 
	{
		Messenger::print("No orbital selected!");
		return;
	}

	AtenViewEigenvector eigenView(this);
	eigenView.showWindow( parent_.aten().currentModelOrFrame(), row);
}

void GridsWidget::on_OrbitalCalculateButton_clicked(bool checked)
{
	int row = ui.OrbitalTable->currentRow();
	if (row == -1)
	{
		Messenger::print("No orbital selected!");
		return;
	}
	// Generate a new grid in the current model
	Model* m = parent_.aten().currentModelOrFrame();
	Grid* g = m->addGrid();
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

void GridsWidget::closeEvent(QCloseEvent* event)
{
	event->accept();
}
