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

#include <QCloseEvent>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QColorDialog>
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/grids.h"
#include "gui/viewbasis.h"
#include "gui/vieweigenvector.h"
#include "templates/variantpointer.h"


// Refresh widget
void GridsWidget::refresh()
{
	Messenger::enter("GridsWidget::refresh");

	refreshing_ = true;

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
	refreshing_ = false;
	Messenger::exit("GridsWidget::refresh");
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
	parent_.updateWidgets(AtenWindow::MainViewTarget);
	Messenger::exit("GridsWidget::loadGrid");
}

/*
 * Menu
 */

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
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridsWidget::on_actionGridDelete_triggered(bool checked)
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* g;
	foreach (QListWidgetItem* qlwi, ui.GridList->selectedItems())
	{
		QListWidgetItem *item = (QListWidgetItem*) qlwi;
		// Get grid pointer
		g = (Grid*) VariantPointer<Grid>(item->data(Qt::UserRole));
		Model* m = g->parent();
		m->removeGrid(g);
	}
	refresh();
	parent_.updateWidgets(AtenWindow::MainViewTarget);
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
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Orbital Page
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
