/*
	*** Grids Dock Widget
	*** src/gui/grids_funcs.cpp
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
