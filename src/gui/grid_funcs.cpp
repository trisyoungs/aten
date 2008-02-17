/*
	*** Qt grid functions interface
	*** src/gui/grid_funcs.cpp
	Copyright T. Youngs 2007

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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include <QtGui/QFileDialog>
#include <QtGui/QColorDialog>

void AtenForm::refresh_gridspage()
{
	// Clear and refresh the grids list
	ui.GridList->clear();
	QListWidgetItem *item;
	for (grid *g = master.get_grids(); g != NULL; g = g->next)
	{
		item = new QListWidgetItem(ui.GridList);
		item->setText(g->get_name());
	}
	// Select the first item
	if (master.get_ngrids() != 0) ui.GridList->setCurrentRow(0);
}

void AtenForm::refresh_gridinfo()
{
	// Get the current row selected in the grid list
	grid *g;
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	else g = master.get_grid(row);
	// Set minimum, maximum, and cutoff
	ui.GridMinimumLabel->setText(ftoa(g->get_minimum()));
	ui.SurfaceCutoffSpin->setMinimum(g->get_minimum());
	ui.SurfaceCutoffSpin->setValue(g->get_cutoff());
	ui.SurfaceCutoffSpin->setMaximum(g->get_maximum());
	ui.GridMaximumLabel->setText(ftoa(g->get_maximum()));
	// Set origin
	vec3<double> origin = g->get_origin();
	ui.GridOriginXSpin->setValue(origin.x);
	ui.GridOriginYSpin->setValue(origin.y);
	ui.GridOriginZSpin->setValue(origin.z);
	// Set axes
	mat3<double> axes = g->get_axes();
	ui.GridAxesAXSpin->setValue(axes.rows[0].x);
	ui.GridAxesAYSpin->setValue(axes.rows[0].y);
	ui.GridAxesAZSpin->setValue(axes.rows[0].z);
	ui.GridAxesBXSpin->setValue(axes.rows[1].x);
	ui.GridAxesBYSpin->setValue(axes.rows[1].y);
	ui.GridAxesBZSpin->setValue(axes.rows[1].z);
	ui.GridAxesCXSpin->setValue(axes.rows[2].x);
	ui.GridAxesCYSpin->setValue(axes.rows[2].y);
	ui.GridAxesCZSpin->setValue(axes.rows[2].z);
	// Set colour and transparency
	GLint *col = g->get_colour();
	ui.SurfaceColourFrame->set_colour(col[0], col[1], col[2]);
	ui.SurfaceTransparencySpin->setValue( (double) col[3] / INT_MAX );
}

void AtenForm::grid_origin_changed(int component, double value)
{
	// Get the current row selected in the grid list
	grid *g;
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	else g = master.get_grid(row);
	// Get and re-set origin
	static vec3<double> o;
	o = g->get_origin();
	o.set(component, value);
	g->set_origin(o);
	gui.mainview.postredisplay();
}

void AtenForm::grid_axis_changed(int r, int component, double value)
{
	// Get the current row selected in the grid list
	grid *g;
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	else g = master.get_grid(row);
	// Get and re-set axes
	static mat3<double> axes;
	axes = g->get_axes();
	axes.rows[r].set(component, value);
	g->set_axes(axes);
	gui.mainview.postredisplay();
}

void AtenForm::on_SaveGridButton_clicked(bool checked)
{
}

void AtenForm::on_GridList_currentRowChanged(int row)
{
	// New item selected, so update the data shown in the page
	if (row != -1) refresh_gridinfo();
}

void AtenForm::on_SurfaceCutoffSpin_valueChanged(double d)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	grid *g = master.get_grid(row);
	g->set_cutoff(d);
	gui.mainview.postredisplay();
}

void AtenForm::on_SurfaceStyleCombo_currentIndexChanged(int index)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	grid *g = master.get_grid(row);
	g->set_style(surface_style (index));
	gui.mainview.postredisplay();
}

void AtenForm::on_SurfaceColourButton_clicked(bool checked)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	grid *g = master.get_grid(row);
	// Get current surface colour and convert into a QColor
	GLint *glcol = g->get_colour();
	QColor oldcol, newcol;
	oldcol.setRedF( double(glcol[0]) / INT_MAX );
	oldcol.setGreenF( double(glcol[1]) / INT_MAX );
	oldcol.setBlueF( double(glcol[2]) / INT_MAX );
	oldcol.setAlphaF( double(glcol[3]) / INT_MAX );
	// Request a colour dialog
	newcol = QColorDialog::getColor(oldcol, this);
	// Store new colour
	g->set_colour(newcol.redF(), newcol.greenF(), newcol.blueF());
	ui.SurfaceColourFrame->set_colour(newcol);
	gui.mainview.postredisplay();
}

void AtenForm::on_SurfaceTransparencySpin_valueChanged(double value)
{
	// Get current surface in list
	int row = ui.GridList->currentRow();
	if (row == -1) return;
	grid *g = master.get_grid(row);
	g->set_transparency( int (value * INT_MAX));
	gui.mainview.postredisplay();
}
