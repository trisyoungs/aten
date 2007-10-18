/*
	*** Qt surface functions interface
	*** src/gui-qt/surface_funcs.cpp

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
#include "gui-qt/mainwindow.h"
#include <QtGui/QFileDialog>

void AtenForm::refresh_surfacespage()
{
	// Clear and refresh the surfaces list
	ui.SurfaceList->clear();
	QListWidgetItem *item;
	for (surface *s = master.surfaces.first(); s != NULL; s = s->next)
	{
		item = new QListWidgetItem(ui.SurfaceList);
		item->setText(s->get_name());
	}
	// Select the first item
	if (master.surfaces.size() != 0) ui.SurfaceList->setCurrentRow(0);
}

void AtenForm::refresh_surfaceinfo()
{
	// Get the current row selected in the surface list
	surface *s;
	int row = ui.SurfaceList->currentRow();
	if (row == -1) return;
	else s = master.surfaces[row];
	// Set minimum, maximum, and cutoff
	ui.SurfaceMinimumLabel->setText(ftoa(s->get_minimum()));
	ui.SurfaceCutoffSpin->setMinimum(s->get_minimum());
	ui.SurfaceCutoffSpin->setValue(s->get_cutoff());
	ui.SurfaceCutoffSpin->setMaximum(s->get_maximum());
	ui.SurfaceMaximumLabel->setText(ftoa(s->get_maximum()));
	// Set origin
	vec3<double> origin = s->get_origin();
	ui.SurfaceOriginXSpin->setValue(origin.x);
	ui.SurfaceOriginYSpin->setValue(origin.y);
	ui.SurfaceOriginZSpin->setValue(origin.z);
	// Set axes
	mat3<double> axes = s->get_axes();
	axes.print();
	ui.SurfaceAxesAXSpin->setValue(axes.rows[0].x);
	ui.SurfaceAxesAYSpin->setValue(axes.rows[0].y);
	ui.SurfaceAxesAZSpin->setValue(axes.rows[0].z);
	ui.SurfaceAxesBXSpin->setValue(axes.rows[1].x);
	ui.SurfaceAxesBYSpin->setValue(axes.rows[1].y);
	ui.SurfaceAxesBZSpin->setValue(axes.rows[1].z);
	ui.SurfaceAxesCXSpin->setValue(axes.rows[2].x);
	ui.SurfaceAxesCYSpin->setValue(axes.rows[2].y);
	ui.SurfaceAxesCZSpin->setValue(axes.rows[2].z);

}

void AtenForm::surface_origin_changed(int component, double value)
{
	// Get the current row selected in the surface list
	surface *s;
	int row = ui.SurfaceList->currentRow();
	if (row == -1) return;
	else s = master.surfaces[row];
	// Get and re-set origin
	static vec3<double> o;
	o = s->get_origin();
	o.set(component, value);
	s->set_origin(o);
	gui.mainview.postredisplay();
}

void AtenForm::surface_axis_changed(int r, int component, double value)
{
	// Get the current row selected in the surface list
	surface *s;
	int row = ui.SurfaceList->currentRow();
	if (row == -1) return;
	else s = master.surfaces[row];
	// Get and re-set axes
	static mat3<double> axes;
	axes = s->get_axes();
	axes.rows[r].set(component, value);
	s->set_axes(axes);
	gui.mainview.postredisplay();
}

void AtenForm::on_LoadSurfaceButton_clicked(bool checked)
{
	filter *f;
	surface *s;
	QString filename;
	QStringList filenames;
	if (opensurfacedialog->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = opensurfacedialog->selectedFilter();
		// Find the corresponding Aten filter that was selected
		for (f = master.filters[FT_SURFACE_IMPORT].first(); f != NULL; f = f->next)
			if (strcmp(f->get_description(),qPrintable(filter)) == 0) break;
		// Get selected filename list
		filenames = opensurfacedialog->selectedFiles();
		// Loop over selected files
		for (int i = 0; i < filenames.size(); ++i)
		{
			filename = filenames.at(i);
			// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
			if (f != NULL) f->import_surface(qPrintable(filename));
			else
			{
				f = master.probe_file(qPrintable(filename), FT_SURFACE_IMPORT);
				if (f != NULL) f->import_surface(qPrintable(filename));
			}
		}
		refresh_surfacespage();
		gui.refresh();
	}
}

void AtenForm::on_SaveSurfaceButton_clicked(bool checked)
{
}

void AtenForm::on_SurfaceList_currentRowChanged(int row)
{
	// New item selected, so update the data shown in the page
	if (row != -1) refresh_surfaceinfo();
}

void AtenForm::on_SurfaceCutoffSpin_valueChanged(double d)
{
	// Get current surface in list
	int row = ui.SurfaceList->currentRow();
	if (row == -1) return;
	surface *s = master.surfaces[row];
	s->set_cutoff(d);
	gui.mainview.postredisplay();
}

void AtenForm::on_SurfaceStyleCombo_currentIndexChanged(int index)
{
	// Get current surface in list
	int row = ui.SurfaceList->currentRow();
	if (row == -1) return;
	surface *s = master.surfaces[row];
	s->set_style(surface_style (index));
	gui.mainview.postredisplay();
}
