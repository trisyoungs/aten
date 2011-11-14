/*
	*** Pores Dock Widget
	*** src/gui/pores_funcs.cpp
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
#include "gui/mainwindow.h"
#include "gui/pores.h"
#include "gui/gui.h"
#include "gui/toolbox.h"

// Constructor
PoresWidget::PoresWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	// Set up interface
	ui.setupUi(this);
	
	// Private variables
	//partitioningScheme_.
}

// Show window
void PoresWidget::showWidget()
{
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.PoresButton->setChecked(TRUE);
}

/*
// Drill Tab
*/

void PoresWidget::on_PoreSelectButton_clicked(bool checked)
{
	// First check - does the current model have a unit cell?
	Model *m = aten.currentModelOrFrame();
	if (m->cell()->type() == UnitCell::NoCell)
	{
		msg.print("Can't drill pores in a non-periodic model.\n");
		return;
	}

	// Grab some values so we are ready to run the command
	int nx = ui.ArrayXSpin->value(), ny = ui.ArrayYSpin->value();
	int face = ui.OriginPlaneCombo->currentIndex()+1;
	Vec3<double> v(ui.PoreVectorX->value(), ui.PoreVectorY->value(), ui.PoreVectorZ->value());
	Dnchar geometry = qPrintable(ui.PoreGeometryCombo->currentText());
	double sizeParam = ui.PoreSizeSpin->value();
	CommandNode::run(Command::SelectPores, "cdiiiddd", geometry.get(), sizeParam, nx, ny, face, v.x, v.y, v.z);
}

void PoresWidget::on_PoreSelectAndCutButton_clicked(bool checked)
{
	// First check - does the current model have a unit cell?
	Model *m = aten.currentModelOrFrame();
	if (m->cell()->type() == UnitCell::NoCell)
	{
		msg.print("Can't drill pores in a non-periodic model.\n");
		return;
	}

	// Grab some values so we are ready to run the command
	int nx = ui.ArrayXSpin->value(), ny = ui.ArrayYSpin->value();
	int face = ui.OriginPlaneCombo->currentIndex()+1;
	Vec3<double> v(ui.PoreVectorX->value(), ui.PoreVectorY->value(), ui.PoreVectorZ->value());
	Dnchar geometry = qPrintable(ui.PoreGeometryCombo->currentText());
	double sizeParam = ui.PoreSizeSpin->value();
	CommandNode::run(Command::DrillPores, "cdiiiddd", geometry.get(), sizeParam, nx, ny, face, v.x, v.y, v.z);
}

/*
// Terminate Tab
*/


/*
// Scheme Tab
*/

void PoresWidget::on_GenerateSchemeButton_clicked(bool checked)
{
}

void PoresWidget::on_CopySchemeButton_clicked(bool checked)
{
}

void PoresWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.PoresButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainCanvas()->postRedisplay();

	event->accept();
}