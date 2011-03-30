/*
	*** MD Dock Widget
	*** src/gui/md_funcs.cpp
	Copyright T. Youngs 2007-2009

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
#include "methods/md.h"
#include "gui/gui.h"
#include "gui/toolbox.h"
#include "gui/mainwindow.h"
#include "gui/md.h"

// Constructor
MDWidget::MDWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;

	// Public variables
}

void MDWidget::showWidget()
{
	show();
	refresh();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.AtomListButton->setChecked(TRUE);
}

void MDWidget::refresh()
{
	// Set controls to reflect values in singleton MDEngine
	refreshing_ = TRUE;
	ui.TemperatureSpin->setValue( md.temperature() );
	ui.PressureSpin->setValue( md.pressure() );
	ui.NStepsSpin->setValue( md.nSteps() );
	ui.TimeStepMantissaSpin->setValue( md.timeStep().mantissa() );
	ui.TimeStepExponentSpin->setValue( md.timeStep().exponent() );
	refreshing_ = FALSE;
}

/*
// Control Page
*/

void MDWidget::on_TemperatureSpin_valueChanged(double value)
{
	if (refreshing_) return;
	md.setTemperature(value);
}

void MDWidget::on_PressureSpin_valueChanged(double value)
{
	if (refreshing_) return;
	md.setPressure(value);
}

void MDWidget::on_NStepsSpin_valueChanged(int value)
{
	if (refreshing_) return;
	md.setNSteps(value);
}

void MDWidget::on_TimeStepMantissaSpin_valueChanged(double value)
{
	if (refreshing_) return;
	md.timeStep().setMantissa(value);
}

void MDWidget::on_TimeStepExponentSpin_valueChanged(int value)
{
	if (refreshing_) return;
	md.timeStep().setExponent(value);
}

void MDWidget::on_RunMDButton_clicked(bool checked)
{
	md.run();
}

/*
// Output Page
*/

/*
// Extra Page
*/

void MDWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.MDButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget->postRedisplay();
	event->accept();
}
