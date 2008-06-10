/*
	*** Qt forcefield action functions
	*** src/gui/forcefieldactions.cpp
	Copyright T. Youngs 2007,2008

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

#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/minimiser.h"
#include "base/master.h"
#include "model/model.h"

void AtenForm::on_actionMinimise_triggered(bool on)
{
	// Activate the slot for the 'Minimise' button on the minimiser window
	gui.minimiserWindow->doMinimisation();
}

void AtenForm::on_actionCalculateEnergy_triggered(bool on)
{
}

void AtenForm::on_actionCalculateForces_triggered(bool on)
{
}
