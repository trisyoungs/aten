/*
	*** Qt edit functions interface
	*** src/gui/edit_funcs.cpp
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
#include "gui/mainwindow.h"
#include "gui/build.h"
#include "gui/gui.h"
#include "model/model.h"
#include "command/staticcommand.h"

// Constructor
AtenBuild::AtenBuild(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
}

// Destructor
AtenBuild::~AtenBuild()
{
}

// Show window
void AtenBuild::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

void AtenBuild::on_AddAtomButton_clicked(bool on)
{
	static StaticCommandNode cmd(Command::CA_NEWATOM, "iddd", 1, 0.0, 0.0, 0.0);
	static StaticCommandNode cmdfrac(Command::CA_NEWATOMFRAC, "iddd", 1, 0.0, 0.0, 0.0);
	if (ui.AddAtomFractionalCheck->isChecked())
	{
		cmdfrac.pokeArguments("iddd", aten.sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
		cmdfrac.execute();
	}
	else
	{
		cmd.pokeArguments("iddd", aten.sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
		cmd.execute();
	}
	gui.modelChanged();
}

void AtenBuild::dialogFinished(int result)
{
	gui.mainWindow->ui.actionBuildWindow->setChecked(FALSE);
}
