/*
	*** Qt command dialog functions
	*** src/gui/command_funcs.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/command.h"

// Constructor
AtenCommand::AtenCommand(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);

	// Private variables

	// Public variables
}

// Destructor
AtenCommand::~AtenCommand()
{
}

void AtenCommand::showWindow()
{
	refresh();
	show();
}

void AtenCommand::refresh()
{
}

void AtenCommand::dialogFinished(int result)
{
	gui.mainWindow->ui.actionCommandWindow->setChecked(FALSE);
}

/*
// Script Tab
*/

/*
// Prompt Tab
*/

void AtenCommand::on_CommandPrompt_returnPressed()
{
	// Grab the current text of the line edit (and clear it at the same time)
	if (aten.tempScript.generateFromString(ui.CommandPrompt->getText()))
	{
		ReturnValue result;
		aten.tempScript.executeAll(result);
	}
	// Force update of the GUI?
	if (ui.PromptForceUpdateCheck->isChecked()) gui.update();
}

