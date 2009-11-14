/*
	*** Qt commandhelp functions interface
	*** src/gui/commandhelp_funcs.cpp
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

#include "gui/commandhelp.h"
#include "command/commands.h"

// Constructor
AtenCommandHelp::AtenCommandHelp(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);
}

// Set controls
void AtenCommandHelp::setControls()
{
}

// Finalise GUI
void AtenCommandHelp::finaliseUi()
{
	// Add command names into combo box
	for (int cf = 0; cf < Command::nCommands; ++cf)
	{
		ui.SearchCombo->insertItem(cf, commands.data[cf].keyword);
	}
}

void AtenCommandHelp::on_SearchCombo_activated(int index)
{
	ui.SearchEdit->clear();
	if ((index < 0) || (index >= Command::nCommands))
	{
		printf("Not a valid command.\n");
		return;
	}

	char s[500];
	if (commands.data[index].hasArguments()) sprintf(s, "<b>%s(%s)</b><br/>       %s", commands.data[index].keyword, commands.data[index].argText, commands.data[index].syntax);
	else sprintf(s, "<b>%s</b><br/>       %s", commands.data[index].keyword, commands.data[index].syntax);

	ui.SearchEdit->insertHtml(s);
}

void AtenCommandHelp::on_CloseButton_clicked(bool checked)
{
	// Remove default button behaviour
	ui.CloseButton->setDefault(FALSE);
	accept();
}

void AtenCommandHelp::keyPressEvent(QKeyEvent *event)
{
	event->ignore();
}
