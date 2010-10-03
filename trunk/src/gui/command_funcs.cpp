/*
	*** Qt Command / Script dialog functions
	*** src/gui/command_funcs.cpp
	Copyright T. Youngs 2007-2010

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
	repopulateCommandList(NULL);
	refreshScripts();
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

// Set list of commands in command tab
void AtenCommand::setCommandList(QStringList cmds)
{
	ui.CommandPrompt->setCommandList(cmds);
}

// Return list of commands stored in command tab
QStringList AtenCommand::commandList()
{
	return ui.CommandPrompt->commandList();
}

/*
// Prompt Tab
*/

void AtenCommand::on_CommandPrompt_returnPressed()
{
	Forest tempScript;
	// Grab the current text of the line edit (and clear it at the same time)
	if (tempScript.generateFromString(ui.CommandPrompt->getText()))
	{
		ReturnValue result;
		tempScript.executeAll(result);
	}
	// Force update of the GUI?
	if (ui.PromptForceUpdateCheck->isChecked()) gui.update();
}

/*
// Scripts Tab
*/

void AtenCommand::refreshScripts()
{
	ui.ScriptsList->clear();
	for (Forest *script = aten.scripts(); script != NULL; script = script->next) ui.ScriptsList->addItem(script->filename());
}

void AtenCommand::on_OpenScriptButton_clicked(bool v)
{
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Script", currentDirectory_.path(), gui.mainWindow->loadScriptFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Create script and model variables within it
		Forest *ca = aten.addScript();
		if (ca->generateFromFile(qPrintable(filename)))
		{
			msg.print("Script file '%s' loaded succesfully.\n", qPrintable(filename));
			ui.ScriptsList->addItem(ca->filename());
		}
		else
		{
			aten.removeScript(ca);
			msg.print("Failed to load script file '%s'.\n", qPrintable(filename));
		}
	}
}

void AtenCommand::on_ReloadAllButton_clicked(bool checked)
{
	// Cycle over scripts, clearing and reloading
	for (Forest *script = aten.scripts(); script != NULL; script = script->next)
	{
		printf("Filename = %s\n", script->filename());
	}
}

void AtenCommand::on_ScriptsList_currentRowChanged(int row)
{
	if (row == -1) ui.RunSelectedButton->setEnabled(FALSE);
	else ui.RunSelectedButton->setEnabled(TRUE);
}

void AtenCommand::on_RunSelectedButton_clicked(bool checked)
{
	int row = ui.ScriptsList->currentRow();
	if (row == -1) return;
	Forest *script = aten.script(row);
	if (script != NULL)
	{
		// Execute the script
		msg.print("Executing script '%s':\n",script->name());
		ReturnValue result;
		script->executeAll(result);
	}
	gui.update();
}

/*
// Command Index Page
*/

void AtenCommand::repopulateCommandList(const char *search)
{
	ui.CommandList->clear();
	for (int cf = 0; cf < Command::nCommands; ++cf)
	{
		if (search == NULL) ui.CommandList->insertItem(cf, commands.data[cf].keyword);
		else if (strstr(commands.data[cf].keyword, search) != 0) ui.CommandList->insertItem(cf, commands.data[cf].keyword);
	}
}

void AtenCommand::on_CommandSearchEdit_textChanged(QString text)
{
	repopulateCommandList(qPrintable(text));
}

void AtenCommand::on_CommandList_currentRowChanged(int row)
{
	Dnchar text;
 	if (commands.data[row].hasArguments()) text.sprintf("<b>%s(%s)</b><br/>", commands.data[row].keyword, commands.data[row].argText);
	else text.sprintf("<b>%s</b><br/>       %s", commands.data[row].keyword, commands.data[row].syntax);
	text.strcatf("Syntax: %s\n", commands.data[row].syntax);
	ui.CommandEdit->insertHtml(text.get());
}
