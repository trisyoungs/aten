/*
	*** Command Dock Widget
	*** src/gui/command_funcs.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/toolbox.h"
#include "gui/command.h"
#include "parser/scopenode.h"
#include "base/sysfunc.h"

// Constructor
CommandWidget::CommandWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
	repopulateCommandList(NULL);
	refreshScripts();
}

void CommandWidget::showWidget()
{
	show();
	refresh();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.CommandButton->setChecked(TRUE);
}

void CommandWidget::refresh()
{
	updateVariableList();
}

// Set list of commands in command tab
void CommandWidget::setCommandList(QStringList cmds)
{
	ui.CommandPrompt->setCommandList(cmds);
}

// Return list of commands stored in command tab
QStringList CommandWidget::commandList()
{
	return ui.CommandPrompt->commandList();
}

/*
// Prompt Tab
*/

void CommandWidget::on_CommandPrompt_returnPressed()
{
	Program tempScript;
	// Grab the current text of the line edit (and clear it at the same time)
	if (tempScript.generateFromString(ui.CommandPrompt->getText()))
	{
		ReturnValue result;
		tempScript.execute(result);
	}
	// Force update of the GUI?
	if (ui.PromptForceUpdateCheck->isChecked()) gui.update(GuiQt::CanvasTarget);
}

/*
// Interactive Tab
*/

// Update variable list
void CommandWidget::updateVariableList()
{
	// Count number of variables
	int count = 0;
	for (Refitem<ScopeNode,int> *sn = interactiveProgram_.mainProgram()->scopeNodes(); sn != NULL; sn = sn->next) count += sn->item->variables.nVariables();
	// Reset table
	ui.VariableTable->clear();
	ui.VariableTable->setRowCount(count);
	ui.VariableTable->setColumnCount(3);
	ui.VariableTable->setHorizontalHeaderLabels(QStringList() << "Variable" << "Type" << "Value");
	QTableWidgetItem *item;
	Variable *var;
	TreeNode *node;
	ReturnValue rv;
	Dnchar s;
	for (Refitem<ScopeNode,int> *sn = interactiveProgram_.mainProgram()->scopeNodes(); sn != NULL; sn = sn->next)
	{
		count = 0;
		for (node = sn->item->variables.variables(); node != NULL; node = node->next)
		{
			var = (Variable*) node;
			item = new QTableWidgetItem(var->name());
			ui.VariableTable->setItem(count, 0, item);
			item = new QTableWidgetItem(VTypes::dataType(var->returnType()));
			ui.VariableTable->setItem(count, 1, item);
			var->execute(rv);
			if (rv.type() < VTypes::AtenData) item = new QTableWidgetItem(rv.asString());
			else
			{
				s.sprintf("%p", rv.asPointer(rv.type()));
				item = new QTableWidgetItem(s.get());
			}
			ui.VariableTable->setItem(count, 2, item);
			++count;
		}
	}
	ui.VariableTable->resizeColumnsToContents();
}

void CommandWidget::on_InteractivePrompt_returnPressed()
{
	// Grab the current text of the line edit (and clear it at the same time)
	interactiveProgram_.mainProgram()->reset(FALSE);
	if (interactiveProgram_.generateFromString(ui.InteractivePrompt->getText()))
	{
		ReturnValue result;
		interactiveProgram_.execute(result);
		updateVariableList();
	}
	// Force update of the GUI?
	if (ui.InteractiveForceUpdateCheck->isChecked()) gui.update(GuiQt::CanvasTarget);
}

/*
// Scripts Tab
*/

void CommandWidget::refreshScripts(bool refreshactions, bool refreshlist)
{
	// Refresh list
	if (refreshlist)
	{
		ui.ScriptsList->clear();
		for (Program *script = aten.scripts(); script != NULL; script = script->next) ui.ScriptsList->addItem(script->filename());
	}
	// Refresh scripts menu
	if (refreshactions)
	{
		for (Refitem<QAction, Program*> *sa = scriptActions_.first(); sa != NULL; sa = sa->next)
		{
			gui.mainWindow()->ui.ScriptsMenu->removeAction(sa->item);
			// Free Reflist QActions
			delete sa->item;
		}
		// Clear Reflist and repopulate, along with Scripts menu actions
		scriptActions_.clear();
		for (Program *prog = aten.scripts(); prog != NULL; prog = prog->next)
		{
			// Create new QAction and add to Reflist
			QAction *qa = new QAction(this);
			// Set action data
			qa->setVisible(TRUE);
			qa->setText(prog->name());
			QObject::connect(qa, SIGNAL(triggered()), this, SLOT(runScript()));
			scriptActions_.add(qa, prog);
			gui.mainWindow()->ui.ScriptsMenu->addAction(qa);
		}
	}
}

void CommandWidget::on_OpenScriptButton_clicked(bool v)
{
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Script", currentDirectory_.path(), gui.mainWindow()->loadScriptFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Create script and model variables within it
		Program *ca = aten.addScript();
		if (ca->generateFromFile(qPrintable(filename)))
		{
			msg.print("Script file '%s' loaded succesfully.\n", qPrintable(filename));
			ui.ScriptsList->addItem(ca->filename());
			refreshScripts(TRUE,FALSE);
		}
		else
		{
			aten.removeScript(ca);
			msg.print("Failed to load script file '%s'.\n", qPrintable(filename));
		}
	}
}

void CommandWidget::on_ReloadAllScriptsButton_clicked(bool checked)
{
	// Cycle over scripts, clearing and reloading
	Program *script = aten.scripts(), *xscript;
	while (script != NULL)
	{
		// Check that the file still exists
		if (!fileExists(script->filename()))
		{
			Tree dialog("Error Finding Script", "option('The script file could not be found.','label'); option('choice','radiogroup','\"Delete the entry for this scriptfile (it will not reappear when Aten restarts)\",\"Do not delete the entry (and skip this script)\"',1,'newline');");
			Dnchar text;
			text.sprintf("Script '%s'", script->filename());
			if (dialog.executeCustomDialog(FALSE, text))
			{
				int choice = dialog.widgetValuei("choice");
				if (choice == 1)
				{
					xscript = script->next;
					aten.removeScript(script);
					script = xscript;
					continue;
				}
			}
			else script = script->next;
		}
		else if (!script->reload())
		{
			Tree dialog("Error Loading Script", "option('The script contained an error (see messagebox for details).','label'); option('choice','radiogroup','\"Retry (changes have just been made to the file)\",\"Delete the entry for this scriptfile (it will not reappear when Aten restarts)\",\"Do not delete the entry and skip this script\"',1,'newline');");
			Dnchar text;
			if (dialog.executeCustomDialog(FALSE, text))
			{
				int choice = dialog.widgetValuei("choice");
				if (choice == 1)
				{
					// Do nothing - the loop will continue with the same script pointer (i.e. Retry loading)
				}
				else if (choice == 2)
				{
					xscript = script->next;
					aten.removeScript(script);
					script = xscript;
					continue;
				}
				else script = script->next;
			}
			else script = script->next;
		}
		else script = script->next;
	}
	// Better refresh the window and lists
	refreshScripts();
}

void CommandWidget::on_ScriptsList_currentRowChanged(int row)
{
	ui.RunSelectedScriptButton->setEnabled(row != -1);
	ui.RemoveSelectedScriptButton->setEnabled(row != -1);
}

void CommandWidget::on_RunSelectedScriptButton_clicked(bool checked)
{
	int row = ui.ScriptsList->currentRow();
	if (row == -1) return;
	Program *script = aten.script(row);
	if (script != NULL)
	{
		// Execute the script
		msg.print("Executing script '%s':\n", script->name());
		ReturnValue result;
		script->execute(result, TRUE);
	}
	gui.update(GuiQt::AllTarget);
}

void CommandWidget::on_RemoveSelectedScriptButton_clicked(bool checked)
{
	int row = ui.ScriptsList->currentRow();
	if (row == -1) return;
	Program *script = aten.script(row);
	if (script != NULL) aten.removeScript(script);
	// Better refresh the window and lists
	refreshScripts();
}

void CommandWidget::runScript()
{
	// First, try to cast the sender into a QAction
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("CommandWidget::runScript - Critical - sender was not a QAction.\n");
		return;
	}
	// Find the relevant Script entry...
	Refitem<QAction, Program*> *ri = scriptActions_.contains(action);
	if (ri == NULL) printf("AtenForm::runScript - Could not find QAction in Reflist.\n");
	else
	{
		// Execute the script
		msg.print("Executing script '%s':\n", ri->data->name());
		ReturnValue result;
		ri->data->execute(result, TRUE);
	}
	gui.update(GuiQt::AllTarget);
}

/*
// Command Index Page
*/

void CommandWidget::repopulateCommandList(const char *search)
{
	ui.CommandList->clear();
	for (int cf = Command::Declarations+1; cf < Command::nCommands; ++cf)
	{
		// Skip over commands beginning with '_' or ones which have no keyword defined (internals)
		if ((commands.data[cf].keyword == NULL) || (commands.data[cf].keyword[0] == '_')) continue;
		if (search == NULL) ui.CommandList->insertItem(cf, commands.data[cf].keyword);
		else if (strstr(commands.data[cf].keyword, search) != 0) ui.CommandList->insertItem(cf, commands.data[cf].keyword);
	}
}

void CommandWidget::on_ClearSearchButton_clicked(bool checked)
{
	ui.CommandSearchEdit->clear();
	repopulateCommandList(NULL);
}

void CommandWidget::on_CommandSearchEdit_textChanged(QString text)
{
	repopulateCommandList(qPrintable(text));
}

void CommandWidget::on_CommandList_currentTextChanged(const QString &text)
{
	ui.CommandEdit->clear();
	if (ui.CommandList->currentRow() == -1) return;
	// Find keyword
	Command::Function cf = commands.command(qPrintable(text));
	if (cf == Command::nCommands) return;
	Dnchar cmdtext;
 	cmdtext.sprintf("<b>%s(%s)</b><br/>%s", commands.data[cf].keyword, commands.data[cf].hasArguments() ? commands.data[cf].argText : "", commands.data[cf].syntax);
	ui.CommandEdit->insertHtml(cmdtext.get());
}

void CommandWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.CommandButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget()->postRedisplay();
	event->accept();
}
