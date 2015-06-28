/*
	*** Command Dock Widget
	*** src/gui/command_funcs.cpp
	Copyright T. Youngs 2007-2015

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

#include <QCloseEvent>
#include <QtWidgets/QFileDialog>
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/command.h"
#include "parser/scopenode.h"
#include "base/sysfunc.h"

// Constructor
CommandWidget::CommandWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	ui.setupUi(this);
	repopulateCommandList(NULL);
	refreshScripts();
}

void CommandWidget::showWidget()
{
	show();
	refresh();
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
 * Prompt Tab
 */

void CommandWidget::on_CommandPrompt_returnPressed()
{
	Program tempScript;
	// Grab the current text of the line edit (and clear it at the same time)
	if (tempScript.generateFromString(ui.CommandPrompt->getText(), "UserCommand", "Command Prompt Input"))
	{
		ReturnValue result;
		tempScript.execute(result);
	}
	// Force update of the GUI?
	if (ui.PromptForceUpdateCheck->isChecked()) parent_.updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Interactive Tab
 */

// Update variable list
void CommandWidget::updateVariableList()
{
	// Count number of variables
	int count = 0;
	for (RefListItem<ScopeNode,int>* sn = interactiveProgram_.mainProgram()->scopeNodes(); sn != NULL; sn = sn->next) count += sn->item->variables.nVariables();
	// Reset table
	ui.VariableTable->clear();
	ui.VariableTable->setRowCount(count);
	ui.VariableTable->setColumnCount(3);
	ui.VariableTable->setHorizontalHeaderLabels(QStringList() << "Variable" << "Type" << "Value");
	QTableWidgetItem *item;
	TreeNode* node;
	Variable* var;
	ReturnValue rv;
	QString s;
	for (RefListItem<ScopeNode,int>* sn = interactiveProgram_.mainProgram()->scopeNodes(); sn != NULL; sn = sn->next)
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
				item = new QTableWidgetItem(s);
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
	interactiveProgram_.mainProgram()->reset(false);
	if (interactiveProgram_.generateFromString(ui.InteractivePrompt->getText(), "InteractivePromptCommand", ""))
	{
		ReturnValue result;
		interactiveProgram_.execute(result);
		updateVariableList();
	}
	// Force update of the GUI?
	if (ui.InteractiveForceUpdateCheck->isChecked()) parent_.updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Scripts Tab
 */

void CommandWidget::refreshScripts(bool refreshactions, bool refreshlist)
{
	// Refresh list
	if (refreshlist)
	{
		ui.ScriptsList->clear();
		for (Program* script = parent_.aten().scripts(); script != NULL; script = script->next) ui.ScriptsList->addItem(script->filename());
	}
	// Refresh scripts menu
	if (refreshactions)
	{
		for (RefListItem<QAction, Program*> *sa = scriptActions_.first(); sa != NULL; sa = sa->next)
		{
// 			gui.mainWindow()->ui.ScriptsMenu->removeAction(sa->item);
			// Free RefList QActions
			delete sa->item;
		}
		// Clear RefList and repopulate, along with Scripts menu actions
		scriptActions_.clear();
		for (Program* prog = parent_.aten().scripts(); prog != NULL; prog = prog->next)
		{
			// Create new QAction and add to RefList
			QAction *qa = new QAction(this);
			// Set action data
			qa->setVisible(true);
			qa->setText(prog->name());
			QObject::connect(qa, SIGNAL(triggered()), this, SLOT(runScript()));
			scriptActions_.add(qa, prog);
		}
	}
}

void CommandWidget::on_OpenScriptButton_clicked(bool v)
{
	static QDir currentDirectory_(parent_.aten().workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Script", currentDirectory_.path(), "All files (*)", &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);

		// Create script and model variables within it
		Program* ca = parent_.aten().addScript();
		if (ca->generateFromFile(filename, filename))
		{
			Messenger::print("Script file '%s' loaded succesfully.", qPrintable(filename));
			ui.ScriptsList->addItem(ca->filename());
			refreshScripts(true, false);
		}
		else
		{
			parent_.aten().removeScript(ca);
			Messenger::print("Failed to load script file '%s'.", qPrintable(filename));
		}
	}
}

void CommandWidget::on_ReloadAllScriptsButton_clicked(bool checked)
{
	// Cycle over scripts, clearing and reloading
	Program* script = parent_.aten().scripts(), *xscript;
	while (script != NULL)
	{
		// Check that the file still exists
		QFileInfo info(script->filename());
		if (!info.exists())
		{
			Tree dialog;
			TreeGuiWidget* group;
			TreeGui& ui = dialog.defaultDialog();
			ui.setProperty(TreeGuiWidgetEvent::TextProperty, "!! Error Finding Script !!");
			ui.addLabel("", "The following script could not be found:",1,1);
			ui.addLabel("", script->filename(),1,2);
			group = ui.addRadioGroup("choice");
			ui.addRadioButton("delete", "Delete the entry for this scriptfile (it will not reappear when Aten restarts)", "choice", 1, 1,3);
			ui.addRadioButton("skip", "Do not delete the entry and skip loading this script", "choice", 0, 1,4);
			if (dialog.defaultDialog().execute())
			{
				int choice = ui.asInteger("choice");
				if (choice == 1)
				{
					xscript = script->next;
					parent_.aten().removeScript(script);
					script = xscript;
					continue;
				}
			}
			else script = script->next;
		}
		else if (!script->reload())
		{
			Tree dialog;
			TreeGuiWidget* group;
			TreeGui& ui = dialog.defaultDialog();
			ui.setProperty(TreeGuiWidgetEvent::TextProperty, "!! Error Loading Script !!");
			ui.addLabel("", "The following script contained an error (see MessageBox for more details):",1,1);
			ui.addLabel("", script->filename(),1,2);
			group = ui.addRadioGroup("choice");
			ui.addRadioButton("retry", "Retry (changes have just been made to the file)", "choice", 1, 1,3);
			ui.addRadioButton("delete", "Delete the entry for this scriptfile (it will not reappear when Aten restarts)", "choice", 0, 1,4);
			ui.addRadioButton("skip", "Do not delete the entry and skip loading this script", "choice", 0, 1,5);
			if (dialog.defaultDialog().execute())
			{
				int choice = ui.asInteger("choice");
				if (choice == 1)
				{
					// Do nothing - the loop will continue with the same script pointer (i.e. Retry loading)
				}
				else if (choice == 2)
				{
					xscript = script->next;
					parent_.aten().removeScript(script);
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
	Program* script = parent_.aten().script(row);
	if (script != NULL)
	{
		// Execute the script
		Messenger::print("Executing script '%s':", qPrintable(script->name()));
		ReturnValue result;
		script->execute(result);
	}
	parent_.updateWidgets(AtenWindow::AllTarget);
}

void CommandWidget::on_RemoveSelectedScriptButton_clicked(bool checked)
{
	int row = ui.ScriptsList->currentRow();
	if (row == -1) return;
	Program* script = parent_.aten().script(row);
	if (script != NULL) parent_.aten().removeScript(script);
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
	RefListItem<QAction, Program*>* ri = scriptActions_.contains(action);
	if (ri == NULL) printf("AtenForm::runScript - Could not find QAction in RefList.\n");
	else
	{
		// Execute the script
		Messenger::print("Executing script '%s':", qPrintable(ri->data->name()));
		ReturnValue result;
		ri->data->execute(result);
	}
	parent_.updateWidgets(AtenWindow::AllTarget);
}

/*
 * Command Index Page
 */

void CommandWidget::repopulateCommandList(const char* search)
{
	ui.CommandList->clear();
	for (int n = Commands::Declarations+1; n < Commands::nCommands; ++n)
	{
		Commands::Function cf = (Commands::Function) n;
		// Skip over commands beginning with '_' or ones which have no keyword defined (internals)
		if ((parent_.aten().commandKeyword(cf) == NULL) || (parent_.aten().commandKeyword(cf)[0] == '_')) continue;
		if (search == NULL) ui.CommandList->insertItem(cf, parent_.aten().commandKeyword(cf));
		else if (strstr(parent_.aten().commandKeyword(cf), search) != 0) ui.CommandList->insertItem(cf, parent_.aten().commandKeyword(cf));
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
	Commands::Function cf = Commands::command(qPrintable(text));
	if (cf == Commands::nCommands) return;
	QString cmdText;
 	cmdText.sprintf("<b>%s(%s)</b><br/>%s", parent_.aten().commandKeyword(cf), parent_.aten().commandHasArguments(cf) ? parent_.aten().commandArgText(cf) : "", parent_.aten().commandSyntax(cf));
	ui.CommandEdit->insertHtml(cmdText);
}

void CommandWidget::closeEvent(QCloseEvent* event)
{
	event->accept();
}
