/*
	*** Qt main window functions
	*** src/gui/mainwindow_funcs.cpp
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

#include "base/master.h"
#include "base/elements.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/disorder.h"
#include "gui/grids.h"
#include "gui/glyphs.h"
#include "gui/build.h"
#include "gui/celltransform.h"
#include "gui/celldefine.h"
#include "gui/transform.h"
#include "gui/position.h"
#include "gui/atomlist.h"
#include "gui/forcefields.h"
#include "gui/minimiser.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "model/model.h"
#include <QtGui/QFileDialog>
#include <QtGui/QKeyEvent>
#include <QtGui/QProgressBar>

#include "base/sysfunc.h"

// Bitmap Image Formats (conform to allowable pixmap formats in Qt)
const char *BIF_filters[BIF_NITEMS] = { "Windows Bitmap (*.bmp)", "Joint Photographic Experts Group (*.jpg)", "Portable Network Graphics (*.png)", "Portable Pixmap (*.ppm)", "X11 Bitmap (*.xbm)", "X11 Pixmap (*.xpm)" };
const char *BIF_extensions[BIF_NITEMS] = { "bmp", "jpg", "png", "ppm", "xbm", "xpm" };
bitmap_format BIF_from_text(const char *s)
{
	return (bitmap_format) enumSearch("bitmap format",BIF_NITEMS,BIF_extensions,s);
}
const char *filter_from_BIF(bitmap_format bif)
{
	return BIF_filters[bif];
}
const char *extension_from_BIF(bitmap_format bif)
{
	return BIF_extensions[bif];
}

// Constructor
AtenForm::AtenForm(QMainWindow *parent) : QMainWindow(parent)
{
	ui.setupUi(this);

	// Private variables
	customElement_ = 8;
}

// Catch window close event
void AtenForm::closeEvent(QCloseEvent *event)
{
	if (gui.saveBeforeClose())
	{
		saveSettings();
		event->accept();
		// Delete subwindows / dialogs
		delete gui.prefsDialog;
		delete gui.forcefieldEditorDialog;
		delete gui.loadModelDialog;
		delete gui.selectPatternDialog;
		delete gui.atomlistDialog;
		delete gui.buildDialog;
		delete gui.cellDefineDialog;
		delete gui.cellTransformDialog;
		delete gui.disorderDialog;
		delete gui.forcefieldsDialog;
		delete gui.glyphsDialog;
		delete gui.gridsDialog;
		delete gui.minimiserDialog;
		delete gui.positionDialog;
		delete gui.transformDialog;
	}
	else event->ignore();
}

/*
// Input
*/

void AtenForm::keyPressEvent(QKeyEvent *event)
{
	Canvas::KeyCode kc = gui.convertToKeyCode(event->key());
	if (kc != Canvas::OtherKey) gui.mainView.informKeyDown(kc);
	else event->ignore();
}

void AtenForm::keyReleaseEvent(QKeyEvent *event)
{
	Canvas::KeyCode kc = gui.convertToKeyCode(event->key());
	if (kc != Canvas::OtherKey) gui.mainView.informKeyUp(kc);
	else event->ignore();
}

/*
// Model Navigation / Management
*/

void AtenForm::on_ModelTabs_currentChanged(int n)
{
	dbgBegin(Debug::Calls,"AtenForm::on_ModelTabs_currentChanged");
	// Different model tab has been selected, so set master.currentmodel to reflect it.
	master.setCurrentModel(master.model(n));
	gui.disorderDialog->refresh();
	gui.modelChanged();
	dbgEnd(Debug::Calls,"AtenForm::on_ModelTabs_currentChanged");
}

void AtenForm::on_ModelTabs_doubleClicked(int tabid)
{
	dbgBegin(Debug::Calls,"AtenForm::on_ModelTabs_doubleClicked");
	// Different model tab has been selected, so set master.currentmodel to reflect it.
	Model *m = master.model(tabid);
	if (m == NULL) return;
	bool ok;
	QString text = QInputDialog::getText(this, tr("Rename Model: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
	if (ok && !text.isEmpty())
	{
		m->setName(qPrintable(text));
		ui.ModelTabs->setTabText(tabid, text);
	}
	dbgEnd(Debug::Calls,"AtenForm::on_ModelTabs_doubleClicked");
}

void AtenForm::refreshModelTabs()
{
	dbgBegin(Debug::Calls,"AtenForm::refreshModelTabs");
	// Set names on tabs
	int count = 0;
	for (Model *m = master.models(); m != NULL; m = m->next)
	{
		ui.ModelTabs->setTabText(count, m->name());
		count ++;
	}
	dbgEnd(Debug::Calls,"AtenForm::refreshModelTabs");
}

void AtenForm::executeCommand()
{
	// Clear old script commands
	master.tempScript.clear();
	// Grab the current text of the line edit
	parser.getArgsDelim(qPrintable(commandEdit_->text()), Parser::UseQuotes);
	// Check for no commands given
	if (parser.nArgs() == 0) return;
	if (master.tempScript.cacheCommand()) master.tempScript.execute(NULL);
	commandEdit_->setText("");
	gui.modelChanged();
}

// Cancel progress indicator
void AtenForm::progressCancel()
{
	gui.notifyProgressCanceled();
}

// Load recent file
void AtenForm::loadRecent()
{
	Dnchar filename;
	Model *m;
	Filter *f;
	// Cast sending QAction and grab filename
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenForm::loadRecent - Sender was not a QAction.\n");
		return;
	}
	// Grab the filename from the action
	filename = qPrintable(action->data().toString());
	// See if any loaded model filename matches this filename
	for (m = master.models(); m != NULL; m = m->next)
	{
		msg(Debug::Verbose,"Checking loaded models for '%s': %s\n",filename.get(),m->filename());
		if (filename == m->filename())
		{
			msg(Debug::Verbose,"Matched filename to loaded model.\n");
			master.setCurrentModel(m);
			return;
		}
	}
	// If we get to here then the model is not currently loaded...
	f = master.probeFile(filename.get(), Filter::ModelImport);
	if (f != NULL)
	{
		f->execute(filename.get());
		master.currentModel()->logChange(Change::VisualLog);
		gui.mainView.postRedisplay();
	}
	else
	{
		// Remove file from recent files list
		int last, n;
		for (last=0; last<MAXRECENTFILES; last++) if (!actionRecentFile[last]->isVisible()) break;
		for (n=last+1; n<MAXRECENTFILES; n++)
			if (actionRecentFile[last]->isVisible())
			{
				actionRecentFile[n-1]->setText(actionRecentFile[n]->text());
				actionRecentFile[n-1]->setData(actionRecentFile[n]->data());
			}
	}
}

// Add file to top of recent list
void AtenForm::addRecent(const char *filename)
{
	// Find unused (i.e. still hidden) recent file action
	int last, n;
	char temp[512];
	for (last=0; last<MAXRECENTFILES; last++) if (!actionRecentFile[last]->isVisible()) break;
	// 'last' now holds the first empty slot in the recent files list.
	// If 'last' == MAXRECENTFILES then shuffle top 'n-1' down a position and add at '0'.
	if (last == MAXRECENTFILES)
	{
		// Push the top items down the list
		for (n=MAXRECENTFILES-2; n>=0; n--)
		{
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
			sprintf(temp,"&%i %s", n, removePath(qPrintable(actionRecentFile[n]->data().toString())));
			actionRecentFile[n+1]->setText(temp);
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
		}
		last = 0;
	}
	// Set the new data
	sprintf(temp,"&%i %s (%s)",last,removePath(filename),filename);
	actionRecentFile[last]->setText(temp);
	actionRecentFile[last]->setData(filename);
	actionRecentFile[last]->setVisible(TRUE);
}

// Update undo/redo actions in Edit menu
void AtenForm::updateUndoRedo()
{
	static char text[128];
	Model *m = master.currentModel();
	// Check the model's state pointers
	if (m->currentUndostate() == NULL)
	{
		ui.actionEditUndo->setText("Undo");
		ui.actionEditUndo->setEnabled(FALSE);
	}
	else
	{
		strcpy(text,"Undo (");
		strcat(text,m->currentUndostate()->description());
		strcat(text,")");
		ui.actionEditUndo->setText(text);
		ui.actionEditUndo->setEnabled(TRUE);
	}
	if (m->currentRedoState() == NULL)
	{
		ui.actionEditRedo->setText("Redo");
		ui.actionEditRedo->setEnabled(FALSE);
	}
	else
	{
		strcpy(text,"Redo (");
		strcat(text,m->currentRedoState()->description());
		strcat(text,")");
		ui.actionEditRedo->setText(text);
		ui.actionEditRedo->setEnabled(TRUE);
	}
}

void AtenForm::refreshScriptsMenu()
{
	// Remove old actions from menu (i.e. current items in Reflist)
	for (Refitem<QAction, CommandList*> *sa = scriptActions_.first(); sa != NULL; sa = sa->next)
	{
		ui.ScriptsMenu->removeAction(sa->item);
		// Free Reflist QActions
		delete sa->item;
	}
	// Clear Reflist and repopulate, along with scriptsmenu
	scriptActions_.clear();
	for (CommandList *cl = master.scripts.first(); cl != NULL; cl = cl->next)
	{
		// Create new QAction and add to Reflist
		QAction *qa = new QAction(this);
		// Set action data
		qa->setVisible(TRUE);
		qa->setText(cl->name());
		QObject::connect(qa, SIGNAL(triggered()), this, SLOT(runScript()));
		scriptActions_.add(qa, cl);
		ui.ScriptsMenu->addAction(qa);
	}
}

void AtenForm::on_actionLoadScript_triggered(bool v)
{
	QString filename;
	if (loadScriptDialog->exec() == 1)
	{
		// Get selected filter in file dialog
		filename = loadScriptDialog->selectedFiles().first();
		CommandList *ca = master.scripts.add();
		if (ca->load(qPrintable(filename))) refreshScriptsMenu();
		else master.scripts.remove(ca);
	}
}

void AtenForm::runScript()
{
	// Cast sending QAction and grab filename
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenForm::runScript - Sender was not a QAction.\n");
		return;
	}
	// Find the CommandList from the loadedscripts() Reflist
	Refitem<QAction, CommandList*> *ri = scriptActions_.search(action);
	if (ri == NULL) printf("AtenForm::runScript - Could not find QAction in Reflist.\n");
	else
	{
		// Execute the script
		msg(Debug::None,"Executing script '%s':\n",ri->data->name());
		ri->data->execute();
	}
}

/*
// Mouse Toolbar
*/

void AtenForm::on_actionMouseInteract_triggered(bool checked)
{
	prefs.setMouseAction(Prefs::LeftButton, Prefs::InteractAction);
}

void AtenForm::on_actionMouseRotate_triggered(bool checked)
{
	prefs.setMouseAction(Prefs::LeftButton, Prefs::RotateAction);
}

void AtenForm::on_actionMouseTranslate_triggered(bool checked)
{
	prefs.setMouseAction(Prefs::LeftButton, Prefs::TranslateAction);
}

/*
// Select Toolbar
*/

void AtenForm::on_actionSelectAtoms_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(Canvas::SelectAction);
}

void AtenForm::on_actionSelectMolecules_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(Canvas::SelectMoleculeAction);
}

void AtenForm::on_actionSelectElement_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(Canvas::SelectElementAction);
}

/*
// Window Show / Hide Functions
*/

void AtenForm::on_actionAtomlistDialog_triggered(bool checked)
{
	if (checked)
	{
		gui.atomlistDialog->showWindow();
		gui.atomlistDialog->refresh();
	}
	else gui.atomlistDialog->hide();
}

void AtenForm::on_actionBuildDialog_triggered(bool checked)
{
	if (checked) gui.buildDialog->showWindow();
	else gui.buildDialog->hide();
}

void AtenForm::on_actionTransformDialog_triggered(bool checked)
{
	if (checked) gui.transformDialog->showWindow();
	else gui.transformDialog->hide();
}

void AtenForm::on_actionPositionDialog_triggered(bool checked)
{
	if (checked) gui.positionDialog->showWindow();
	else gui.positionDialog->hide();
}

void AtenForm::on_actionCellDefineDialog_triggered(bool checked)
{
	if (checked)
	{
		gui.cellDefineDialog->showWindow();
		gui.cellDefineDialog->refresh();
	}
	else gui.cellDefineDialog->hide();
}

void AtenForm::on_actionCellTransformDialog_triggered(bool checked)
{
	if (checked)
	{
		gui.cellTransformDialog->showWindow();
		gui.cellTransformDialog->refresh();
	}
	else gui.cellTransformDialog->hide();
}

void AtenForm::on_actionMinimiserDialog_triggered(bool checked)
{
	if (checked) gui.minimiserDialog->showWindow();
	else gui.minimiserDialog->hide();
}

void AtenForm::on_actionDisorderDialog_triggered(bool checked)
{
	if (checked) gui.disorderDialog->showWindow();
	else gui.disorderDialog->hide();
}

void AtenForm::on_actionForcefieldsDialog_triggered(bool checked)
{
	if (checked)
	{
		gui.forcefieldsDialog->showWindow();
		gui.forcefieldsDialog->refresh();
	}
	else gui.forcefieldsDialog->hide();
}

void AtenForm::on_actionGridsDialog_triggered(bool checked)
{
	if (checked) gui.gridsDialog->showWindow();
	else gui.gridsDialog->hide();
}

// void AtenForm::on_actionGlyphsWindow_triggered(bool checked)
// {
// 	if (checked) gui.glyphsWindow->showWindow();
// 	else gui.glyphsWindow->hide();
// }

// void AtenForm::on_actionAnalyseWindow_triggered(bool checked)
// {
//	if (checked) gui.analyseWindow->showWindow();
//	else gui.analyseWindow->hide();
// }
