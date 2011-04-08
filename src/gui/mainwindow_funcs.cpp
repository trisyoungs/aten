/*
	*** Qt main window functions
	*** src/gui/mainwindow_funcs.cpp
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
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/trajectory.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "gui/about.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "model/undostate.h"
#include "parser/commandnode.h"
#include <QtGui/QFileDialog>
#include <QtGui/QKeyEvent>
#include <QtGui/QProgressBar>
#include "base/sysfunc.h"
#include "main/version.h"
#include <iostream>
#include <fstream>

// Constructor
AtenForm::AtenForm(QMainWindow *parent) : QMainWindow(parent)
{
	// Private variables
	saveModelFilter = NULL;

	// Public variables
	infoLabel1_ = NULL;
	infoLabel2_ = NULL;
	messageLabel_ = NULL;

	ui.setupUi(this);
}

// Destructor
AtenForm::~AtenForm()
{
	// No need (i.e. do not) delete: dummyToolButton.
	if (infoLabel1_ != NULL) delete infoLabel1_;
	if (infoLabel2_ != NULL) delete infoLabel2_;
	if (messageLabel_ != NULL) delete messageLabel_;
}

// Catch window close event
void AtenForm::closeEvent(QCloseEvent *event)
{
	if (gui.saveBeforeClose())
	{
		saveSettings();
		event->accept();
		// Delete subwindows / dialogs
// 		delete gui.prefsDialog;
// 		delete gui.forcefieldEditorDialog;
// // 		delete gui.loadModelDialog;
// 		delete gui.selectPatternDialog;
// 		delete gui.atomlistWindow;
// 		delete gui.buildWindow;
// 		delete gui.cellDefineWindow;
// 		delete gui.cellTransformWindow;
// 		delete gui.disorderWindow;
// 		delete gui.forcefieldsWindow;
// 		delete gui.glyphsWindow;
// 		delete gui.gridsWindow;
// 		delete gui.mdWindow;
// 		delete gui.positionWindow;
// 		delete gui.selectWindow;
// 		delete gui.transformWindow;
		gui.app->exit(0);
	}
	else event->ignore();
}

/*
// Refresh Functions
*/

// Update GUI after model change (or different model selected)
void AtenForm::update()
{
	// Update status bar
	QString s;
	Model *m = aten.currentModel();
	// First label - atom and trajectory frame information
	if (m->hasTrajectory())
	{
		if (m->renderSourceModel() == m)
		{
			s = "(Parent of ";
			s += itoa(m->nTrajectoryFrames());
			s += " frames) ";
		}
		else
		{
			s = "(Frame ";
			s += itoa(m->trajectoryFrameIndex()+1);
			s += " of ";
			s += itoa(m->nTrajectoryFrames());
			s += ") ";
		}
		gui.trajectoryWidget->refresh();
	}
	updateTrajectoryMenu();

	m = m->renderSourceModel();
	s += itoa(m->nAtoms());
	s += " Atoms ";
	// Add on unknown atom information
	if (m->nUnknownAtoms() != 0)
	{
		s += " (<b>";
		s += itoa(m->nUnknownAtoms());
		s += " unknown</b>) ";
	}
	if (m->nSelected() != 0)
	{
		s += "(<b>";
		s += itoa(m->nSelected());
		s += " selected</b>) ";
	}
	s += ftoa(m->mass());
	s += " g mol<sup>-1</sup> ";
	infoLabel1_->setText(s);
	// Second label - cell information
	UnitCell::CellType ct = m->cell()->type();
	if (ct != UnitCell::NoCell)
	{
		s = UnitCell::cellType(ct);
		s += ", ";
		s += ftoa(m->density());
		switch (prefs.densityUnit())
		{
			case (Prefs::GramsPerCm):
				s += " g cm<sup>-3</sup>";
				break;
			case (Prefs::AtomsPerAngstrom):
				s += " atoms &#8491;<sup>-3</sup>";
				break;
			default:
				break;
		}
	}
	else s = "Non-periodic";
	infoLabel2_->setText(s);
	// Update save button status
	ui.actionFileSave->setEnabled( m->changeLog.isModified() );
	// Enable the Atom menu if one or more atoms are selected
	ui.AtomContextMenu->setEnabled( m->renderSourceModel()->nSelected() == 0 ? FALSE : TRUE);
	// Update Undo Redo lists
	updateUndoRedo();
	// Enable/Disable cut/copy/paste/delete based on selection status and clipboard contents
	ui.actionEditPaste->setEnabled( aten.userClipboard->nAtoms() != 0);
	ui.actionEditPasteTranslated->setEnabled( aten.userClipboard->nAtoms() != 0);
	ui.actionEditCopy->setEnabled( m->nSelected() != 0 );
	ui.actionEditCut->setEnabled( m->nSelected() != 0 );
	ui.actionEditDelete->setEnabled( m->nSelected() != 0 );
	// Update main window title
	updateWindowTitle();
}

// Update trajectory menu
void AtenForm::updateTrajectoryMenu()
{
	if (!gui.exists()) return;
	// First see if the model has a trajectory associated to it
	Model *m = aten.currentModel();
	Model::RenderSource rs = m->renderSource();
	bool hastrj = (m->nTrajectoryFrames() != 0);
	int framenatoms = hastrj ? m->trajectoryCurrentFrame()->nAtoms() : -1;
	ui.actionTrajectoryRemove->setEnabled(hastrj);
	ui.actionTrajectoryInheritParentStyle->setChecked(m->trajectoryPropagateParentStyle());
	ui.actionTrajectoryInheritParentStyle->setEnabled(m->nAtoms() == framenatoms);
	ui.actionTrajectoryCopyStyleToParent->setEnabled((rs == Model::TrajectorySource) && (m->nAtoms() == framenatoms));
	ui.actionTrajectoryPropagateStyleFromHere->setEnabled((rs == Model::TrajectorySource) && m->trajectoryIsCached());
	ui.actionTrajectoryFirstFrame->setEnabled(hastrj);
	ui.actionTrajectoryLastFrame->setEnabled(hastrj);
	ui.actionTrajectoryPlayPause->setEnabled(hastrj);
	ui.actionTrajectoryPlayPause->setChecked( gui.trajectoryWidget->ui.TrajectoryPlayPauseButton->isChecked());
	ui.actionTrajectoryFrames->setEnabled(hastrj);
	ui.actionTrajectorySaveMovie->setEnabled(hastrj);
	// Select the correct view action
	ui.actionTrajectoryModel->setChecked(rs == Model::ModelSource);
	ui.actionTrajectoryFrames->setChecked(rs == Model::TrajectorySource);
}

// Refresh window title
void AtenForm::updateWindowTitle()
{
	if (!gui.exists()) return;
	Model *m = aten.currentModel();
	Dnchar title;
	title.sprintf("Aten (v%s r%s) - %s (%s)%s", ATENVERSION, ATENREVISION, m->name(), m->filename()[0] == '\0' ? "<<no filename>>" : m->filename(), m->changeLog.isModified() ? " [Modified]" : "");
	setWindowTitle(title.get());
}

// Cancel any current mode and return to select
void AtenForm::cancelCurrentMode()
{
	ui.actionSelectAtoms->trigger();
}

// Load recent file
void AtenForm::loadRecent()
{
	Dnchar filename;
	Model *m;
	Tree *filter;
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
	for (m = aten.models(); m != NULL; m = m->next)
	{
		msg.print(Messenger::Verbose,"Checking loaded models for '%s': %s\n",filename.get(),m->filename());
		if (filename == m->filename())
		{
			msg.print(Messenger::Verbose,"Matched filename to loaded model.\n");
			aten.setCurrentModel(m);
			// Update GUI
			gui.update(GuiQt::AllTarget);
			return;
		}
	}
	// If we get to here then the model is not currently loaded...
	filter = aten.probeFile(filename.get(), FilterData::ModelImport);
	if (filter != NULL)
	{
		ReturnValue rv;
		filter->executeRead(filename.get(), rv);
		aten.currentModel()->changeLog.add(Log::Visual);
		gui.mainWidget->postRedisplay();
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
	Dnchar temp;
	for (last=0; last<MAXRECENTFILES; last++) if (!actionRecentFile[last]->isVisible()) break;
	// 'last' now holds the first empty slot in the recent files list.
	// If 'last' == MAXRECENTFILES then shuffle top 'n-1' down a position and add at '0'.
	if (last == MAXRECENTFILES)
	{
		// Push the top items down the list
		for (n=MAXRECENTFILES-2; n>=0; n--)
		{
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
			temp.sprintf("&%i %s", n+1, removePath(qPrintable(actionRecentFile[n]->data().toString())));
			actionRecentFile[n+1]->setText(temp.get());
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
		}
		last = 0;
	}
	// Set the new data
	temp.sprintf("&%i %s (%s)",last,removePath(filename),filename);
	actionRecentFile[last]->setText(temp.get());
	actionRecentFile[last]->setData(filename);
	actionRecentFile[last]->setVisible(TRUE);
}

void AtenForm::on_actionAboutAten_triggered(bool checked)
{
	gui.aboutDialog->showWindow();
}

void AtenForm::on_actionAboutQt_triggered(bool checked)
{
	QMessageBox::aboutQt(this, "About Qt");
}

// Update undo/redo actions in Edit menu
void AtenForm::updateUndoRedo()
{
	Dnchar text;
	Model *m = aten.currentModelOrFrame();
	// Check the model's state pointers
	if (m->currentUndoState() == NULL)
	{
		ui.actionEditUndo->setText("Undo");
		ui.actionEditUndo->setEnabled(FALSE);
	}
	else
	{
		text.sprintf("Undo (%s)", m->currentUndoState()->description());
		ui.actionEditUndo->setText(text.get());
		ui.actionEditUndo->setEnabled(TRUE);
	}
	if (m->currentRedoState() == NULL)
	{
		ui.actionEditRedo->setText("Redo");
		ui.actionEditRedo->setEnabled(FALSE);
	}
	else
	{
		text.sprintf("Redo (%s)", m->currentRedoState()->description());
		ui.actionEditRedo->setText(text.get());
		ui.actionEditRedo->setEnabled(TRUE);
	}
}

// Change current user action
void AtenForm::uaButtonClicked(int id)
{
	QAbstractButton *button;
	// Check button correspondiong to supplied index
	button = uaButtons_.button(id);
	if (button == NULL) printf("Internal Error: AtenForm::uaButtonClicked - No button associated to id %i\n", id);
	else if (button->isChecked()) gui.mainWidget->setSelectedMode((UserAction::Action) id);
}

// Set action/button to reflect supplied user action
void AtenForm::setActiveUserAction(UserAction::Action ua)
{
	// Set (check) relevant action or button based on supplied UserAction
	QAbstractButton *button;
	switch (ua)
	{
		// No active mode
		case (UserAction::NoAction):
			uaDummyButton_->setChecked(TRUE);
			ui.actionNoAction->setChecked(TRUE);
			break;
		// Three select QActions on main ToolBar
		case (UserAction::SelectAction):
		case (UserAction::SelectMoleculeAction):
		case (UserAction::SelectElementAction):
			uaDummyButton_->setChecked(TRUE);
			break;
		// All other actions are related to buttons elsewhere in the GUI
		default:
			ui.actionNoAction->setChecked(TRUE);
			button = uaButtons_.button(ua);
			if (button == NULL) printf("No button associated to user action %i.\n");
			else button->setChecked(TRUE);
			break;
	}
}

// Set message label text
void AtenForm::setMessageLabel(const char *s)
{
	messageLabel_->setText(s);
}
