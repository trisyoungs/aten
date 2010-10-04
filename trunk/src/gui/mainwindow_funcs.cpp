/*
	*** Qt main window functions
	*** src/gui/mainwindow_funcs.cpp
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
#include "gui/disorder.h"
#include "gui/geometry.h"
#include "gui/grids.h"
#include "gui/glyphs.h"
#include "gui/build.h"
#include "gui/celltransform.h"
#include "gui/celldefine.h"
#include "gui/command.h"
#include "gui/transform.h"
#include "gui/position.h"
#include "gui/atomlist.h"
#include "gui/select.h"
#include "gui/forcefields.h"
#include "gui/fragment.h"
#include "gui/md.h"
#include "gui/minimiser.h"
#include "gui/vibrations.h"
#include "gui/zmatrix.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
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
	customElement_ = 8;
	forcefieldCombo_ = NULL;
	trajectoryToolbarRefreshing_ = FALSE;

	// Public variables
	infoLabel1 = NULL;
	infoLabel2 = NULL;
	messageLabel = NULL;
	progressBar = NULL;
	progressTitle = NULL;
	progressEta = NULL;
	progressButton = NULL;
	progressIndicator = NULL;
	uaGroup = NULL;
	dummyToolButton = NULL;

	ui.setupUi(this);
}

// Destructor
AtenForm::~AtenForm()
{
	// No need (i.e. do not) delete: dummyToolButton.
	if (infoLabel1 != NULL) delete infoLabel1;
	if (infoLabel2 != NULL) delete infoLabel2;
	if (messageLabel != NULL) delete messageLabel;
	if (progressBar != NULL) delete progressBar;
	if (progressTitle != NULL) delete progressTitle;
	if (progressEta != NULL) delete progressEta;
	if (progressButton != NULL) delete progressButton;
	if (progressIndicator != NULL) delete progressIndicator;
// 	if (uaGroup != NULL) delete uaGroup;
// 	for (Refitem<QActionGroup,int> *ri = actionGroups_.first(); ri != NULL; ri = ri->next) delete ri->item;
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
// 		delete gui.minimiserWindow;
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
		// Make sure the trajectory toolbar is visible
		ui.TrajectoryToolbar->setDisabled(FALSE);
		ui.TrajectoryToolbar->setVisible(TRUE);
		// Menu controls (and toolbar)
		updateTrajectoryControls();
		// Update current tab text
		updateModelTabName(-1, m);
	}
	else
	{
		// Make sure the trajectory toolbar is visible
		ui.TrajectoryToolbar->setDisabled(TRUE);
// 		ui.TrajectoryToolbar->setVisible(FALSE);
	}
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
	infoLabel1->setText(s);
	// Second label - cell information
	Cell::CellType ct = m->cell()->type();
	if (ct != Cell::NoCell)
	{
		s = Cell::cellType(ct);
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
	infoLabel2->setText(s);
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

// Rename specified (or current if -1) tab
void AtenForm::updateModelTabName(int tabid, Model *m)
{
	if (tabid < 0) tabid = ui.ModelTabs->currentIndex();
	Dnchar title;
	if (m->nTrajectoryFrames() == 0) title.sprintf("%s", m->name());
	else if (m->renderSourceModel() == m) title.sprintf("%s (Parent of %i frames)", m->name(), m->nTrajectoryFrames());
	else title.sprintf("%s (Frame %i of %i)", m->name(), m->trajectoryFrameIndex()+1, m->nTrajectoryFrames());
	ui.ModelTabs->setTabText(tabid, title.get());
}

// Update trajectory controls
void AtenForm::updateTrajectoryControls()
{
	if (!gui.exists()) return;
	// First see if the model has a trajectory associated to it
	Model *m = aten.currentModel();
	if (m->nTrajectoryFrames() == 0) ui.TrajectoryToolbar->setDisabled(TRUE);
	else
	{
		// If the trajectory is playing, desensitise all but the play/pause button
		if (gui.isTrajectoryPlaying())
		{
			ui.actionTrajectoryViewTrajectory->setDisabled(TRUE);
			ui.actionTrajectoryFirstFrame->setDisabled(TRUE);
			ui.actionTrajectoryPreviousFrame->setDisabled(TRUE);
			ui.actionTrajectoryNextFrame->setDisabled(TRUE);
			ui.actionTrajectoryLastFrame->setDisabled(TRUE);
			ui.actionTrajectoryPlayPause->setDisabled(FALSE);
			setTrajectoryToolbarActive(FALSE);
		}
		else
		{
			ui.actionTrajectoryViewTrajectory->setDisabled(FALSE);
			ui.actionTrajectoryFirstFrame->setDisabled(FALSE);
			ui.actionTrajectoryPreviousFrame->setDisabled(FALSE);
			ui.actionTrajectoryNextFrame->setDisabled(FALSE);
			ui.actionTrajectoryLastFrame->setDisabled(FALSE);
			ui.actionTrajectoryPlayPause->setDisabled(FALSE);
			setTrajectoryToolbarActive(TRUE);
		}
		ui.actionViewTrajectory->setDisabled(FALSE);
		// Select the correct view action
		if (m->renderSource() == Model::ModelSource)
		{
			ui.actionViewModel->setChecked(TRUE);
			ui.actionTrajectoryViewTrajectory->setChecked(FALSE);
		}
		else
		{
			ui.actionViewTrajectory->setChecked(TRUE);
			ui.actionTrajectoryViewTrajectory->setChecked(TRUE);
		}
		// Set slider and spinbox
		updateTrajectoryToolbar();
	}
}

// Update trajectory toolbar controls
void AtenForm::updateTrajectoryToolbar()
{
	trajectoryToolbarRefreshing_ = TRUE;
	trajectorySlider_->setMinimum(1);
	trajectorySlider_->setMaximum(aten.currentModel()->nTrajectoryFrames());
	trajectorySlider_->setValue(aten.currentModel()->trajectoryFrameIndex()+1);
	trajectorySpin_->setRange(1,aten.currentModel()->nTrajectoryFrames());
	trajectorySpin_->setValue(aten.currentModel()->trajectoryFrameIndex()+1);
	trajectoryToolbarRefreshing_ = FALSE;
}

// Set the active status of some controls on the trajectory toolbar
void AtenForm::setTrajectoryToolbarActive(bool active)
{
	trajectorySlider_->setEnabled(active);
	trajectorySpin_->setEnabled(active);
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

// Add model tab
int AtenForm::addModelTab(Model *m)
{
	if (!gui.exists()) return -1;
	// Create new tab in ModelTabs QTabBar
	int tabid = ui.ModelTabs->addTab("Unnamed");
	ui.ModelTabs->setCurrentIndex(tabid);
	updateModelTabName(tabid, m);
	return tabid;
}

// Cancel any current mode and return to select
void AtenForm::cancelCurrentMode()
{
	ui.actionSelectAtoms->trigger();
}

/*
// Model Navigation / Management
*/

void AtenForm::on_ModelTabs_currentChanged(int n)
{
	msg.enter("AtenForm::on_ModelTabs_currentChanged");
	// Different model tab has been selected, so set aten.currentmodel to reflect it.
	aten.setCurrentModel(aten.model(n));
	gui.update(TRUE,TRUE,TRUE);
	msg.exit("AtenForm::on_ModelTabs_currentChanged");
}

void AtenForm::on_ModelTabs_doubleClicked(int tabid)
{
	msg.enter("AtenForm::on_ModelTabs_doubleClicked");
	// Different model tab has been selected, so set aten.currentmodel to reflect it.
	Model *m = aten.model(tabid);
	if (m == NULL) return;
	bool ok;
	QString text = QInputDialog::getText(this, tr("Rename Model: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
	if (ok && !text.isEmpty())
	{
		CommandNode::run(Command::SetName, "c", qPrintable(text));
		updateModelTabName(tabid, m);
		updateWindowTitle();
		gui.disorderWindow->refresh();
	}
	msg.exit("AtenForm::on_ModelTabs_doubleClicked");
}

void AtenForm::refreshModelTabs()
{
	msg.enter("AtenForm::refreshModelTabs");
	// Set names on tabs
	int tabid = 0;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		updateModelTabName(tabid, m);
		tabid ++;
	}
	gui.disorderWindow->refresh();
	msg.exit("AtenForm::refreshModelTabs");
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
			temp.sprintf("&%i %s", n, removePath(qPrintable(actionRecentFile[n]->data().toString())));
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

// Enable/disable manually-created widgets
void AtenForm::setWidgetsEnabled(bool b)
{
	// Must manually enable all widgets added to toolbars by hand. Bug in Qt?
	forcefieldCombo_->setEnabled(b);
	trajectorySlider_->setEnabled(b);
	trajectorySpin_->setEnabled(b);
	bondToleranceSpin_->setEnabled(b);
}

/*
// Window Show / Hide Functions
*/

void AtenForm::on_actionAtomlistWindow_triggered(bool checked)
{
	if (checked)
	{
		gui.atomlistWindow->showWindow();
		gui.atomlistWindow->refresh();
	}
	else gui.atomlistWindow->hide();
}

void AtenForm::on_actionBuildWindow_triggered(bool checked)
{
	if (checked) gui.buildWindow->showWindow();
	else gui.buildWindow->hide();
}

void AtenForm::on_actionTransformWindow_triggered(bool checked)
{
	if (checked) gui.transformWindow->showWindow();
	else gui.transformWindow->hide();
}

void AtenForm::on_actionSelectWindow_triggered(bool checked)
{
	if (checked) gui.selectWindow->showWindow();
	else gui.selectWindow->hide();
}

void AtenForm::on_actionPositionWindow_triggered(bool checked)
{
	if (checked) gui.positionWindow->showWindow();
	else gui.positionWindow->hide();
}

void AtenForm::on_actionCellDefineWindow_triggered(bool checked)
{
	if (checked)
	{
		gui.cellDefineWindow->showWindow();
		gui.cellDefineWindow->refresh();
	}
	else gui.cellDefineWindow->hide();
}

void AtenForm::on_actionCellTransformWindow_triggered(bool checked)
{
	if (checked)
	{
		gui.cellTransformWindow->showWindow();
		gui.cellTransformWindow->refresh();
	}
	else gui.cellTransformWindow->hide();
}

void AtenForm::on_actionCommandWindow_triggered(bool checked)
{
	if (checked)
	{
		gui.commandWindow->showWindow();
		gui.commandWindow->refresh();
	}
	else gui.commandWindow->hide();
}

void AtenForm::on_actionMinimiserWindow_triggered(bool checked)
{
	if (checked) gui.minimiserWindow->showWindow();
	else gui.minimiserWindow->hide();
}

void AtenForm::on_actionDisorderWindow_triggered(bool checked)
{
	if (checked) gui.disorderWindow->showWindow();
	else gui.disorderWindow->hide();
}

void AtenForm::on_actionForcefieldsWindow_triggered(bool checked)
{
	if (checked)
	{
		gui.forcefieldsWindow->showWindow();
		gui.forcefieldsWindow->refresh();
	}
	else gui.forcefieldsWindow->hide();
}

void AtenForm::on_actionFragmentWindow_triggered(bool checked)
{
	if (checked) gui.fragmentWindow->showWindow();
	else gui.fragmentWindow->hide();
}

void AtenForm::on_actionGeometryWindow_triggered(bool checked)
{
	if (checked) gui.geometryWindow->showWindow();
	else gui.geometryWindow->hide();
}

void AtenForm::on_actionGridsWindow_triggered(bool checked)
{
	if (checked) gui.gridsWindow->showWindow();
	else gui.gridsWindow->hide();
}

void AtenForm::on_actionGlyphsWindow_triggered(bool checked)
{
	if (checked) gui.glyphsWindow->showWindow();
	else gui.glyphsWindow->hide();
}

void AtenForm::on_actionMolecularDynamicsWindow_triggered(bool checked)
{
	if (checked) gui.mdWindow->showWindow();
	else gui.mdWindow->hide();
}

// void AtenForm::on_actionAnalyseWindow_triggered(bool checked)
// {
//	if (checked) gui.analyseWindow->showWindow();
//	else gui.analyseWindow->hide();
// }

void AtenForm::on_actionVibrationsWindow_triggered(bool checked)
{
	if (checked) gui.vibrationsWindow->showWindow();
	else gui.vibrationsWindow->hide();
}

void AtenForm::on_actionZMatrixEditorWindow_triggered(bool checked)
{
	if (checked) gui.zmatrixWindow->showWindow();
	else gui.zmatrixWindow->hide();
}

void AtenForm::on_actionAboutAten_triggered(bool checked)
{
	gui.aboutDialog->showWindow();
}

void AtenForm::on_actionAboutQt_triggered(bool checked)
{
	QMessageBox::aboutQt(this, "About Qt");
}
