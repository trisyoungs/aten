/*
	*** Qt user interface functions
	*** src/gui/gui.cpp
	Copyright T. Youngs 2007

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
#include "gui/canvas.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include <QtGui/QMessageBox>
#include <QtCore/QTextStream>
#include <QtGui/QProgressBar>

#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

// External Declarations
gui_qt gui;

// Constructor
gui_qt::gui_qt()
{
	#ifdef MEMDEBUG
		printf("Constructor : gui_qt\n");
	#endif
	does_exist = FALSE;
	is_available = FALSE;
	NORENDER = FALSE;
	trajectory_playing = FALSE;
	trajectory_timerid = -1;
}

// Destructor
gui_qt::~gui_qt()
{
	#ifdef MEMDEBUG
		printf("Destructor : gui_qt\n");
	#endif
}

// Early initial functions
void gui_qt::prepare()
{
}

// Initialise and create GUI
void gui_qt::run(int argc, char **argv)
{
	dbg_begin(DM_CALLS,"gui_qt::run");

	// Initialise GLUT, Qt, and the icons resource
	glutInit(&argc, argv);
        app = new QApplication(argc, argv);
	Q_INIT_RESOURCE(icons);

	// Create the GUI windows
        mainwindow = new AtenForm;
	prefsdialog = new AtenPrefs;

	// Set the main gui widgetcanvas to be associated to the GUIs TCanvas (and vice versa)
	gui.mainview.set_widget(mainwindow->ui.ModelView);	
	mainwindow->ui.ModelView->set_canvas(&gui.mainview);

	// Set up misc things for Qt (QActionGroups etc.) that we couldn't do in Designer
	mainwindow->finalise_ui();
	prefsdialog->finalise_ui();

	// Set controls in the windows
	mainwindow->set_controls();
	prefsdialog->set_controls();

	// Show the widgets in the GUI and flag it as existing
	mainwindow->show();
	does_exist = TRUE;

	// Prepare first model in list
	master.set_currentmodel(master.get_models());
	master.get_currentmodel()->calculate_viewmatrix();
	master.get_currentmodel()->project_all();

	// Add loaded models to tabbar (and reset its view while we're here)
	int tabid;
	for (model *m = master.get_models(); m != NULL; m = m->next)
	{
		tabid = mainwindow->ui.ModelTabs->addTab(m->get_name());
		m->reset_view();
	}

	// Refresh the surfaces and forcefield lists
	mainwindow->refresh_gridspage();
	mainwindow->refresh_forcefieldpage();

	int n = app->exec();
	dbg_end(DM_CALLS,"gui_qt::run");
}

// Update trajectory controls
void gui_qt::update_trajcontrols()
{
	// First see if the model has a trajectory associated to it
	if (master.get_currentmodel()->get_totalframes() == 0)
	{
		mainwindow->ui.actionFrameFirst->setDisabled(TRUE);
		mainwindow->ui.actionFramePrevious->setDisabled(TRUE);
		mainwindow->ui.actionFrameNext->setDisabled(TRUE);
		mainwindow->ui.actionFrameLast->setDisabled(TRUE);
		mainwindow->ui.actionPlayPause->setDisabled(TRUE);
	}
	else
	{
		// If the trajectory is playing, desensitise all but the play/pause button
		if (trajectory_playing)
		{
			mainwindow->ui.actionFrameFirst->setDisabled(TRUE);
			mainwindow->ui.actionFramePrevious->setDisabled(TRUE);
			mainwindow->ui.actionFrameNext->setDisabled(TRUE);
			mainwindow->ui.actionFrameLast->setDisabled(TRUE);
			mainwindow->ui.actionPlayPause->setDisabled(FALSE);
		}
		else
		{
			mainwindow->ui.actionFrameFirst->setDisabled(FALSE);
			mainwindow->ui.actionFramePrevious->setDisabled(FALSE);
			mainwindow->ui.actionFrameNext->setDisabled(FALSE);
			mainwindow->ui.actionFrameLast->setDisabled(FALSE);
			mainwindow->ui.actionPlayPause->setDisabled(FALSE);
		}
	}
}

// Update labels
void gui_qt::update_labels()
{
	// Update the information labels in the button bar
	if (!does_exist) return;
	dbg_begin(DM_CALLS,"gui_qt::update_labels");
	QString s;
	model *m = master.get_currentmodel();
	// Trajectory information label
	if (m->get_totalframes() != 0)
	{
		s = "(Frame ";
		s += itoa(m->get_frameposition());
		s += " of ";
		s += itoa(m->get_totalframes());
		s += ") ";
	}
	// Model information
	s += itoa(m->get_natoms());
	s += " Atoms ";
	if (m->get_nselected() != 0)
	{
		s += "(<b>";
		s += itoa(m->get_nselected());
		s += " selected</b>) ";
	}
	s += ftoa(m->get_mass());
	s += " g mol<sup>-1</sup> ";
	cell_type ct = m->get_celltype();
	if (ct != CT_NONE)
	{
		s += "(";
		s += text_from_CT(ct);
		s += ", ";
		s += ftoa(m->get_density());
		switch (prefs.get_density_units())
		{
			case (DU_GPERCM):	s += " g cm<sup>-3</sup>)"; break;
			case (DU_ATOMSPERANG):	s += " atoms &#8491;<sup>-3</sup>)"; break;
		}
	}
	mainwindow->statuslabel->setText(s);
	dbg_end(DM_CALLS,"gui_qt::update_labels");
}

/*
// General GUI Routines
*/

void gui_qt::process_events()
{
	// During intensive computations, this should be called periodically so that the interface remains responsive, and the process can be interrupted, modelview can be manipulated etc.
}

// Add model to GUI list, in this case a tab in the ModelTabs widget
void gui_qt::add_model(model *m)
{
	if (!does_exist) return;
	// Create new tab in ModelTabs QTabBar
	int tabid = mainwindow->ui.ModelTabs->addTab(m->get_name());
	m->reset_view();
}

// Remove model from list
void gui_qt::remove_model(model *m)
{
	if (!does_exist) return;
	mainwindow->ui.ModelTabs->removeTab(master.get_modelindex(m));
}

// Select model in ModelTabs
void gui_qt::select_model(model *m)
{
	if (!does_exist) return;
	// Determine index of selected model.
	int id = master.get_currentmodelindex();
	// Select corresponding tab
	mainwindow->ui.ModelTabs->setCurrentIndex(id);
	// Update main window
	gui.refresh();
}

// Add ff to list
void gui_qt::add_ff(forcefield *ff)
{
	if (!does_exist) return;
}

// Remove ff from list
void gui_qt::remove_ff(forcefield *ff)
{
	// Find iter that matches the old model in the model master
	if (!does_exist) return;
}

// Select ff in list
void gui_qt::select_ff(forcefield *ff)
{
	if (!does_exist) return;
}

// Add grid to list
void gui_qt::add_grid(grid *g)
{
	if (is_available) printf("gui_qt::add_grid - Not defined.\n");
}

// Remove grid from list
void gui_qt::remove_grid(grid *g)
{
	if (is_available) printf("gui_qt::remove_grid - Not defined.\n");
}

// Select grid in list and show in main/sub windows
void gui_qt::select_grid(grid *g)
{
	if (is_available) printf("gui_qt::select_grid - Not defined.\n");
}

// Redraw main window canvas
void gui_qt::refresh()
{
	if (!does_exist) return;
	// Update labels on status bar
	update_labels();
	// Update contents of the atom list
	mainwindow->refresh_atompage();
	// Update the contents of the cell page
	mainwindow->refresh_cellpage();
	// Update the disorder page
	mainwindow->refresh_disorderpage();
	// Update pattern list in forcefield window
	mainwindow->refresh_forcefieldpatterns();
	// Update trajectory playback controls
	update_trajcontrols();
	// Update Undo/Redo menuitems
	mainwindow->update_undoredo();
	// Request redraw of the main canvas
	mainview.postredisplay();
	// If the model save_point is recent, disable save button
	mainwindow->ui.actionFileSave->setEnabled(master.get_currentmodel()->is_modified());
	// Enable the Atom menu if one or more atoms are selected
	mainwindow->ui.AtomMenu->setEnabled( master.get_currentmodel()->get_nselected() == 0 ? FALSE : TRUE);
	// Enable View->Trajectory menu item if a trajectory is associated
	mainwindow->ui.actionViewTrajectory->setEnabled( master.get_currentmodel()->get_currentframe() == NULL ? FALSE : TRUE);
}

int gui_qt::user_question(const char *s, const char *t)
{
	return -1;
}

void gui_qt::print_message(const char *s)
{
	static char str[8096];
	static int n, len;
	if (!does_exist) return;
	// Remove the '\n' from the end of s (if it has one)
	for (n=0; s[n] != '\0'; n++) str[n] = (s[n] == '\n' ? ' ' : s[n]);
	str[n] = '\0';
	mainwindow->ui.TextDisplay->append(str);
}

bool gui_qt::save_before_close()
{
	// Check the status of all models, asking to save before close if necessary
	char text[512];
	int returnvalue;
	filter *f;
	for (model *m = master.get_models(); m != NULL; m = m->next)
	{
		if (m->is_modified())
		{
			// Create a model message dialog
			sprintf(text, "Model '%s' has been modified.\n", m->get_name());
			returnvalue = QMessageBox::warning(mainwindow, "Aten", text, QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
			switch (returnvalue)
			{
				// Discard changes
				case (QMessageBox::Discard):
					break;
				// Cancel quit and return to app
				case (QMessageBox::Cancel):
					return FALSE;
				// Save model before quit
				case (QMessageBox::Save):
					// If model has a filter set, just save it
					f = m->get_filter();
					if (f != NULL) f->execute(m->get_filename());
					else if (mainwindow->run_savemodel_dialog())
					{
						m->set_filter(mainwindow->savemodelfilter);
						m->set_filename(mainwindow->savemodelfilename.get());
						mainwindow->savemodelfilter->execute(m->get_filename());
					}
					else return FALSE;
					break;
			}
		}
	}
	return TRUE;
}

// Convert Qt keysym to key_code
key_code gui_qt::convert_to_KC(int sym)
{
	key_code result = KC_OTHER;
	switch (sym)
	{
		case (Qt::Key_Left):
			result = KC_LEFT;
			break;
		case (Qt::Key_Right):
			result = KC_RIGHT;
			break;
		case (Qt::Key_Up):
			result = KC_UP;
			break;
		case (Qt::Key_Down):
			result = KC_DOWN;
			break;
		case (Qt::Key_Shift):
			result = KC_SHIFT_L;
			break;
		case (Qt::Key_Control):
			result = KC_CONTROL_L;
			break;
		case (Qt::Key_Alt):
			result = KC_ALT_L;
			break;
	}
	return result;
}

// Instantiate a progress dialog
void gui_qt::progress_create(const char *jobtitle, int stepstodo)
{
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!does_exist)
	{
		gui.text_progress_create(jobtitle, stepstodo);
		return;
	}
	// Check that a progress dialog isn't already running
	if (mainwindow->progressindicator->isVisible())
	{
		printf("Weird programmatical event - second progress dialog creation request!\n");
		return;
	}
	mainwindow->progressbar->setMaximum(stepstodo);
	mainwindow->progressbar->setValue(0);
	mainwindow->progresslabel->setText(jobtitle);
	mainwindow->progressindicator->setVisible(TRUE);
	progress_canceled = FALSE;
	// Disable some key widgets on the main form
	mainwindow->ui.MainWindowStack->setEnabled(FALSE);
	mainwindow->ui.ModelViewFrame->setEnabled(FALSE);
	mainwindow->ui.StackButtonsFrame->setEnabled(FALSE);
}

// Update the progress dialog
bool gui_qt::progress_update(int currentstep)
{
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!does_exist)
	{
		gui.text_progress_update(currentstep);
		return TRUE;
	}
	if (!mainwindow->progressindicator->isVisible())
	{
		printf("Weird programmatical event - tried to update a non-existent progress dialog!\n");
		return TRUE;
	}
	mainwindow->progressbar->setValue(currentstep);
	app->processEvents();
	// Check to see if the abort button was pressed
	return (!progress_canceled);
}

// Terminate the progress dialog
void gui_qt::progress_terminate()
{
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!does_exist)
	{
		gui.text_progress_terminate();
		return;
	}
	if (!mainwindow->progressindicator->isVisible())
	{
		printf("Weird programmatical event - tried to terminate a non-existent progress dialog!\n");
		return;
	}
	// Hide the progress bar and re-enable widgets
	mainwindow->progressindicator->setVisible(FALSE);
	mainwindow->ui.MainWindowStack->setEnabled(TRUE);
	mainwindow->ui.ModelViewFrame->setEnabled(TRUE);
	mainwindow->ui.StackButtonsFrame->setEnabled(TRUE);
}

// Stop trajectory playback
void gui_qt::stop_trajectory_playback()
{
	mainwindow->ui.ModelView->killTimer(trajectory_timerid);
	mainwindow->ui.actionPlayPause->setChecked(FALSE);
	trajectory_playing = FALSE;
	refresh();
}

// Instantiate text-based progress dialog
void gui_qt::text_progress_create(const char *jobtitle, int stepstodo)
{
	// Reset the counters
	textprogress_stepstodo = stepstodo;
	textprogress_percent = 0;
	// Print out the empty progress indicator
	printf("--- %s\n", jobtitle);
	printf("Progress [-]                              (  0%%)");
}

// Update the text progress dialog
void gui_qt::text_progress_update(int currentstep)
{
	static char *twister = "-\\|/";
	static char *c = twister;
	static int n, ndots;
	static double dpercent;
	static int percent;
	// Work out percentage and print dots and spaces
	dpercent = double(currentstep) / double(textprogress_stepstodo);
	percent = int(dpercent * 100.0);
	ndots = int(dpercent * 30.0);
	dpercent *= 100.0;
	if (percent != textprogress_percent)
	{
		// Print the header
		printf("\rProgress [%c]",*c);
		// Increase the twoster character
		c ++;
		if (*c == '\0') c = twister;
		for (n=0; n<ndots; n++) printf(".");
		for (n=ndots; n<30; n++) printf(" ");
		// Lastly, print percentage
		printf("(%-3i%%)",percent);
		textprogress_percent = percent;
	}
}

// Terminate the text progress dialog
void gui_qt::text_progress_terminate()
{
	printf("\n");
}

// Add filename to recent files list
void gui_qt::add_recent(const char *filename)
{
	mainwindow->add_recent(filename);
}
