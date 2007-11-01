/*
	*** Qt user interface functions
	*** src/gui-qt/gui-qt.cpp
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
#include "gui-qt/canvas-qt.h"
#include "gui-qt/gui-qt.h"
#include "gui-qt/mainwindow.h"
#include "gui-qt/prefs.h"
#include <QtGui/QMessageBox>
#include <QtCore/QTextStream>
#include <QtGui/QProgressDialog>

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
	trajectory_playing = FALSE;
	trajectory_timerid = -1;
	progress = NULL;
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
	mainwindow->refresh_surfacespage();
	mainwindow->refresh_forcefieldpage();

	// Start timer
	if (master.use_timer) master.start_timer_events();

	int n = app->exec();
	dbg_end(DM_CALLS,"gui_qt::run");
}

// Set GUI to reflect prefs
void gui_qt::set_controls()
{
	// All done in finalise_ui() calls.
}

// Show window
void gui_qt::show(gui_window gw)
{
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

// Change msgbox font
void gui_qt::change_msg_font(const char *font)
{
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
		s += ")";
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
	s += " g ";
	cell_type ct = m->cell.get_type();
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

// Redraw main window canvas
void gui_qt::refresh()
{
	if (!does_exist) return;
	// Update labels on status bar
	gui.update_labels();
	// Update contents of the atom list
	mainwindow->refresh_atompage();
	// Update the contents of the cell page
	mainwindow->refresh_cellpage();
	// Update the disorder page
	mainwindow->refresh_disorderpage();
	// Update trajectory playback controls
	gui.update_trajcontrols();
	// Request redraw of the main canvas
	mainview.postredisplay();
	// If the model save_point is recent, disable save button
	mainwindow->ui.actionFileSave->setEnabled(master.get_currentmodel()->is_modified());
	// Enable the Atom menu if one or more atoms are selected
	mainwindow->ui.AtomMenu->setEnabled( master.get_currentmodel()->get_nselected() == 0 ? FALSE : TRUE);
	// Enable View->Trajectory menu item if as trajectory is associated
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

void gui_qt::close_application()
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
					return;
				// Save model before quit
				case (QMessageBox::Save):
					// If model has a filter set, just save it
					f = m->get_filter();
					if (f != NULL) f->export_model(m);
					else if (mainwindow->run_savemodel_dialog())
					{
						m->set_filter(mainwindow->savemodelfilter);
						m->set_filename(mainwindow->savemodelfilename.get());
						mainwindow->savemodelfilter->export_model(m);
					}
					else return;
					break;
			}
		}
	}
	// Close the GUI
	app->exit(0);
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
	if (progress != NULL)
	{
		printf("Weird programmatical event - second progress dialog creation request!\n");
		return;
	}
	progress = new QProgressDialog(jobtitle, "Abort", 0, stepstodo);
	progress->setModal(TRUE);
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
	if (progress == NULL)
	{
		printf("Weird programmatical event - tried to update a non-existent progress dialog!\n");
		return TRUE;
	}
	progress->setValue(currentstep);
	app->processEvents();
	// Check to see if the abort button was pressed
	return (progress->wasCanceled() ? FALSE : TRUE);
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
	if (progress == NULL)
	{
		printf("Weird programmatical event - tried to terminate a non-existent progress dialog!\n");
		return;
	}
	progress->setValue(progress->maximum());
	delete progress;
	progress = NULL;
}

// Stop trajectory playback
void gui_qt::stop_trajectory_playback()
{
	mainwindow->ui.ModelView->killTimer(trajectory_timerid);
	mainwindow->ui.actionPlayPause->setChecked(FALSE);
	trajectory_playing = FALSE;
	refresh();
}
