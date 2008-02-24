/*
	*** Qt main window functions
	*** src/gui/mainwindow_funcs.cpp
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
#include "base/elements.h"
#include "methods/sd.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include <QtGui/QFileDialog>
#include <QtGui/QKeyEvent>
#include <QtGui/QProgressBar>
#include <QtCore/QSettings>

#include "base/sysfunc.h"

// Bitmap Image Formats (conform to allowable pixmap formats in Qt)
const char *BIF_filters[BIF_NITEMS] = { "Windows Bitmap (*.bmp)", "Joint Photographic Experts Group (*.jpg)", "Portable Network Graphics (*.png)", "Portable Pixmap (*.ppm)", "X11 Bitmap (*.xbm)", "X11 Pixmap (*.xpm)" };
const char *BIF_extensions[BIF_NITEMS] = { "bmp", "jpg", "png", "ppm", "xbm", "xpm" };
bitmap_format BIF_from_text(const char *s)
	{ return (bitmap_format) enum_search("bitmap format",BIF_NITEMS,BIF_extensions,s); }
const char *filter_from_BIF(bitmap_format bif)
	{ return BIF_filters[bif]; }
const char *extension_from_BIF(bitmap_format bif)
	{ return BIF_extensions[bif]; }

// Constructor
AtenForm::AtenForm(QMainWindow *parent) : QMainWindow(parent)
{
	int i;
	for (i=0; i<SP_NITEMS; i++) stackbuttons[i] = NULL;
	ui.setupUi(this);
}

// Catch window close event
void AtenForm::closeEvent(QCloseEvent *event)
{
	if (gui.save_before_close())
	{
		save_settings();
		event->accept();
	}
	else event->ignore();
}

/*
// Input
*/

// Change user interaction mode
void AtenForm::set_useraction(bool on, user_action ua)
{
	// We pass all changes to the user interaction mode through here.
	// This way we can 'link' the selectToolBar and all the other buttons....
	if (!on) return;
	if ((ua >= UA_PICKSELECT) && (ua <= UA_PICKRADIAL))
	{
		// One of the select actions in selectGroup
		dummybutton->setChecked(TRUE);
	}
	else
	{
		// One of the buttons in uaGroup
		QAction *action = selectGroup->checkedAction();
		if (action != NULL) action->setChecked(FALSE);
	}
	gui.mainview.set_selectedmode(ua);
}

void AtenForm::keyPressEvent(QKeyEvent *event)
{
	key_code kc = gui.convert_to_KC(event->key());
	if (kc != KC_OTHER) gui.mainview.inform_keydown(kc);
	else event->ignore();
}

void AtenForm::keyReleaseEvent(QKeyEvent *event)
{
	key_code kc = gui.convert_to_KC(event->key());
	if (kc != KC_OTHER) gui.mainview.inform_keyup(kc);
	else event->ignore();
}

/*
// Model Navigation / Management
*/

void AtenForm::on_ModelTabs_currentChanged(int n)
{
	dbg_begin(DM_CALLS,"AtenForm::on_ModelTabs_currentChanged");
	// Different model tab has been selected, so set master.currentmodel to reflect it.
	master.set_currentmodel(master.get_model(n));
	gui.refresh();
	dbg_end(DM_CALLS,"AtenForm::on_ModelTabs_currentChanged");
}

/*
// Widget Stack Functions
*/

void AtenForm::switch_stack(int buttonid, bool checked)
{
	// If the state of the button is *not* checked then we hide the stack since no buttons are checked. Otherwise, uncheck all other buttons and show the corresponding widget in the stack for this button.
	int n;
	user_action ua = gui.mainview.get_selectedmode();
	if (checked)
	{
		for (n=0; n<SP_NITEMS; n++) if (n != buttonid) stackbuttons[n]->setChecked(FALSE);
		ui.MainWindowStack->setCurrentIndex(buttonid);
		ui.MainWindowStack->show();
		// If the new visible page is the atom list, do a quick refresh of it
		if (buttonid == SP_ATOMS) refresh_atompage();
		// Change to plain selection mode 
	}
	else ui.MainWindowStack->hide();
	// Choose a plain selection mode again...
	if ((ua == UA_NONE) || (ua > UA_PICKRADIAL))
	{
		ui.actionSelectAtoms->setChecked(TRUE);
		set_useraction(TRUE, UA_PICKSELECT);
	}
	//ui.actionSelectAtoms->setChecked(TRUE);
	//set_useraction(TRUE, UA_PICKSELECT);
	master.get_currentmodel()->log_change(LOG_CAMERA);
}

void AtenForm::refresh_modeltabs()
{
	dbg_begin(DM_CALLS,"AtenForm::refresh_modeltabs");
	// Set names on tabs
	int count = 0;
	for (model *m = master.get_models(); m != NULL; m = m->next)
	{
		ui.ModelTabs->setTabText(count, m->get_name());
		count ++;
	}
	dbg_end(DM_CALLS,"AtenForm::refresh_modeltabs");
}

void AtenForm::execute_command()
{
	// Clear old script commands
	master.cmd_script.clear();
	// Grab the current text of the line edit
	parser.get_args_delim(qPrintable(command_edit->text()), PO_USEQUOTES);
	// Check for no commands given
	if (parser.get_nargs() == 0) return;
	if (master.cmd_script.cache_command()) master.cmd_script.execute(NULL);
	gui.refresh();
	command_edit->setText("");
}

// Cancel progress indicator
void AtenForm::progress_cancel()
{
	gui.notify_progress_canceled();
}

// Save program settings
void AtenForm::save_settings()
{
	char temp[128];
	// Save the recent file entries
	for (int i=0; i<MAXRECENTFILES; i++)
	{
		// Create name tag
		strcpy(temp,"RecentFile");
		strcat(temp,itoa(i));
		//if (actionRecentFile[i]->isVisible()) printf("action %i is visible\n",i);
		if (actionRecentFile[i]->isVisible()) settings->setValue(temp,actionRecentFile[i]->data().toString());
		else settings->remove(temp);
	}
}

// Load recent file
void AtenForm::load_recent()
{
	dnchar filename;
	model *m;
	filter *f;
	// Cast sending QAction and grab filename
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenForm::load_recent - Sender was not a QAction.\n");
		return;
	}
	// Grab the filename from the action
	filename = qPrintable(action->data().toString());
	// See if any loaded model filename matches this filename
	for (m = master.get_models(); m != NULL; m = m->next)
	{
		msg(DM_VERBOSE,"Checking loaded models for '%s': %s\n",filename.get(),m->get_filename());
		if (filename == m->get_filename())
		{
			msg(DM_VERBOSE,"Matched filename to loaded model.\n");
			master.set_currentmodel(m);
			return;
		}
	}
	// If we get to here then the model is not currently loaded...
	f = master.probe_file(filename.get(), FT_MODEL_IMPORT);
	if (f != NULL)
	{
		f->execute(filename.get());
		master.get_currentmodel()->log_change(LOG_VISUAL);
		gui.refresh();
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
void AtenForm::add_recent(const char *filename)
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
			sprintf(temp,"&%i %s", n, remove_path(qPrintable(actionRecentFile[n]->data().toString())));
			actionRecentFile[n+1]->setText(temp);
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
		}
		last = 0;
	}
	// Set the new data
	sprintf(temp,"&%i %s (%s)",last,remove_path(filename),filename);
	actionRecentFile[last]->setText(temp);
	actionRecentFile[last]->setData(filename);
	actionRecentFile[last]->setVisible(TRUE);
}

// Update undo/redo actions in Edit menu
void AtenForm::update_undoredo()
{
	static char text[128];
	model *m = master.get_currentmodel();
	// Check the model's state pointers
	if (m->get_currentundostate() == NULL)
	{
		ui.actionEditUndo->setText("Undo");
		ui.actionEditUndo->setEnabled(FALSE);
	}
	else
	{
		strcpy(text,"Undo (");
		strcat(text,m->get_currentundostate()->get_text());
		strcat(text,")");
		ui.actionEditUndo->setText(text);
		ui.actionEditUndo->setEnabled(TRUE);
	}
	if (m->get_currentredostate() == NULL)
	{
		ui.actionEditRedo->setText("Redo");
		ui.actionEditRedo->setEnabled(FALSE);
	}
	else
	{
		strcpy(text,"Redo (");
		strcat(text,m->get_currentredostate()->get_text());
		strcat(text,")");
		ui.actionEditRedo->setText(text);
		ui.actionEditRedo->setEnabled(TRUE);
	}
}
