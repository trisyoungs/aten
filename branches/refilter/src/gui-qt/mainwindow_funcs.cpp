/*
	*** Qt main window functions
	*** src/gui-qt/mainwindow_funcs.cpp
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
#include "gui-qt/mainwindow.h"
#include <QtGui/QFileDialog>
#include <QtGui/QKeyEvent>
#include <QtGui/QProgressBar>
#include <QtCore/QSettings>

// Image Formats
const char *PF_filters[PF_NITEMS] = { "Windows Bitmap (*.bmp)", "Joint Photographic Experts Group (*.jpg)", "Portable Network Graphics (*.png)", "Portable Pixmap (*.ppm)", "X11 Bitmap (*.xbm)", "X11 Pixmap (*.xpm)" };
const char *PF_extensions[PF_NITEMS] = { "bmp", "jpg", "png", "ppm", "xbm", "xpm" };
const char *filter_from_PF(pixmap_format pf)
	{ return PF_filters[pf]; }
const char *extension_from_PF(pixmap_format pf)
	{ return PF_extensions[pf]; }

// Constructor
AtenForm::AtenForm(QMainWindow *parent) : QMainWindow(parent)
{
	int i;
	for (i=0; i<SP_NITEMS; i++) stackbuttons[i] = NULL;
	ui.setupUi(this);
}

// Finalise GUI
void AtenForm::finalise_ui()
{
	dbg_begin(DM_CALLS,"AtenForm::finalise_ui");
	filter *f;
	int n;
	char temp[256];
	QStringList filters;

	// Set the title of the main window to reflect the version
	strcpy(temp,"Aten (beta ");
	strcat(temp,PACKAGE_VERSION);
	strcat(temp,")");
	setWindowTitle(temp);

	// Initialise application name, organisation and author, and create settings structure
	QCoreApplication::setOrganizationDomain("www.projectaten.org");
	QCoreApplication::setApplicationName("Aten");
	settings = new QSettings;

	// Set up recent files list (create all actions first)
	for (n=0; n<MAXRECENTFILES; n++)
	{
		actionRecentFile[n] = new QAction(this);
		actionRecentFile[n]->setVisible(FALSE);
		QObject::connect(actionRecentFile[n], SIGNAL(triggered()), this, SLOT(load_recent()));
		ui.RecentMenu->addAction(actionRecentFile[n]);
	}
	// -- Now populate list
	for (n=0; n<MAXRECENTFILES; n++)
	{
		// Construct settings value to search for
		strcpy(temp,"RecentFile");
		strcat(temp,itoa(n));
		if (settings->contains(temp)) add_recent(qPrintable(settings->value(temp).toString()));
	}

	// Set editable items in the Atom List
	ui.AtomTreeList->setEditTriggers(QAbstractItemView::DoubleClicked);

	// Create QActionGroup for draw styles
	QActionGroup *styleGroup = new QActionGroup(this);
	styleGroup->addAction(ui.actionStyleStick);
	styleGroup->addAction(ui.actionStyleTube);
	styleGroup->addAction(ui.actionStyleSphere);
	styleGroup->addAction(ui.actionStyleScaled);
	styleGroup->addAction(ui.actionStyleIndividual);

	// Create QActionGroup for selections
	selectGroup = new QActionGroup(this);
	selectGroup->addAction(ui.actionSelectAtoms);
	selectGroup->addAction(ui.actionSelectMolecules);
	selectGroup->addAction(ui.actionSelectElement);

	// Create QActionGroup for Mouse toolbar
	QActionGroup *mousegroup = new QActionGroup(this);
	mousegroup->addAction(ui.actionMouseInteract);
	mousegroup->addAction(ui.actionMouseRotate);
	mousegroup->addAction(ui.actionMouseTranslate);

	// Hide some toolbars initially
	ui.TrajectoryToolBar->setVisible(FALSE);
	ui.CommandToolBar->setVisible(FALSE);
	ui.MouseToolBar->setVisible(FALSE);

	// Add text edit to CommandToolBar
	command_edit = new QLineEdit(this);
	ui.CommandToolBar->addWidget(command_edit);
	QObject::connect(command_edit, SIGNAL(editingFinished()), this, SLOT(execute_command()));

	// Create QActionGroup for perspective / orthographic views
	QActionGroup *viewtypeGroup = new QActionGroup(this);
	viewtypeGroup->addAction(ui.actionViewOrthographic);
	viewtypeGroup->addAction(ui.actionViewPerspective);

	// Create QActionGroup for model / trajectory render source
	QActionGroup *rendersourceGroup = new QActionGroup(this);
	rendersourceGroup->addAction(ui.actionViewModel);
	rendersourceGroup->addAction(ui.actionViewTrajectory);

	// Hide the stack widget initially
	ui.MainWindowStack->hide();

	// Set correct draw_style on toolbar
	switch (prefs.get_static_style())
	{
		case (DS_STICK): ui.actionStyleStick->setChecked(true); break;
		case (DS_TUBE): ui.actionStyleTube->setChecked(true); break;
		case (DS_SPHERE): ui.actionStyleSphere->setChecked(true); break;
		case (DS_SCALED): ui.actionStyleScaled->setChecked(true); break;
		case (DS_INDIVIDUAL): ui.actionStyleIndividual->setChecked(true); break;
	}

	// Create master group for stackpage buttons that change user action modes
	uaGroup = new QButtonGroup(this);
	uaGroup->addButton(ui.DrawAtomButton);
	uaGroup->addButton(ui.DrawChainButton);
	uaGroup->addButton(ui.DrawDeleteButton);
	uaGroup->addButton(ui.DrawTransmuteButton);
	uaGroup->addButton(ui.MeasureDistanceButton);
	uaGroup->addButton(ui.MeasureAngleButton);
	uaGroup->addButton(ui.MeasureTorsionButton);
	uaGroup->addButton(ui.BondSingleButton);
	uaGroup->addButton(ui.BondDoubleButton);
	uaGroup->addButton(ui.BondTripleButton);
	uaGroup->addButton(ui.BondDeleteButton);
	// --- Add dummy button so we can have none of the others selected
	dummybutton = new QPushButton(this);
	dummybutton->setCheckable(TRUE);
	dummybutton->setChecked(TRUE);
	dummybutton->setVisible(FALSE);
	uaGroup->addButton(dummybutton);

	// Create a subgroup for the element select buttons
	QButtonGroup *elementGroup = new QButtonGroup(this);
	elementGroup->addButton(ui.ElementHButton);
	elementGroup->addButton(ui.ElementCButton);
	elementGroup->addButton(ui.ElementNButton);
	elementGroup->addButton(ui.ElementUserButton);

	// Store widgetstack buttons
	stackbuttons[SP_ATOMS] = ui.ShowAtomPageButton;
	stackbuttons[SP_EDIT] = ui.ShowEditPageButton;
	stackbuttons[SP_TRANSFORM] = ui.ShowTransformPageButton;
	stackbuttons[SP_POSITION] = ui.ShowPositionPageButton;
	stackbuttons[SP_CELL] = ui.ShowCellPageButton;
	stackbuttons[SP_MINIMISER] = ui.ShowMinimiserPageButton;
	stackbuttons[SP_DISORDER] = ui.ShowDisorderPageButton;
	stackbuttons[SP_FORCEFIELD] = ui.ShowForcefieldsPageButton;
	stackbuttons[SP_GRID] = ui.ShowGridsPageButton;
	stackbuttons[SP_ANALYSE] = ui.ShowAnalysePageButton;

	// Add permanent statusbar widgets
	statuslabel = new QLabel(this,0);
	ui.MainWindowStatusBar->addPermanentWidget(statuslabel,0);
	statuslabel->setFrameShape(QFrame::NoFrame);
	statuslabel->setFrameShadow(QFrame::Plain);
	progressindicator = new QFrame(this);
	progressindicator->setContentsMargins(0,0,0,0);
	QHBoxLayout *layout = new QHBoxLayout(progressindicator);
	layout->setMargin(0);
	progressbar = new QProgressBar(this);
	progresslabel = new QLabel(this,0);
	progressbutton = new QPushButton(this);
	progressbutton->setText("Cancel");
	QObject::connect(progressbutton, SIGNAL(clicked()), this, SLOT(progress_cancel()));
	layout->addWidget(progressbar,255);
	layout->addWidget(progresslabel,0);
	layout->addWidget(progressbutton,0);
	ui.MainWindowStatusBar->insertPermanentWidget(0,progressindicator,128);
	progressindicator->setVisible(FALSE);

	// Create open model dialog
	dialog[FT_MODEL_IMPORT] = new QFileDialog(this);
	dialog[FT_MODEL_IMPORT]->setFileMode(QFileDialog::ExistingFiles);
	dialog[FT_MODEL_IMPORT]->setDirectory(master.workdir.get());
	dialog[FT_MODEL_IMPORT]->setWindowTitle("Open Model(s)");
	filters << "All files (*)";
	for (f = master.filters[FT_MODEL_IMPORT].first(); f != NULL; f = f->next) filters << f->get_description();
	dialog[FT_MODEL_IMPORT]->setFilters(filters);

	// Create open trajectory dialog
	dialog[FT_TRAJECTORY_IMPORT] = new QFileDialog(this);
	dialog[FT_TRAJECTORY_IMPORT]->setFileMode(QFileDialog::ExistingFile);
	dialog[FT_TRAJECTORY_IMPORT]->setDirectory(master.workdir.get());
	dialog[FT_TRAJECTORY_IMPORT]->setWindowTitle("Add Trajectory");
	filters.clear();
	filters << "All files (*)";
	for (f = master.filters[FT_TRAJECTORY_IMPORT].first(); f != NULL; f = f->next) filters << f->get_description();
	dialog[FT_TRAJECTORY_IMPORT]->setFilters(filters);

	// Create save model dialog
	dialog[FT_MODEL_EXPORT] = new QFileDialog(this);
	dialog[FT_MODEL_EXPORT]->setWindowTitle("Save Model");
	dialog[FT_MODEL_EXPORT]->setAcceptMode(QFileDialog::AcceptSave);
	dialog[FT_MODEL_EXPORT]->setDirectory(master.workdir.get());
	dialog[FT_MODEL_EXPORT]->setConfirmOverwrite(TRUE);
	dialog[FT_MODEL_EXPORT]->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (f = master.filters[FT_MODEL_EXPORT].first(); f != NULL; f = f->next) filters << f->get_description();
	// Check for empty filters list (causes crash)
	if (!filters.empty()) dialog[FT_MODEL_EXPORT]->setFilters(filters);

	// Create save image dialog
	saveimagedialog = new QFileDialog(this);
	saveimagedialog->setWindowTitle("Save Image");
	saveimagedialog->setAcceptMode(QFileDialog::AcceptSave);
	saveimagedialog->setDirectory(master.workdir.get());
	saveimagedialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (n=0; n < PF_NITEMS; n++) filters << filter_from_PF( (pixmap_format) n);
	saveimagedialog->setFilters(filters);

	// Create open forcefield dialog
	dialog[FT_FIELD_IMPORT] = new QFileDialog(this);
	dialog[FT_FIELD_IMPORT]->setWindowTitle("Open Forcefield");
	dialog[FT_FIELD_IMPORT]->setDirectory(master.workdir.get());
	dialog[FT_FIELD_IMPORT]->setFileMode(QFileDialog::ExistingFile);
	filters.clear();
	filters << "All files (*)";
	filters << "Forcefields (*.ff)";
	dialog[FT_FIELD_IMPORT]->setFilters(filters);

	// Create open grid dialog
	dialog[FT_GRID_IMPORT] = new QFileDialog(this);
	dialog[FT_GRID_IMPORT]->setWindowTitle("Open Grid");
	dialog[FT_GRID_IMPORT]->setDirectory(master.workdir.get());
	dialog[FT_GRID_IMPORT]->setFileMode(QFileDialog::ExistingFile);
	filters.clear();
	filters << "All files (*)";
	for (f = master.filters[FT_GRID_IMPORT].first(); f != NULL; f = f->next) filters << f->get_description();
	dialog[FT_GRID_IMPORT]->setFilters(filters);

	dbg_end(DM_CALLS,"AtenForm::finalise_ui");
}

// Set controls
void AtenForm::set_controls()
{
	dbg_begin(DM_CALLS,"AtenForm::set_controls");
	// Set correct draw_style on toolbar
	switch (prefs.get_static_style())
	{
		case (DS_STICK): ui.actionStyleStick->setChecked(TRUE); break;
		case (DS_TUBE): ui.actionStyleTube->setChecked(TRUE); break;
		case (DS_SPHERE): ui.actionStyleSphere->setChecked(TRUE); break;
		case (DS_SCALED): ui.actionStyleScaled->setChecked(TRUE); break;
		case (DS_INDIVIDUAL): ui.actionStyleIndividual->setChecked(TRUE); break;
	}
	// Set some menu items
	prefs.using_perspective() ? ui.actionViewPerspective->setChecked(TRUE) : ui.actionViewOrthographic->setChecked(TRUE);
	// Set controls on edit page
	ui.BondToleranceSpin->setValue(prefs.get_bond_tolerance());
	dbg_end(DM_CALLS,"AtenForm::set_controls");
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
	else
	{
		ui.MainWindowStack->hide();

	}
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
	master.cmd_script.commands.clear();
	// Grab the current text of the line edit
	parser.get_args_delim(qPrintable(command_edit->text()), PO_USEQUOTES);
	// Check for no commands given
	if (parser.get_nargs() == 0) return;
	if (master.cmd_script.cache_command()) master.cmd_script.run();
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
		if (filename == m->get_filename())
		{
			printf("Matched filename to model.\n");
			master.set_currentmodel(m);
			return;
		}
	}
	// If we get to here then the model is not currently loaded...
	f = master.probe_file(filename.get(), FT_MODEL_IMPORT);
	if (f != NULL) f->import_model(filename.get());

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
	sprintf(temp,"&%i %s",last,remove_path(filename));
	actionRecentFile[last]->setText(temp);
	actionRecentFile[last]->setData(filename);
	actionRecentFile[last]->setVisible(TRUE);
}
