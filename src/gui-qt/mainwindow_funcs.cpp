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
	for (int i=0; i<SP_NITEMS; i++) stackbuttons[i] = NULL;
	ui.setupUi(this);
}

// Finalise GUI
void AtenForm::finalise_ui()
{
	dbg_begin(DM_CALLS,"AtenForm::finalise_ui");
	filter *f;
	int n;
	char title[128];
	QStringList filters;

	// Set the title of the main window to reflect the version
	strcpy(title,"Aten (beta ");
	strcat(title,PACKAGE_VERSION);
	strcat(title,")");
	setWindowTitle(title);

	// Create QActionGroup for draw styles
	QActionGroup *alignmentGroup = new QActionGroup(this);
	alignmentGroup->addAction(ui.actionStyleStick);
	alignmentGroup->addAction(ui.actionStyleTube);
	alignmentGroup->addAction(ui.actionStyleSphere);
	alignmentGroup->addAction(ui.actionStyleScaled);
	alignmentGroup->addAction(ui.actionStyleIndividual);

	// Hide trajectory and command toolbars initially
	ui.TrajectoryToolBar->setVisible(FALSE);
	ui.CommandToolBar->setVisible(FALSE);

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

	// Create master button group for the Edit stackpage
	QButtonGroup *editGroup = new QButtonGroup(this);
	editGroup->addButton(ui.SelectAtomsButton);
	editGroup->addButton(ui.SelectMoleculesButton);
	// Set correct draw_style on toolbar
	switch (prefs.get_static_style())
	{
		case (DS_STICK): ui.actionStyleStick->setChecked(true); break;
		case (DS_TUBE): ui.actionStyleTube->setChecked(true); break;
		case (DS_SPHERE): ui.actionStyleSphere->setChecked(true); break;
		case (DS_SCALED): ui.actionStyleScaled->setChecked(true); break;
		case (DS_INDIVIDUAL): ui.actionStyleIndividual->setChecked(true); break;
	}	editGroup->addButton(ui.SelectElementsButton);
	editGroup->addButton(ui.DrawAtomButton);
	editGroup->addButton(ui.DrawChainButton);
	editGroup->addButton(ui.DrawDeleteButton);
	editGroup->addButton(ui.DrawTransmuteButton);
	editGroup->addButton(ui.MeasureDistanceButton);
	editGroup->addButton(ui.MeasureAngleButton);
	editGroup->addButton(ui.MeasureTorsionButton);
	editGroup->addButton(ui.BondSingleButton);
	editGroup->addButton(ui.BondDoubleButton);
	editGroup->addButton(ui.BondTripleButton);
	editGroup->addButton(ui.BondDeleteButton);
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
	ui.MainWindowStatusBar->insertPermanentWidget(0,statuslabel,0);
	statuslabel->setFrameStyle(QFrame::NoFrame);

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
		case (DS_STICK): ui.actionStyleStick->setChecked(true); break;
		case (DS_TUBE): ui.actionStyleTube->setChecked(true); break;
		case (DS_SPHERE): ui.actionStyleSphere->setChecked(true); break;
		case (DS_SCALED): ui.actionStyleScaled->setChecked(true); break;
		case (DS_INDIVIDUAL): ui.actionStyleIndividual->setChecked(true); break;
	}
	// Set controls in minimiser page
	ui.SDMaxStepSpin->setValue(sd.get_maxstep());
	ui.SDMaxLineTrialsSpin->setValue(sd.get_maxlinetrials());
	dbg_end(DM_CALLS,"AtenForm::set_controls");
}

/*
// Input
*/

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
	if (checked)
	{
		for (n=0; n<SP_NITEMS; n++) if (n != buttonid) stackbuttons[n]->setChecked(FALSE);
		ui.MainWindowStack->setCurrentIndex(buttonid);
		ui.MainWindowStack->show();
		// If the new visible page is the atom list, do a quick refresh of it
		if (buttonid == SP_ATOMS) refresh_atompage();
		// If the checked page button is not the Edit Page, make sure we're in select mode
		if (buttonid != SP_EDIT)
		{
			ui.SelectAtomsButton->setChecked(TRUE);
			gui.mainview.set_selectedmode(UA_PICKSELECT);
		}
	}
	else
	{
		ui.MainWindowStack->hide();
		ui.SelectAtomsButton->setChecked(TRUE);
		gui.mainview.set_selectedmode(UA_PICKSELECT);
	}
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
	parser.get_args_delim(qPrintable(command_edit->text()), PO_DEFAULTS);
	// Check for no commands given
	if (parser.get_nargs() == 0) return;
	if (master.cmd_script.cache_command()) master.cmd_script.run();
	gui.refresh();
	command_edit->setText("");
}
