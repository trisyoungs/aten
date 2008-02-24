/*
	*** Qt user interface initialisation functions
	*** src/gui/mainwindow_init.cpp
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

#include "gui/mainwindow.h"
#include <QtGui/QFileDialog>
#include <QtCore/QSettings>

// Finalise GUI
void AtenForm::finalise_ui()
{
	dbg_begin(DM_CALLS,"AtenForm::finalise_ui");
	filter *f;
	int n;
	char temp[256];
	QStringList filters;

	// Set the title of the main window to reflect the version
	sprintf(temp,"Aten (beta %s)",PACKAGE_VERSION);
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
	ui.AtomTree->setEditTriggers(QAbstractItemView::DoubleClicked);

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
	switch (prefs.get_render_style())
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
	uaGroup->addButton(ui.AtomAddHydrogenButton);
	uaGroup->addButton(ui.ProbeAtomButton);
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
	savebitmapdialog = new QFileDialog(this);
	savebitmapdialog->setWindowTitle("Save Image");
	savebitmapdialog->setAcceptMode(QFileDialog::AcceptSave);
	savebitmapdialog->setDirectory(master.workdir.get());
	savebitmapdialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (n=0; n < BIF_NITEMS; n++) filters << filter_from_BIF( (bitmap_format) n);
	savebitmapdialog->setFilters(filters);

	// Create save vector dialog
	savevectordialog = new QFileDialog(this);
	savevectordialog->setWindowTitle("Save Vector");
	savevectordialog->setAcceptMode(QFileDialog::AcceptSave);
	savevectordialog->setDirectory(master.workdir.get());
	savevectordialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (n=0; n < VIF_NITEMS; n++) filters << filter_from_VIF( (vector_format) n);
	savevectordialog->setFilters(filters);

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
	switch (prefs.get_render_style())
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

