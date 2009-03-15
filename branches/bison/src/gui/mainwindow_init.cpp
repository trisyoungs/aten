/*
	*** Qt user interface initialisation functions
	*** src/gui/mainwindow_init.cpp
	Copyright T. Youngs 2007-2009

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
#include "parser/tree.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include "gui/grids.h"

// Finalise GUI
void AtenForm::finaliseUi()
{
	msg.enter("AtenForm::finaliseUi");
	int n;

	// Set the title of the main window to reflect the version
	char title[64];
	sprintf(title, "Aten (%s)", ATENVERSION);
	setWindowTitle(title);

	// Initialise application name, organisation and author
	QCoreApplication::setOrganizationDomain("www.projectaten.net");
	QCoreApplication::setApplicationName("Aten");

	// Set up recent files list (create all actions first)
	for (n=0; n<MAXRECENTFILES; n++)
	{
		actionRecentFile[n] = new QAction(this);
		actionRecentFile[n]->setVisible(FALSE);
		QObject::connect(actionRecentFile[n], SIGNAL(triggered()), this, SLOT(loadRecent()));
		ui.RecentMenu->addAction(actionRecentFile[n]);
	}

	// Create QActionGroup for draw styles
	QActionGroup *styleGroup = new QActionGroup(this);
	styleGroup->addAction(ui.actionStyleStick);
	styleGroup->addAction(ui.actionStyleTube);
	styleGroup->addAction(ui.actionStyleSphere);
	styleGroup->addAction(ui.actionStyleScaled);
	styleGroup->addAction(ui.actionStyleIndividual);

	// Create QActionGroup for colour schemes
	QActionGroup *schemeGroup = new QActionGroup(this);
	schemeGroup->addAction(ui.actionSchemeElement);
	schemeGroup->addAction(ui.actionSchemeCharge);
	schemeGroup->addAction(ui.actionSchemeForce);

	// Create QActionGroup for Mouse toolbar
	QActionGroup *mousegroup = new QActionGroup(this);
	mousegroup->addAction(ui.actionMouseInteract);
	mousegroup->addAction(ui.actionMouseRotate);
	mousegroup->addAction(ui.actionMouseTranslate);

	// Hide some toolbars initially
	ui.BondToolbar->setVisible(FALSE);
	//ui.DrawToolbar->setVisible(FALSE);
	ui.CommandToolbar->setVisible(FALSE);
	ui.ForcefieldsToolbar->setVisible(FALSE);
	ui.MeasureToolbar->setVisible(FALSE);
	ui.TrajectoryToolbar->setVisible(FALSE);

	// Add text edit to CommandToolBar
	commandEdit_ = new QLineEdit(this);
	ui.CommandToolbar->addWidget(commandEdit_);
	ui.CommandToolbar->setMinimumSize(128,16);
	QObject::connect(commandEdit_, SIGNAL(returnPressed()), this, SLOT(executeCommand()));

	// Add combobox to ForcefieldsToolbar
	forcefieldCombo_ = new QComboBox(this);
	ui.ForcefieldsToolbar->addWidget(forcefieldCombo_);
	ui.CommandToolbar->setMinimumSize(128,16);
	QObject::connect(forcefieldCombo_, SIGNAL(currentIndexChanged(int)), this, SLOT(forcefieldCombo_currentIndexChanged(int)));

	// Add extra widgets to trajectory toolbar
	trajectorySlider_ = new QSlider(Qt::Horizontal, this);
	ui.TrajectoryToolbar->addWidget(trajectorySlider_);
	QObject::connect(trajectorySlider_, SIGNAL(sliderMoved(int)), this, SLOT(trajectorySlider_sliderMoved(int)));
	trajectorySpin_ = new QSpinBox(this);
	ui.TrajectoryToolbar->addWidget(trajectorySpin_);
	QObject::connect(trajectorySpin_, SIGNAL(valueChanged(int)), this, SLOT(trajectorySpin_valueChanged(int)));

	// Create QActionGroup for perspective / orthographic views
	QActionGroup *viewtypeGroup = new QActionGroup(this);
	viewtypeGroup->addAction(ui.actionViewOrthographic);
	viewtypeGroup->addAction(ui.actionViewPerspective);

	// Create QActionGroup for model / trajectory render source
	QActionGroup *rendersourceGroup = new QActionGroup(this);
	rendersourceGroup->addAction(ui.actionViewModel);
	rendersourceGroup->addAction(ui.actionViewTrajectory);

	// Add the previously-created QGLWidget to the main interface, and set up calls
	QVBoxLayout *vbox = new QVBoxLayout();
	vbox->setMargin(0);
	gui.mainWidget->setParent(this);
	gui.mainWidget->setMouseTracking(TRUE);
	gui.mainWidget->setFocusPolicy(Qt::StrongFocus);
	vbox->addWidget( (QWidget*) gui.mainWidget);
	ui.ViewFrame->setLayout(vbox);

	// Set correct Atom::DrawStyle on toolbar
	switch (prefs.renderStyle())
	{
		case (Atom::StickStyle): ui.actionStyleStick->setChecked(true); break;
		case (Atom::TubeStyle): ui.actionStyleTube->setChecked(true); break;
		case (Atom::SphereStyle): ui.actionStyleSphere->setChecked(true); break;
		case (Atom::ScaledStyle): ui.actionStyleScaled->setChecked(true); break;
		case (Atom::IndividualStyle): ui.actionStyleIndividual->setChecked(true); break;
	}

	// Add bond tolerance spinbox to Bond Toolbar
	bondToleranceSpin_ = new QDoubleSpinBox(this);
	bondToleranceSpin_->setRange(0.0, 100.0);
	bondToleranceSpin_->setSingleStep(0.01);
	ui.BondToolbar->addWidget(bondToleranceSpin_);
	QObject::connect(bondToleranceSpin_, SIGNAL(valueChanged(double)), this, SLOT(bondTolerance_valueChanged(double)));

	// Create master group for toolbar buttons that change user action modes
	uaGroup = new QActionGroup(this);
	// Select Toolbar
	uaGroup->addAction(ui.actionSelectAtoms);
	uaGroup->addAction(ui.actionSelectMolecules);
	uaGroup->addAction(ui.actionSelectElement);
	// Draw Toolbar
	uaGroup->addAction(ui.actionDrawAtom);
	uaGroup->addAction(ui.actionDrawChain);
	uaGroup->addAction(ui.actionDeleteAtom);
	uaGroup->addAction(ui.actionTransmuteAtom);
	uaGroup->addAction(ui.actionBondSingle);
	uaGroup->addAction(ui.actionBondDouble);
	uaGroup->addAction(ui.actionBondTriple);
	uaGroup->addAction(ui.actionDeleteBond);
	uaGroup->addAction(ui.actionAddHydrogenAtom);
	uaGroup->addAction(ui.actionAtomProbe);
	// Measure Toolbar
	uaGroup->addAction(ui.actionMeasureDistance);
	uaGroup->addAction(ui.actionMeasureAngle);
	uaGroup->addAction(ui.actionMeasureTorsion);
	// Invisible tool button for PickAtomsAction
	dummyToolButton = new QAction(this);
	dummyToolButton->setCheckable(TRUE);
	uaGroup->addAction(dummyToolButton);
	

	// Create a subgroup for the element select buttons
	QActionGroup *elementGroup = new QActionGroup(this);
	elementGroup->addAction(ui.actionElementH);
	elementGroup->addAction(ui.actionElementC);
	elementGroup->addAction(ui.actionElementN);
	elementGroup->addAction(ui.actionElementCustom);

	// Fix up the statusbar with a single big frame and no size grip
	ui.MainWindowStatusBar->setSizeGripEnabled(FALSE);
	QFrame *frame = new QFrame(this);
	ui.MainWindowStatusBar->addPermanentWidget(frame,1);
	// Message label
	QHBoxLayout *lablayout = new QHBoxLayout(frame);
	messageLabel = new QLabel(this);
	messageLabel->setTextFormat(Qt::RichText);
	messageLabel->setWordWrap(TRUE);
	QFont font = messageLabel->font();
	font.setPointSize(8);
	messageLabel->setFont(font);
	lablayout->addWidget(messageLabel, 100);
	QFrame *sep = new QFrame;
	sep->setFrameStyle(QFrame::VLine);
	lablayout->addWidget(sep,0);
	// Info labels
	QVBoxLayout *infolayout = new QVBoxLayout;
	infoLabel1 = new QLabel(this);
	infoLabel1->setFont(font);
	infolayout->addWidget(infoLabel1);
	infoLabel2 = new QLabel(this);
	infoLabel2->setFont(font);
	infolayout->addWidget(infoLabel2);
	lablayout->addLayout(infolayout,0);
	// Progress indicator
	progressIndicator = new QFrame(this);
	progressIndicator->setContentsMargins(0,0,0,0);
	QHBoxLayout *layout = new QHBoxLayout(progressIndicator);
	layout->setMargin(0);
	progressBar = new QProgressBar(this);
	progressLabel = new QLabel(this,0);
	progressButton = new QPushButton(this);
	progressButton->setText("Cancel");
	QObject::connect(progressButton, SIGNAL(clicked()), this, SLOT(progressCancel()));
	layout->addWidget(progressBar,255);
	layout->addWidget(progressLabel,0);
	layout->addWidget(progressButton,0);
	ui.MainWindowStatusBar->insertPermanentWidget(0,progressIndicator,128);
	progressIndicator->setVisible(FALSE);

	// Populate scripts menu
	refreshScriptsMenu();

	// Load Qt Settings
	loadSettings();

	// Create filter lists for file dialogs
	createDialogFilters();

	msg.exit("AtenForm::finaliseUi");
}

// Set filter combos on file dialogs
void AtenForm::createDialogFilters()
{
	msg.enter("AtenForm::createDialogFilters");
	Tree *filter;
	int n;

	// Model Import
	loadModelFilters.clear();
	loadModelFilters += "All files (*)";
	for (filter = aten.filters(Tree::ModelImport); filter != NULL; filter = filter->next)
	{
		loadModelFilters += ";;";
		loadModelFilters += filter->description();
	}
	ui.actionFileOpen->setEnabled(!loadModelFilters.isEmpty());
	ui.RecentMenu->setEnabled(!loadModelFilters.isEmpty());

	// Trajectory Import
	loadTrajectoryFilters.clear();
	loadTrajectoryFilters += "All files (*)";
	for (filter = aten.filters(Tree::TrajectoryImport); filter != NULL; filter = filter->next)
	{
		loadTrajectoryFilters += ";;";
		loadTrajectoryFilters += filter->description();
	}
	ui.actionFileAddTrajectory->setEnabled(!loadTrajectoryFilters.isEmpty());

	// Model Export
	saveModelFilters.clear();
	for (filter = aten.filters(Tree::ModelExport); filter != NULL; filter = filter->next)
	{
		if (!saveModelFilters.isEmpty()) saveModelFilters += ";;";
		saveModelFilters += filter->description();
	}
	// Check for empty filters list
	ui.actionFileSave->setEnabled(!saveModelFilters.isEmpty());
	ui.actionFileSaveAs->setEnabled(!saveModelFilters.isEmpty());

	// Save image
	saveBitmapFilters.clear();
	for (n=0; n < GuiQt::nBitmapFormats; n++)
	{
		if (!saveBitmapFilters.isEmpty()) saveBitmapFilters += ";;";
		saveBitmapFilters += GuiQt::bitmapFormatFilter( (GuiQt::BitmapFormat) n);
	}

	// Save vector
// 	saveVectorDialog->filters().clear();
// 	filters.clear();
// 	for (n=0; n < VIF_NITEMS; n++) filters << filter_from_VIF( (vector_format) n);
// 	saveVectorDialog->setFilters(filters);

	// Expression Export
	saveExpressionFilters.clear();
	for (filter = aten.filters(Tree::ExpressionExport); filter != NULL; filter = filter->next)
	{
		if (!saveExpressionFilters.isEmpty()) saveExpressionFilters += ";;";
		saveExpressionFilters += filter->description();
	}
	// Check for empty filters list
	ui.actionFileSaveExpression->setEnabled(!saveExpressionFilters.isEmpty());

	// Grid import
	loadGridFilters.clear();
	loadGridFilters += "All files (*)";
	for (filter = aten.filters(Tree::GridImport); filter != NULL; filter = filter->next)
	{
		loadGridFilters += ";;";
		loadGridFilters += filter->description();
	}
	gui.gridsWindow->ui.actionGridLoad->setEnabled(!loadGridFilters.isEmpty());

	// Create open script dialog
	loadScriptFilters.clear();
	loadScriptFilters += "All files (*)";


	msg.exit("AtenForm::createDialogFilters");
}

// Set controls
void AtenForm::setControls()
{
	msg.enter("AtenForm::setControls");
	// Set correct Atom::DrawStyle on toolbar
	switch (prefs.renderStyle())
	{
		case (Atom::StickStyle): ui.actionStyleStick->setChecked(TRUE); break;
		case (Atom::TubeStyle): ui.actionStyleTube->setChecked(TRUE); break;
		case (Atom::SphereStyle): ui.actionStyleSphere->setChecked(TRUE); break;
		case (Atom::ScaledStyle): ui.actionStyleScaled->setChecked(TRUE); break;
		case (Atom::IndividualStyle): ui.actionStyleIndividual->setChecked(TRUE); break;
	}
	// Set some menu items
	prefs.hasPerspective() ? ui.actionViewPerspective->setChecked(TRUE) : ui.actionViewOrthographic->setChecked(TRUE);
	// Set correct colour scheme menuitem
	switch (prefs.colourScheme())
	{
		case (Prefs::ElementScheme): ui.actionSchemeElement->setChecked(TRUE); break;
		case (Prefs::ChargeScheme): ui.actionSchemeCharge->setChecked(TRUE); break;
		case (Prefs::ForceScheme): ui.actionSchemeForce->setChecked(TRUE); break;
		//case (Prefs::VelocityScheme): ui.actionSchemeVelocity->setChecked(TRUE); break;
	}
	// Set controls on Bond toolbar 
	bondToleranceSpin_->setValue(prefs.bondTolerance());
	// Set the initial configuration of the splitter
	ui.MainSplitter->setSizes( QList<int>() << 500 << 64 );
	msg.exit("AtenForm::setControls");
}

