/*
	*** Qt user interface initialisation functions
	*** src/gui/mainwindow_init.cpp
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

#include "parse/filter.h"
#include "aten/aten.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"

// Finalise GUI
void AtenForm::finaliseUi()
{
	msg.enter("AtenForm::finaliseUi");
	Filter *f;
	int n;
	QStringList filters;

	// Set the title of the main window to reflect the version
	char title[64];
	sprintf(title, "Aten (%s)", ATENVERSION);
	setWindowTitle(title);

	// Initialise application name, organisation and author
	QCoreApplication::setOrganizationDomain("www.projectaten.org");
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
	ui.DrawToolbar->setVisible(FALSE);
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

	// Add permanent statusbar widgets
	statusLabel = new QLabel(this,0);
	ui.MainWindowStatusBar->addPermanentWidget(statusLabel,0);
	statusLabel->setFrameShape(QFrame::NoFrame);
	statusLabel->setFrameShadow(QFrame::Plain);
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

	// Create open model dialog
	loadModelDialog = new QFileDialog(this);
	loadModelDialog->setFileMode(QFileDialog::ExistingFiles);
	loadModelDialog->setDirectory(aten.workDir());
	loadModelDialog->setWindowTitle("Open Model(s)");
	filters << "All files (*)";
	for (f = aten.filters(Filter::ModelImport); f != NULL; f = f->next) filters << f->description();
	if (filters.empty())
	{
		ui.actionFileOpen->setEnabled(FALSE);
		ui.RecentMenu->setEnabled(FALSE);
	}
	else loadModelDialog->setFilters(filters);

	// Create open trajectory dialog
	loadTrajectoryDialog = new QFileDialog(this);
	loadTrajectoryDialog->setFileMode(QFileDialog::ExistingFile);
	loadTrajectoryDialog->setDirectory(aten.workDir());
	loadTrajectoryDialog->setWindowTitle("Add Trajectory");
	filters.clear();
	filters << "All files (*)";
	for (f = aten.filters(Filter::TrajectoryImport); f != NULL; f = f->next) filters << f->description();
	loadTrajectoryDialog->setFilters(filters);

	// Create save model dialog
	saveModelDialog = new QFileDialog(this);
	saveModelDialog->setWindowTitle("Save Model");
	saveModelDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveModelDialog->setDirectory(aten.workDir());
	saveModelDialog->setConfirmOverwrite(TRUE);
	saveModelDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (f = aten.filters(Filter::ModelExport); f != NULL; f = f->next) filters << f->description();
	// Check for empty filters list (causes crash)
	if (filters.empty())
	{
		ui.actionFileSave->setEnabled(FALSE);
		ui.actionFileSaveAs->setEnabled(FALSE);
	}
	else saveModelDialog->setFilters(filters);

	// Create save image dialog
	saveBitmapDialog = new QFileDialog(this);
	saveBitmapDialog->setWindowTitle("Save Image");
	saveBitmapDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveBitmapDialog->setDirectory(aten.workDir());
	saveBitmapDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (n=0; n < BIF_NITEMS; n++) filters << filter_from_BIF( (bitmap_format) n);
	saveBitmapDialog->setFilters(filters);

	// Create save vector dialog
	saveVectorDialog = new QFileDialog(this);
	saveVectorDialog->setWindowTitle("Save Vector");
	saveVectorDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveVectorDialog->setDirectory(aten.workDir());
	saveVectorDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (n=0; n < VIF_NITEMS; n++) filters << filter_from_VIF( (vector_format) n);
	saveVectorDialog->setFilters(filters);

	// Create save expression dialog
	saveExpressionDialog = new QFileDialog(this);
	saveExpressionDialog->setWindowTitle("Save Vector");
	saveExpressionDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveExpressionDialog->setDirectory(aten.workDir());
	saveExpressionDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (f = aten.filters(Filter::ExpressionExport); f != NULL; f = f->next) filters << f->description();
	// Check for empty filters list (causes crash)
	if (filters.empty()) ui.actionFileSaveExpression->setEnabled(FALSE);
	else saveExpressionDialog->setFilters(filters);

	// Create open script dialog
	loadScriptDialog = new QFileDialog(this);
	loadScriptDialog->setWindowTitle("Open Script");
	loadScriptDialog->setDirectory(aten.workDir());
	loadScriptDialog->setFileMode(QFileDialog::ExistingFile);
	filters.clear();
	filters << "All files (*)";
	loadScriptDialog->setFilters(filters);

	msg.exit("AtenForm::finaliseUi");
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

