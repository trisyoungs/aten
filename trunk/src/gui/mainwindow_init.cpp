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
#include "base/master.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include <QtGui/QFileDialog>
#include <QtCore/QSettings>

// Finalise GUI
void AtenForm::finaliseUi()
{
	dbgBegin(Debug::Calls,"AtenForm::finaliseUi");
	Filter *f;
	int n;
	char temp[256];
	QStringList filters;

	// Set the title of the main window to reflect the version
	setWindowTitle("Aten (0.98)");

	// Initialise application name, organisation and author, and create settings structure
	QCoreApplication::setOrganizationDomain("www.projectaten.org");
	QCoreApplication::setApplicationName("Aten");
	settings_ = new QSettings;

	// Set up recent files list (create all actions first)
	for (n=0; n<MAXRECENTFILES; n++)
	{
		actionRecentFile[n] = new QAction(this);
		actionRecentFile[n]->setVisible(FALSE);
		QObject::connect(actionRecentFile[n], SIGNAL(triggered()), this, SLOT(loadRecent()));
		ui.RecentMenu->addAction(actionRecentFile[n]);
	}
	// -- Now populate list
	for (n=0; n<MAXRECENTFILES; n++)
	{
		// Construct settings value to search for
		strcpy(temp,"RecentFile");
		strcat(temp,itoa(n));
		if (settings_->contains(temp)) addRecent(qPrintable(settings_->value(temp).toString()));
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

	// Create QActionGroup for colour schemes
	QActionGroup *schemeGroup = new QActionGroup(this);
	schemeGroup->addAction(ui.actionSchemeElement);
	schemeGroup->addAction(ui.actionSchemeCharge);
	schemeGroup->addAction(ui.actionSchemeForce);

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
	commandEdit_ = new QLineEdit(this);
	ui.CommandToolBar->addWidget(commandEdit_);
	QObject::connect(commandEdit_, SIGNAL(returnPressed()), this, SLOT(executeCommand()));

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
	dummyButton = new QPushButton(this);
	dummyButton->setCheckable(TRUE);
	dummyButton->setChecked(TRUE);
	dummyButton->setVisible(FALSE);
	uaGroup->addButton(dummyButton);

	// Create a subgroup for the element select buttons
	QButtonGroup *elementGroup = new QButtonGroup(this);
	elementGroup->addButton(ui.ElementHButton);
	elementGroup->addButton(ui.ElementCButton);
	elementGroup->addButton(ui.ElementNButton);
	elementGroup->addButton(ui.ElementUserButton);

	// Store widgetstack buttons
	stackButtons_[SP_ATOMS] = ui.ShowAtomPageButton;
	stackButtons_[SP_EDIT] = ui.ShowEditPageButton;
	stackButtons_[SP_TRANSFORM] = ui.ShowTransformPageButton;
	stackButtons_[SP_POSITION] = ui.ShowPositionPageButton;
	stackButtons_[SP_CELLDEFINE] = ui.ShowCellDefinePageButton;
	stackButtons_[SP_CELLMANIPULATE] = ui.ShowCellManipulatePageButton;
	stackButtons_[SP_MINIMISER] = ui.ShowMinimiserPageButton;
	stackButtons_[SP_DISORDER] = ui.ShowDisorderPageButton;
	stackButtons_[SP_FORCEFIELD] = ui.ShowForcefieldsPageButton;
	stackButtons_[SP_GRID] = ui.ShowGridsPageButton;
	stackButtons_[SP_ANALYSE] = ui.ShowAnalysePageButton;

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

	// Create dialog array
	dialog = new QFileDialog*[Filter::nFilterTypes];

	// Create open model dialog
	dialog[Filter::ModelImport] = new QFileDialog(this);
	dialog[Filter::ModelImport]->setFileMode(QFileDialog::ExistingFiles);
	dialog[Filter::ModelImport]->setDirectory(master.workDir.get());
	dialog[Filter::ModelImport]->setWindowTitle("Open Model(s)");
	filters << "All files (*)";
	for (f = master.filters(Filter::ModelImport); f != NULL; f = f->next) filters << f->description();
	if (filters.empty())
	{
		ui.actionFileOpen->setEnabled(FALSE);
		ui.RecentMenu->setEnabled(FALSE);
	}
	else dialog[Filter::ModelImport]->setFilters(filters);

	// Create open trajectory dialog
	dialog[Filter::TrajectoryImport] = new QFileDialog(this);
	dialog[Filter::TrajectoryImport]->setFileMode(QFileDialog::ExistingFile);
	dialog[Filter::TrajectoryImport]->setDirectory(master.workDir.get());
	dialog[Filter::TrajectoryImport]->setWindowTitle("Add Trajectory");
	filters.clear();
	filters << "All files (*)";
	for (f = master.filters(Filter::TrajectoryImport); f != NULL; f = f->next) filters << f->description();
	dialog[Filter::TrajectoryImport]->setFilters(filters);

	// Create save model dialog
	dialog[Filter::ModelExport] = new QFileDialog(this);
	dialog[Filter::ModelExport]->setWindowTitle("Save Model");
	dialog[Filter::ModelExport]->setAcceptMode(QFileDialog::AcceptSave);
	dialog[Filter::ModelExport]->setDirectory(master.workDir.get());
	dialog[Filter::ModelExport]->setConfirmOverwrite(TRUE);
	dialog[Filter::ModelExport]->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (f = master.filters(Filter::ModelExport); f != NULL; f = f->next) filters << f->description();
	// Check for empty filters list (causes crash)
	if (filters.empty())
	{
		ui.actionFileSave->setEnabled(FALSE);
		ui.actionFileSaveAs->setEnabled(FALSE);
	}
	else dialog[Filter::ModelExport]->setFilters(filters);

	// Create save image dialog
	saveBitmapDialog = new QFileDialog(this);
	saveBitmapDialog->setWindowTitle("Save Image");
	saveBitmapDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveBitmapDialog->setDirectory(master.workDir.get());
	saveBitmapDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (n=0; n < BIF_NITEMS; n++) filters << filter_from_BIF( (bitmap_format) n);
	saveBitmapDialog->setFilters(filters);

	// Create open forcefield dialog
	openForcefieldDialog = new QFileDialog(this);
	openForcefieldDialog->setFileMode(QFileDialog::ExistingFile);
	openForcefieldDialog->setDirectory(master.dataDir.get());
	openForcefieldDialog->setWindowTitle("Open Forcefield");
	filters.clear();
	filters << "All files (*)";
	filters << "Forcefield Files (*.ff)";
	openForcefieldDialog->setFilters(filters);

	// Create save forcefield dialog
	saveForcefieldDialog = new QFileDialog(this);
	saveForcefieldDialog->setWindowTitle("Save Forcefield");
	saveForcefieldDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveForcefieldDialog->setDirectory(master.workDir.get());
	saveForcefieldDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	filters << "All files (*)";
	filters << "Forcefield Files (*.ff)";
	saveForcefieldDialog->setFilters(filters);

	// Create save vector dialog
	saveVectorDialog = new QFileDialog(this);
	saveVectorDialog->setWindowTitle("Save Vector");
	saveVectorDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveVectorDialog->setDirectory(master.workDir.get());
	saveVectorDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (n=0; n < VIF_NITEMS; n++) filters << filter_from_VIF( (vector_format) n);
	saveVectorDialog->setFilters(filters);

	// Create save expression dialog
	dialog[Filter::ExpressionExport] = new QFileDialog(this);
	dialog[Filter::ExpressionExport]->setWindowTitle("Save Vector");
	dialog[Filter::ExpressionExport]->setAcceptMode(QFileDialog::AcceptSave);
	dialog[Filter::ExpressionExport]->setDirectory(master.workDir.get());
	dialog[Filter::ExpressionExport]->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	for (f = master.filters(Filter::ExpressionExport); f != NULL; f = f->next) filters << f->description();
	// Check for empty filters list (causes crash)
	if (filters.empty()) ui.actionFileSaveExpression->setEnabled(FALSE);
	else dialog[Filter::ExpressionExport]->setFilters(filters);

	// Create open grid dialog
	dialog[Filter::GridImport] = new QFileDialog(this);
	dialog[Filter::GridImport]->setWindowTitle("Open Grid");
	dialog[Filter::GridImport]->setDirectory(master.workDir.get());
	dialog[Filter::GridImport]->setFileMode(QFileDialog::ExistingFile);
	filters.clear();
	filters << "All files (*)";
	for (f = master.filters(Filter::GridImport); f != NULL; f = f->next) filters << f->description();
	if (filters.empty()) ui.actionFileOpenGrid->setEnabled(FALSE);
	else dialog[Filter::GridImport]->setFilters(filters);

	// Create open script dialog
	openScriptDialog = new QFileDialog(this);
	openScriptDialog->setWindowTitle("Open Script");
	openScriptDialog->setDirectory(master.workDir.get());
	openScriptDialog->setFileMode(QFileDialog::ExistingFile);
	filters.clear();
	filters << "All files (*)";
	openScriptDialog->setFilters(filters);

	dbgEnd(Debug::Calls,"AtenForm::finaliseUi");
}

// Set controls
void AtenForm::setControls()
{
	dbgBegin(Debug::Calls,"AtenForm::setControls");
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
	// Set controls on edit page
	ui.BondToleranceSpin->setValue(prefs.bondTolerance());
	// Set the initial configuration of the splitter
	ui.MainSplitter->setSizes( QList<int>() << 500 << 64 );
	dbgEnd(Debug::Calls,"AtenForm::setControls");
}

