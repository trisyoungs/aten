/*
	*** Qt user interface initialisation functions
	*** src/gui/mainwindow_init.cpp
	Copyright T. Youngs 2007-2011

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
#include "main/version.h"
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
	Dnchar title;
	title.sprintf("Aten (%s)", ATENVERSION);
	setWindowTitle(title.get());

	// Set up recent files list (create all actions first)
	for (n=0; n<MAXRECENTFILES; n++)
	{
		actionRecentFile[n] = new QAction(this);
		actionRecentFile[n]->setVisible(FALSE);
		QObject::connect(actionRecentFile[n], SIGNAL(triggered()), this, SLOT(loadRecent()));
		ui.RecentMenu->addAction(actionRecentFile[n]);
	}

	// Create QActionGroup for draw styles
	QActionGroup *group = new QActionGroup(this);
	actionGroups_.add(group);
	group->addAction(ui.actionStyleStick);
	group->addAction(ui.actionStyleTube);
	group->addAction(ui.actionStyleSphere);
	group->addAction(ui.actionStyleScaled);
	group->addAction(ui.actionStyleIndividual);

	// Create QActionGroup for colour schemes
	group = new QActionGroup(this);
	actionGroups_.add(group);
	group->addAction(ui.actionSchemeElement);
	group->addAction(ui.actionSchemeCharge);
	group->addAction(ui.actionSchemeForce);
	group->addAction(ui.actionSchemeVelocity);
	group->addAction(ui.actionSchemeCustom);

	// Create QActionGroup for Mouse toolbar
	group = new QActionGroup(this);
	actionGroups_.add(group);
	group->addAction(ui.actionMouseInteract);
	group->addAction(ui.actionMouseRotate);
	group->addAction(ui.actionMouseTranslate);

	// Hide some toolbars initially
	ui.BondToolbar->setVisible(FALSE);
	ui.MeasureToolbar->setVisible(FALSE);
	ui.TrajectoryToolbar->setVisible(FALSE);

	// Add extra widgets to trajectory toolbar
	trajectorySlider_ = new QSlider(Qt::Horizontal, ui.TrajectoryToolbar);
	ui.TrajectoryToolbar->addWidget(trajectorySlider_);
	QObject::connect(trajectorySlider_, SIGNAL(sliderMoved(int)), this, SLOT(trajectorySlider_sliderMoved(int)));
	trajectorySpin_ = new QSpinBox(ui.TrajectoryToolbar);
	ui.TrajectoryToolbar->addWidget(trajectorySpin_);
	QObject::connect(trajectorySpin_, SIGNAL(valueChanged(int)), this, SLOT(trajectorySpin_valueChanged(int)));

	// Create QActionGroup for perspective / orthographic views
	group = new QActionGroup(this);
	actionGroups_.add(group);
	group->addAction(ui.actionViewOrthographic);
	group->addAction(ui.actionViewPerspective);

	// Create QActionGroup for model / trajectory render source
	group = new QActionGroup(this);
	actionGroups_.add(group);
	group->addAction(ui.actionViewModel);
	group->addAction(ui.actionViewTrajectory);

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
		case (Atom::StickStyle):
			ui.actionStyleStick->setChecked(TRUE);
			break;
		case (Atom::TubeStyle):
			ui.actionStyleTube->setChecked(TRUE);
			break;
		case (Atom::SphereStyle):
			ui.actionStyleSphere->setChecked(TRUE);
			break;
		case (Atom::ScaledStyle):
			ui.actionStyleScaled->setChecked(TRUE);
			break;
		case (Atom::IndividualStyle):
			ui.actionStyleIndividual->setChecked(TRUE);
			break;
		default:
			break;
	}

	// Add bond tolerance spinbox to Bond Toolbar
	bondToleranceSpin_ = new QDoubleSpinBox(ui.BondToolbar);
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
	uaGroup->addAction(ui.actionDrawFragment);
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

	/*
	// Statusbar
	*/
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
	infolayout->setSizeConstraint(QLayout::SetMaximumSize);
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
	QGridLayout *layout = new QGridLayout(progressIndicator);
	layout->setMargin(0);
	progressBar = new QProgressBar(this);
	progressBar->setMaximumWidth(100);
	progressTitle = new QLabel(this,0);
	progressTitle->setFont(font);
	progressEta = new QLabel(this,0);
	progressEta->setFont(font);
	progressButton = new QPushButton(this);
	progressButton->setText("Cancel");
	QObject::connect(progressButton, SIGNAL(clicked()), this, SLOT(progressCancel()));
	layout->addWidget(progressTitle, 0,0,1,2, Qt::AlignHCenter);
	layout->addWidget(progressBar, 1,0,1,1);
	layout->addWidget(progressButton, 1,1,1,1);
	layout->addWidget(progressEta, 2,0,1,2, Qt::AlignHCenter);
	progressIndicator->setVisible(FALSE);
	ui.MainWindowStatusBar->insertPermanentWidget(0,progressIndicator,0);

	// Create glyph actions for Selection (atom context) menu
	QMenu *menu = new QMenu(this);
	for (int n=0; n<Glyph::nGlyphTypes; ++n)
	{
		createGlyphActions[n] = menu->addAction(Glyph::glyphTypeName( (Glyph::GlyphType) n));
		QObject::connect(createGlyphActions[n], SIGNAL(triggered()), this, SLOT(createGlyph()));
	}
	ui.actionCreateGlyph->setMenu(menu);

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
	Refitem<Tree,int> *ri;
	int n;

	// Model Import
	loadModelFilters.clear();
	loadModelFilters += "All files (*)";
	for (ri = aten.filters(FilterData::ModelImport); ri != NULL; ri = ri->next)
	{
		loadModelFilters += ";;";
		loadModelFilters += ri->item->filter.description();
	}
	ui.actionFileOpen->setEnabled(!loadModelFilters.isEmpty());
	ui.RecentMenu->setEnabled(!loadModelFilters.isEmpty());

	// Trajectory Import
	loadTrajectoryFilters.clear();
	loadTrajectoryFilters += "All files (*)";
	for (ri= aten.filters(FilterData::TrajectoryImport); ri != NULL; ri = ri->next)
	{
		loadTrajectoryFilters += ";;";
		loadTrajectoryFilters += ri->item->filter.description();
	}
	ui.actionFileAddTrajectory->setEnabled(!loadTrajectoryFilters.isEmpty());

	// Model Export
	saveModelFilters.clear();
	for (ri = aten.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
	{
		if (!saveModelFilters.isEmpty()) saveModelFilters += ";;";
		saveModelFilters += ri->item->filter.description();
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
	for (ri = aten.filters(FilterData::ExpressionExport); ri != NULL; ri = ri->next)
	{
		if (!saveExpressionFilters.isEmpty()) saveExpressionFilters += ";;";
		saveExpressionFilters += ri->item->filter.description();
	}
	// Check for empty filters list
	ui.actionFileSaveExpression->setEnabled(!saveExpressionFilters.isEmpty());

	// Grid import
	loadGridFilters.clear();
	loadGridFilters += "All files (*)";
	for (ri = aten.filters(FilterData::GridImport); ri != NULL; ri = ri->next)
	{
		loadGridFilters += ";;";
		loadGridFilters += ri->item->filter.description();
	}
	gui.gridsWidget->ui.actionGridLoad->setEnabled(!loadGridFilters.isEmpty());

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
		case (Atom::StickStyle):
			ui.actionStyleStick->setChecked(TRUE);
			break;
		case (Atom::TubeStyle):
			ui.actionStyleTube->setChecked(TRUE);
			break;
		case (Atom::SphereStyle):
			ui.actionStyleSphere->setChecked(TRUE);
			break;
		case (Atom::ScaledStyle):
			ui.actionStyleScaled->setChecked(TRUE);
			break;
		case (Atom::IndividualStyle):
			ui.actionStyleIndividual->setChecked(TRUE);
			break;
		default:
			break;
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
		default:
			break;
	}
	// Set controls on Bond toolbar 
	bondToleranceSpin_->setValue(prefs.bondTolerance());

	msg.exit("AtenForm::setControls");
}

