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
#include "gui/build.h"
#include "gui/geometry.h"
#include "gui/position.h"
#include "gui/transform.h"
#include "gui/gui.h"
#include "gui/toolbox.h"
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

	// Populate QActionGroup for main toolbar selection actions
	uaSelectActions_ = new QActionGroup(this);
	uaSelectActions_->addAction(ui.actionSelectAtoms);
	uaSelectActions_->addAction(ui.actionSelectMolecules);
	uaSelectActions_->addAction(ui.actionSelectElement);
	uaSelectActions_->addAction(ui.actionNoAction);
	
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

	// Create QActionGroup for perspective / orthographic views
	group = new QActionGroup(this);
	actionGroups_.add(group);
	group->addAction(ui.actionViewOrthographic);
	group->addAction(ui.actionViewPerspective);

	// Create QActionGroup for model / trajectory render source
	group = new QActionGroup(this);
	actionGroups_.add(group);
	group->addAction(ui.actionTrajectoryModel);
	group->addAction(ui.actionTrajectoryFrames);

	// Add the previously-created QGLWidget to the main interface, and set up calls
	QVBoxLayout *vbox = new QVBoxLayout();
	vbox->setMargin(0);
	gui.mainCanvas()->setParent(this);
	gui.mainCanvas()->setMouseTracking(TRUE);
	gui.mainCanvas()->setFocusPolicy(Qt::StrongFocus);
	vbox->addWidget( (QWidget*) gui.mainCanvas() );
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

	// Create master group for buttons that change user action modes
	uaDummyButton_ = new QToolButton(this);
	uaDummyButton_->setCheckable(TRUE);
	uaDummyButton_->setVisible(FALSE);
	uaButtons_.addButton(uaDummyButton_);
	// -- From Build Dock Widget
	uaButtons_.addButton(gui.buildWidget->ui.DrawAtomButton, UserAction::DrawAtomAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawChainButton, UserAction::DrawChainAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawFragmentButton, UserAction::DrawFragmentAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawDeleteAtomButton, UserAction::DrawDeleteAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawTransmuteButton, UserAction::DrawTransmuteAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawAddHButton, UserAction::DrawAddHydrogenAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawSingleBondButton, UserAction::DrawBondSingleAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawDoubleBondButton, UserAction::DrawBondDoubleAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawTripleBondButton, UserAction::DrawBondTripleAction);
	uaButtons_.addButton(gui.buildWidget->ui.DrawDeleteBondButton, UserAction::DrawDeleteBondAction);
	// -- From Geometry Dock Widget
	uaButtons_.addButton(gui.geometryWidget->ui.MeasureDistanceButton, UserAction::MeasureDistanceAction);
	uaButtons_.addButton(gui.geometryWidget->ui.MeasureAngleButton, UserAction::MeasureAngleAction);
	uaButtons_.addButton(gui.geometryWidget->ui.MeasureTorsionButton, UserAction::MeasureTorsionAction);
	// -- From Position Dock Widget
	uaButtons_.addButton(gui.positionWidget->ui.ShiftPickVectorButton, UserAction::ShiftPickVectorAction);
	// -- From Transform Dock Widget
	uaButtons_.addButton(gui.transformWidget->ui.TransformPickAButton, UserAction::TransformPickAAction);
	uaButtons_.addButton(gui.transformWidget->ui.TransformPickBButton, UserAction::TransformPickBAction);
	uaButtons_.addButton(gui.transformWidget->ui.TransformPickCButton, UserAction::TransformPickCAction);
	uaButtons_.addButton(gui.transformWidget->ui.ConvertSourcePickAButton, UserAction::ConvertSourcePickAAction);
	uaButtons_.addButton(gui.transformWidget->ui.ConvertSourcePickBButton, UserAction::ConvertSourcePickBAction);
	uaButtons_.addButton(gui.transformWidget->ui.ConvertSourcePickCButton, UserAction::ConvertSourcePickCAction);
	uaButtons_.addButton(gui.transformWidget->ui.ConvertTargetPickAButton, UserAction::ConvertTargetPickAAction);
	uaButtons_.addButton(gui.transformWidget->ui.ConvertTargetPickBButton, UserAction::ConvertTargetPickBAction);
	uaButtons_.addButton(gui.transformWidget->ui.ConvertTargetPickCButton, UserAction::ConvertTargetPickCAction);
	
	// Connect buttonPressed signal of button group to our handler
	QObject::connect(&uaButtons_, SIGNAL(buttonClicked(int)), this, SLOT(uaButtonClicked(int)));

	/*
	// Statusbar
	*/
	// Fix up the statusbar with a single big frame and no size grip
	ui.MainWindowStatusBar->setSizeGripEnabled(FALSE);
	QFrame *frame = new QFrame(this);
	ui.MainWindowStatusBar->addPermanentWidget(frame,1);
	// Message label
	QHBoxLayout *lablayout = new QHBoxLayout(frame);
	messageLabel_ = new QLabel(this);
	messageLabel_->setTextFormat(Qt::RichText);
	messageLabel_->setWordWrap(TRUE);
	QFont font = messageLabel_->font();
	font.setPointSize(8);
	messageLabel_->setFont(font);
	lablayout->addWidget(messageLabel_, 100);
	QFrame *sep = new QFrame;
	sep->setFrameStyle(QFrame::VLine);
	lablayout->addWidget(sep,0);
	// Info labels
	QVBoxLayout *infolayout = new QVBoxLayout;
	infolayout->setSizeConstraint(QLayout::SetMaximumSize);
	infoLabel1_ = new QLabel(this);
	infoLabel1_->setFont(font);
	infolayout->addWidget(infoLabel1_);
	infoLabel2_ = new QLabel(this);
	infoLabel2_->setFont(font);
	infolayout->addWidget(infoLabel2_);
	lablayout->addLayout(infolayout,0);

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
	ui.actionTrajectoryOpen->setEnabled(!loadTrajectoryFilters.isEmpty());

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
	ui.actionSaveExpression->setEnabled(!saveExpressionFilters.isEmpty());

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
	setActiveStyleAction(prefs.renderStyle());

	// Set view perspective/orthographic
	prefs.hasPerspective() ? ui.actionViewPerspective->setChecked(TRUE) : ui.actionViewOrthographic->setChecked(TRUE);

	// Set correct colour scheme menuitem
	setActiveSchemeAction(prefs.colourScheme());

	msg.exit("AtenForm::setControls");
}

