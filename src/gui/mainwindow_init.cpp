/*
	*** Qt user interface initialisation functions
	*** src/gui/mainwindow_init.cpp
	Copyright T. Youngs 2007-2015

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

#include "main/version.h"
#include "gui/mainwindow.h"
#include "gui/build.h"
#include "gui/geometry.h"
#include "gui/position.h"
#include "gui/transform.h"
#include "main/aten.h"
#include <QtWidgets/QMessageBox>

// Finalise GUI
void AtenWindow::finaliseUi()
{
	Messenger::enter("AtenWindow::finaliseUi");
	int n;

	// Set up recent files list (create all actions first)
	for (n=0; n<MAXRECENTFILES; n++)
	{
		actionRecentFile[n] = new QAction(this);
		actionRecentFile[n]->setVisible(false);
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

	// Set correct Atom::DrawStyle on toolbar
	switch (prefs.renderStyle())
	{
		case (Prefs::StickStyle):
			ui.actionStyleStick->setChecked(true);
			break;
		case (Prefs::TubeStyle):
			ui.actionStyleTube->setChecked(true);
			break;
		case (Prefs::SphereStyle):
			ui.actionStyleSphere->setChecked(true);
			break;
		case (Prefs::ScaledStyle):
			ui.actionStyleScaled->setChecked(true);
			break;
		case (Prefs::IndividualStyle):
			ui.actionStyleIndividual->setChecked(true);
			break;
		default:
			break;
	}

	// Create master group for buttons that change user action modes
	uaDummyButton_ = new QToolButton(this);
	uaDummyButton_->setCheckable(true);
	uaDummyButton_->setVisible(false);
	uaButtons_.addButton(uaDummyButton_);
	// -- From Build Dock Widget
	uaButtons_.addButton(buildWidget->ui.DrawAtomButton, UserAction::DrawAtomAction);
	uaButtons_.addButton(buildWidget->ui.DrawChainButton, UserAction::DrawChainAction);
	uaButtons_.addButton(buildWidget->ui.DrawFragmentButton, UserAction::DrawFragmentAction);
	uaButtons_.addButton(buildWidget->ui.DrawDeleteAtomButton, UserAction::DrawDeleteAction);
	uaButtons_.addButton(buildWidget->ui.DrawTransmuteButton, UserAction::DrawTransmuteAction);
	uaButtons_.addButton(buildWidget->ui.DrawAddHButton, UserAction::DrawAddHydrogenAction);
	uaButtons_.addButton(buildWidget->ui.DrawGrowButton, UserAction::DrawGrowAtomAction);
	uaButtons_.addButton(buildWidget->ui.DrawSingleBondButton, UserAction::DrawBondSingleAction);
	uaButtons_.addButton(buildWidget->ui.DrawDoubleBondButton, UserAction::DrawBondDoubleAction);
	uaButtons_.addButton(buildWidget->ui.DrawTripleBondButton, UserAction::DrawBondTripleAction);
	uaButtons_.addButton(buildWidget->ui.DrawDeleteBondButton, UserAction::DrawDeleteBondAction);
	// -- From Geometry Dock Widget
	uaButtons_.addButton(geometryWidget->ui.MeasureDistanceButton, UserAction::MeasureDistanceAction);
	uaButtons_.addButton(geometryWidget->ui.MeasureAngleButton, UserAction::MeasureAngleAction);
	uaButtons_.addButton(geometryWidget->ui.MeasureTorsionButton, UserAction::MeasureTorsionAction);
	// -- From Position Dock Widget
	uaButtons_.addButton(positionWidget->ui.ShiftPickVectorButton, UserAction::ShiftPickVectorAction);
	// -- From Transform Dock Widget
	uaButtons_.addButton(transformWidget->ui.TransformPickAButton, UserAction::TransformPickAAction);
	uaButtons_.addButton(transformWidget->ui.TransformPickBButton, UserAction::TransformPickBAction);
	uaButtons_.addButton(transformWidget->ui.TransformPickCButton, UserAction::TransformPickCAction);
	uaButtons_.addButton(transformWidget->ui.ConvertSourcePickAButton, UserAction::ConvertSourcePickAAction);
	uaButtons_.addButton(transformWidget->ui.ConvertSourcePickBButton, UserAction::ConvertSourcePickBAction);
	uaButtons_.addButton(transformWidget->ui.ConvertSourcePickCButton, UserAction::ConvertSourcePickCAction);
	uaButtons_.addButton(transformWidget->ui.ConvertTargetPickAButton, UserAction::ConvertTargetPickAAction);
	uaButtons_.addButton(transformWidget->ui.ConvertTargetPickBButton, UserAction::ConvertTargetPickBAction);
	uaButtons_.addButton(transformWidget->ui.ConvertTargetPickCButton, UserAction::ConvertTargetPickCAction);
	
	// Connect buttonPressed signal of button group to our handler
	QObject::connect(&uaButtons_, SIGNAL(buttonClicked(int)), this, SLOT(uaButtonClicked(int)));

	/*
	// Statusbar
	*/
	// Fix up the statusbar with a single big frame and no size grip
	ui.MainWindowStatusBar->setSizeGripEnabled(false);
	QFrame *frame = new QFrame(this);
	ui.MainWindowStatusBar->addPermanentWidget(frame,1);
	// Message label
	QHBoxLayout *lablayout = new QHBoxLayout(frame);
	messageLabel_ = new QLabel(this);
	messageLabel_->setTextFormat(Qt::RichText);
	messageLabel_->setWordWrap(true);
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

	Messenger::exit("AtenWindow::finaliseUi");
}

// Set controls
void AtenWindow::setControls()
{
	Messenger::enter("AtenWindow::setControls");
	
	// Set correct Atom::DrawStyle on toolbar
	setActiveStyleAction(prefs.renderStyle());

	// Set view perspective/orthographic
	prefs.hasPerspective() ? ui.actionViewPerspective->setChecked(true) : ui.actionViewOrthographic->setChecked(true);

	// Set correct colour scheme menuitem
	setActiveSchemeAction(prefs.colourScheme());

	Messenger::exit("AtenWindow::setControls");
}

// Update and show
void AtenWindow::updateAndShow()
{
	// Display message box warning if there was a filter load error
	if (aten_.nFilterPrograms() == 0)
	{
		QMessageBox::warning(NULL, "Aten", "Filters could not be found.\nNo import/export will be possible.\nSet the environment variable ATENDATA to point to Aten's data directory (e.g. 'export ATENDATA=/usr/local/aten/data'), or run with --atendata <dir>.\n", QMessageBox::Ok, QMessageBox::Ok);
	}
	else if (aten_.failedFilters().count() > 0)
	{
		// Construct the messagebox text
		QString text("One or more filters could not be loaded properly on startup.\nCheck shell output or run Settings->Reload Filters to diagnose the problem.\nFilters with errors were:\n");
		for (int n=0; n<aten_.failedFilters().count(); ++n)
		{
			text += "\t";
			text += aten_.failedFilters().at(n) + "\n";
		}
		QMessageBox::warning(NULL, "Aten", text, QMessageBox::Ok, QMessageBox::Ok);
	}

	// Show the window
	show();
}
