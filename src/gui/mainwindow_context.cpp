/*
	*** Main Window - Context Menu Functions
	*** src/gui/mainwindow_context.cpp
	Copyright T. Youngs 2007-2016

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
#include "gui/mainwindow.h"
#include "gui/popupcolour.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Create context menu and setup actions
void AtenWindow::createContextMenu()
{
	QMenu* menu;
	QAction* action;

	// Create style submenu
	menu = contextMenu_.addMenu("&Style");
	for (int n=0; n<Prefs::OwnStyle; ++n)
	{
		action = menu->addAction(Prefs::drawStyle( (Prefs::DrawStyle) n ));
		action->setData(n);
		connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuSetAtomStyle(bool)));
	}

	// Create atom label submenu
	menu = contextMenu_.addMenu("&Label");
	for (int n=0; n<Atom::nLabelTypes; ++n)
	{
		action = menu->addAction(Atom::atomLabel( (Atom::AtomLabel) n ));
		connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuSetAtomLabel(bool)));
	}
	menu->addSeparator();
	action = menu->addAction("Clear");
	connect(action, SIGNAL(triggered(bool)), ui.SelectionLabelClearButton, SLOT(click()));

	// Create atom order submenu
	menu = contextMenu_.addMenu("&Order");
	action = menu->addAction("Shift Up");
	connect(action, SIGNAL(triggered(bool)), ui.AtomsShiftUpButton, SLOT(click()));
	action = menu->addAction("Shift Down");
	connect(action, SIGNAL(triggered(bool)), ui.AtomsShiftDownButton, SLOT(click()));
	action = menu->addAction("Move to Start");
	connect(action, SIGNAL(triggered(bool)), ui.AtomsMoveToStartButton, SLOT(click()));
	action = menu->addAction("Move to End");
	connect(action, SIGNAL(triggered(bool)), ui.AtomsMoveToEndButton, SLOT(click()));
	menu->addSeparator();
	action = menu->addAction("Reorder");
	connect(action, SIGNAL(triggered(bool)), ui.ToolsOrderReorderButton, SLOT(click()));

	// Appearance
	contextMenu_.addSeparator();
// 	menu = contextMenu_.addMenu("Set Colour...");
// 	menu->addAction(new ColourPopup(*this, NULL));
// 	connect(action, SIGNAL(triggered(bool)), ui.SelectionLabelClearButton, SLOT(click()));
	action = contextMenu_.addAction("Reset Colour to Element");
	connect(action, SIGNAL(triggered(bool)), ui.SelectionAppearanceResetToElementButton, SLOT(click()));
	action = contextMenu_.addAction("Hide");
	connect(action, SIGNAL(triggered(bool)), ui.SelectionAppearanceHideButton, SLOT(click()));

	// Position
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("Fix Position");
	connect(action, SIGNAL(triggered(bool)), ui.SelectionPositionFixButton, SLOT(click()));
	action = contextMenu_.addAction("Free Position");
	connect(action, SIGNAL(triggered(bool)), ui.SelectionPositionFreeButton, SLOT(click()));

	// Edit
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("Cut");
	connect(action, SIGNAL(triggered(bool)), ui.HomeEditCutButton, SLOT(click()));
	action = contextMenu_.addAction("Copy");
	connect(action, SIGNAL(triggered(bool)), ui.HomeEditCopyButton, SLOT(click()));
	action = contextMenu_.addAction("Paste");
	connect(action, SIGNAL(triggered(bool)), ui.HomeEditPasteButton, SLOT(click()));
	action = contextMenu_.addAction("Delete");
	connect(action, SIGNAL(triggered(bool)), ui.HomeEditDeleteButton, SLOT(click()));

	// Probe
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("Probe");
	connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuProbeAtom(bool)));

	// Create
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("Create Fragment");
	connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuCreateFragment(bool)));

// 	// (De)Activate glyph menu items based on number of atoms selected
// 	for (int gt=0; gt<Glyph::nGlyphTypes; ++gt) createGlyphActions[gt]->setEnabled( Glyph::nGlyphData( (Glyph::GlyphType) gt) == nSelected);
}

// Show the modelview context menu
void AtenWindow::callContextMenu(Atom* atomUnderMouse, int x, int y)
{
	// If we are not currently editable_, return now
	if (!editable_) return;

	// If there is no atom under the mouse, then exit
	contextAtom_ = atomUnderMouse;
	if (contextAtom_ == NULL) return;

	// If the atom under the mouse is selected, just run the popup. If it is not selected, deselect everything else and select it
	QPoint pos(x,y);
//	printf("AtomPopup: model %li, undermouse = %li, nselected = %i\n", viewTarget, target, viewTarget->nSelected());
	Model* viewTarget = aten_.currentModelOrFrame();
	if (!viewTarget) return;

	if (!contextAtom_->isSelected())
	{
		viewTarget->beginUndoState("Select atom (Context Menu)");
		viewTarget->selectNone();
		viewTarget->selectAtom(contextAtom_);
		viewTarget->endUndoState();

		updateWidgets(AtenWindow::MainViewTarget);
	}

	// Call the context menu
	contextMenu_.exec(pos);
}

// Set atom style
void AtenWindow::contextMenuSetAtomStyle(bool checked)
{
	// Cast the sender into a QAction
	QAction* action = qobject_cast<QAction*> (sender());
	if (!action) return;

	CommandNode::run(Commands::AtomStyle, "c", qPrintable(action->text()));

	updateWidgets(AtenWindow::MainViewTarget);
}

// Set atom label
void AtenWindow::contextMenuSetAtomLabel(bool checked)
{
	// Cast the sender into a QAction
	QAction* action = qobject_cast<QAction*> (sender());
	if (!action) return;

	CommandNode::run(Commands::Label, "c", qPrintable(action->text()));

	updateWidgets(AtenWindow::MainViewTarget);
}

// Probe atom information
void AtenWindow::contextMenuProbeAtom(bool checked)
{
	if (contextAtom_ != NULL) contextAtom_->print();
}

/*
// Reset atom custom colour to element colour
void AtenWindow::on_actionAtomColourSet_triggered(bool checked)
{
	QColor oldcol, newcol;
	// Get colour of clicked atom and convert into a QColor
	if (contextAtom_ != NULL) oldcol.setRgbF( contextAtom_->colour()[0], contextAtom_->colour()[1], contextAtom_->colour()[2], contextAtom_->colour()[3] );
	else oldcol.setRgbF(1.0, 1.0, 1.0, 1.0);
		
	// Request a colour dialog
	bool ok = false;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;

	// Store new colour
	CommandNode::run(Commands::ColourAtoms, "dddd", newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	contextAtom_ = NULL;

	// Set colour scheme menu option automatically if necessary
	if (prefs.colourScheme() != Prefs::OwnScheme) TMenuButton::setGroupButtonChecked("Colourschemes", "Own");
	Messenger::print("Colouring scheme changed to 'own'.");

	updateWidgets(AtenWindow::MainViewTarget);
}*/

// Create fragment from current selection
void AtenWindow::contextMenuCreateFragment(bool checked)
{
	Model* viewTarget = aten_.currentModelOrFrame();
	aten_.addFragmentFromSelection(viewTarget, "Selections");

	// Update
	updateWidgets(AtenWindow::BuildPanelTarget);
	ReturnValue rv;
	ui.BuildDrawFragmentButton->callPopupMethod("updateFragments", rv);
}

void AtenWindow::createGlyph()
{
	// Cast sending QAction and grab filename
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenWindow::loadRecent - Sender was not a QAction.\n");
		return;
	}
	// Which action was it?
	int n;
	for (n=0; n<Glyph::nGlyphTypes; ++n) if (createGlyphActions[n] == action) break;
	if (n == Glyph::nGlyphTypes)
	{
		printf("Internal Error - Failed to 'cast' action into glyph type.\n");
		return;
	}
	Glyph::GlyphType gt = (Glyph::GlyphType) n;
	// Create glyph in model
	CommandNode::run(Commands::NewGlyph, "c", Glyph::glyphType(gt));
	// Set data to atom selection
	Model* viewTarget = aten_.currentModelOrFrame();
	n = 1;
	for (RefListItem<Atom,int>* ri = viewTarget->selection(); ri != NULL; ri = ri->next)
	{
		CommandNode::run(Commands::GlyphAtomR, "ii", n, ri->item->id()+1);
		n++;
	}
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GlyphsTarget);
}

