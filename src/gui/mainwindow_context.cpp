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
#include <QWidgetAction>
#include <QInputDialog>

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
		action = menu->addAction(Atom::atomLabelNice( (Atom::AtomLabel) n ));
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
	connect(action, SIGNAL(triggered(bool)), ui.ToolsAtomsReorderButton, SLOT(click()));

	// Create selection submenu
	menu = contextMenu_.addMenu("S&elect...");
	action = menu->addAction("Similar &elements");
	connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuSelectElement(bool)));
	action = menu->addAction("Similar &atoms");
	connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuSelectSimilar(bool)));
	action = menu->addAction("&Fragment");
	connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuSelectFragment(bool)));
	contextMenu_.addSeparator();

	// Create typing submenu
	menu = contextMenu_.addMenu("&Type...");
	action = menu->addAction("Add/set name type");
	connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuTypeSetName(bool)));
	contextMenu_.addSeparator();

	// Appearance
	menu = contextMenu_.addMenu("Set Colour...");
	TColourWidget* colourWidget = new TColourWidget(menu);
	QWidgetAction* colourWidgetAction = new QWidgetAction(menu);
	colourWidgetAction->setDefaultWidget(colourWidget);
	menu->addAction(colourWidgetAction);
	connect(colourWidget, SIGNAL(colourChanged(QColor)), this, SLOT(contextMenuColourChanged(QColor)));
	action = contextMenu_.addAction("&Reset Colour to Element");
	connect(action, SIGNAL(triggered(bool)),  ui.SelectionAppearanceResetToElementButton, SLOT(click()));
	action = contextMenu_.addAction("&Hide");
	connect(action, SIGNAL(triggered(bool)), ui.SelectionAppearanceHideButton, SLOT(click()));
	// -- Re-map submenu
	menu = contextMenu_.addMenu("Re-&map elements...");
	for (int n=0; n<ElementMap::nZMapTypes; ++n)
	{
		action = menu->addAction(ElementMap::zMapType( (ElementMap::ZMapType) n ));
		connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuReMap(bool)));
	}

	// Position
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("Fi&x Position");
	connect(action, SIGNAL(triggered(bool)), ui.SelectionPositionFixButton, SLOT(click()));
	action = contextMenu_.addAction("Fr&ee Position");
	connect(action, SIGNAL(triggered(bool)), ui.SelectionPositionFreeButton, SLOT(click()));

	// Edit
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("&Cut");
	connect(action, SIGNAL(triggered(bool)), ui.HomeEditCutButton, SLOT(click()));
	action = contextMenu_.addAction("Cop&y");
	connect(action, SIGNAL(triggered(bool)), ui.HomeEditCopyButton, SLOT(click()));
	action = contextMenu_.addAction("&Paste");
	connect(action, SIGNAL(triggered(bool)), ui.HomeEditPasteButton, SLOT(click()));
	action = contextMenu_.addAction("&Delete");
	connect(action, SIGNAL(triggered(bool)), ui.HomeEditDeleteButton, SLOT(click()));

	// Probe
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("Pro&be");
	connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuProbeAtom(bool)));

	// Set view origin
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("Set &View Origin");
	connect(action, SIGNAL(triggered(bool)), this, SLOT(contextMenuSetViewOrigin(bool)));

	// Create
	contextMenu_.addSeparator();
	action = contextMenu_.addAction("Create &Fragment");
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

		updateWidgets();
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

	updateWidgets();
}

// Set atom label
void AtenWindow::contextMenuSetAtomLabel(bool checked)
{
	// Cast the sender into a QAction
	QAction* action = qobject_cast<QAction*> (sender());
	if (!action) return;

	CommandNode::run(Commands::Label, "c", qPrintable(action->text()));

	updateWidgets();
}

// Probe atom information
void AtenWindow::contextMenuProbeAtom(bool checked)
{
	if (contextAtom_ != NULL) contextAtom_->print();
	aten_.currentModelOrFrame()->setViewOrigin(contextAtom_->r());
}

// Set view origin
void AtenWindow::contextMenuSetViewOrigin(bool checked)
{
	Model* viewTarget = aten_.currentModelOrFrame();

	viewTarget->setViewOrigin(viewTarget->selectionCentreOfGeometry());

	updateWidgets();
}

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

// Select all elements similar to target atom
void AtenWindow::contextMenuSelectElement(bool checked)
{
	CommandNode::run(Commands::Select, "c", ElementMap::symbol(contextAtom_));

	updateWidgets();
}

// Select similar atoms to target atom, based on connectivity
void AtenWindow::contextMenuSelectSimilar(bool checked)
{
	// Create a NETA description for the clicked atom
	Neta neta;
	neta.setCharacterElement(contextAtom_->element());
	neta.createBasic(contextAtom_, true);

	// Extract the description so we can pass it to the SelectType command
	QString netaString;
	neta.description()->netaPrint(netaString);

	// Now select by type.
	CommandNode::run(Commands::SelectType, "ic", contextAtom_->element(), qPrintable(netaString));

	updateWidgets();
}

// Select fragment to which target atom belongs
void AtenWindow::contextMenuSelectFragment(bool checked)
{
	CommandNode::run(Commands::SelectTree, "i", contextAtom_->id()+1);

	updateWidgets();
}

void AtenWindow::contextMenuTypeSetName(bool checked)
{
	Model* viewTarget = aten_.currentModelOrFrame();

	// Get type name from user
	bool ok;
	QString oldName = contextAtom_->type() ? contextAtom_->type()->name() : "";
	QString name = QInputDialog::getText(this, QString("Add/Set Type Name for %1 atoms").arg(viewTarget->nSelected()), "New name/type for atom:", QLineEdit::Normal, oldName, &ok);
	if (!ok) return;

	// Get names forcefield for current model
	Forcefield* namesFF = viewTarget->namesForcefield();

	// Loop over atoms, attempting to add/retrieve a name type for each atom
	for (RefListItem<Atom,int>* ri = viewTarget->selection(); ri != NULL; ri = ri->next)
	{
		Atom* i = ri->item;

		ForcefieldAtom* ffa = viewTarget->addAtomName(i->element(), name);
		i->setType(ffa);
	}

	updateWidgets();
}

// Re-map atom selection according to specified type
void AtenWindow::contextMenuReMap(bool checked)
{
	// Cast sending QAction
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenWindow::contextMenuReMap - Sender was not a QAction.\n");
		return;
	}

	// Get element mapping style from action text
	ElementMap::ZMapType zm = ElementMap::zMapType(action->text());
	if (zm != ElementMap::nZMapTypes)
	{
		CommandNode::run(Commands::ReMap, "c", qPrintable(action->text()));

		updateWidgets();
	}
}

// Change custom colour of selected atoms
void AtenWindow::contextMenuColourChanged(QColor colour)
{
	CommandNode::run(Commands::ColourAtoms, "dddd", colour.redF(), colour.greenF(), colour.blueF(), colour.alphaF());

	updateWidgets();
}

void AtenWindow::createGlyph()
{
	// Cast sending QAction and grab filename
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenWindow::createGlyph() - Sender was not a QAction.\n");
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
	updateWidgets(AtenWindow::GlyphsTarget);
}

