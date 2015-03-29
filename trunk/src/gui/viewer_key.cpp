/*
	*** Viewer - Keyboard Input
	*** src/gui/viewer_key.cpp
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

#include <QKeyEvent>
#include "gui/mainwindow.h"
#include "gui/fragments.h"
#include "gui/build.h"
#include "model/model.h"
#include "main/aten.h"

// Return state of specified keymodifier
bool Viewer::keyModifier(Prefs::ModifierKey mk)
{
	return keyModifier_[mk];
}

// Qt Slot (key press event)
void Viewer::keyPressEvent(QKeyEvent* event)
{
	// Check datamodel...
	bool refresh = FALSE, ignore = TRUE;
	Qt::KeyboardModifiers km = event->modifiers();
	keyModifier_[Prefs::ShiftKey] = km&Qt::ShiftModifier;
	keyModifier_[Prefs::CtrlKey] = km&Qt::ControlModifier;
	keyModifier_[Prefs::AltKey] = km&Qt::AltModifier;
	
	// Get current active model
	Model* source = aten_->currentModelOrFrame();
	if (source == NULL)
	{
		printf("Pointless Viewer::keyPressEvent - no source model.\n");
		Messenger::exit("Viewer::keyPressEvent");
		return;
	}

	// Set some useful flags...
	bool manipulate = FALSE;
	bool nofold = atenWindow_->buildWidget->ui.PreventFoldCheck->isChecked();	// ATEN2 TODO
	for (int n=0; n<3; n++)
	{
		if (keyModifier_[n])
		{
			switch (prefs.keyAction(Prefs::ModifierKey(n)))
			{
				case (Prefs::ManipulateKeyAction):
					manipulate = TRUE;
					break;
				default:
					break;
			}
		}
	}
	
	int n;
	
	switch (event->key())
	{
		case (Qt::Key_Left):
			if (keyModifier_[Prefs::CtrlKey])
			{
				printf("Why doesn't this ever get printed?\n");
				source->prepareTransform();
				source->beginUndoState("Rotate selection about world Y axis");
				source->rotateSelectionWorld(2.0,0.0);
				source->endUndoState();
				source->updateMeasurements();
				source->finalizeTransform(oldPositions_, "Transform Selection", nofold);
				atenWindow_->updateWidgets(AtenWindow::CanvasTarget);
			}
			else source->rotateView( keyModifier_[Prefs::ShiftKey] ? -1.0 : -10.0, 0.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Right):
			source->rotateView( keyModifier_[Prefs::ShiftKey] ? 1.0 : 10.0, 0.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Up):
			source->rotateView(0.0, keyModifier_[Prefs::ShiftKey] ? -1.0 : -10.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Down):
			source->rotateView(0.0, keyModifier_[Prefs::ShiftKey] ? 1.0 : 10.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Escape):
			cancelCurrentMode();
			refresh = TRUE;
			ignore = FALSE;
			break;
		// Cycle render styles
		case (Qt::Key_F8):
			n = prefs.renderStyle() + 1;
			if (n == Prefs::nDrawStyles) n = 0;
			atenWindow_->setActiveStyleAction( (Prefs::DrawStyle) n);
			ignore = FALSE;
			break;
		// Cycle colouring styles
		case (Qt::Key_F9):
			n = prefs.colourScheme() + 1;
			if (n == Prefs::nColouringSchemes) n = 0;
			atenWindow_->setActiveSchemeAction( (Prefs::ColouringScheme) n);
			ignore = FALSE;
			break;
		default:
			break;
	}
	
	// Mode-specific
	switch (selectedMode_)
	{
		case (UserAction::DrawFragmentAction):
			// Cycle link atom....
			if (keyModifier_[Prefs::AltKey])
			{
				Fragment* frag = atenWindow_->fragmentsWidget->currentFragment();	// ATEN2 TODO
				if (frag == NULL) break;
				frag->cycleLinkAtom();
				refresh = TRUE;
			}
			// Refresh if Shift status has changed
			if (keyModifier_[Prefs::ShiftKey]) refresh = TRUE;
			if (keyModifier_[Prefs::CtrlKey])
			{
				refresh = TRUE;
				atenWindow_->fragmentsWidget->increaseBondId();
			}
			break;
		default:
			break;
	}
	// Update display if necessary
	if (refresh) update();
	if (ignore) event->ignore();
}

// Qt Slot (key release event)
void Viewer::keyReleaseEvent(QKeyEvent* event)
{
	// Set keystates
	bool oldshift = keyModifier_[Prefs::ShiftKey];
	bool oldctrl = keyModifier_[Prefs::CtrlKey];
	bool oldalt = keyModifier_[Prefs::AltKey];
	Qt::KeyboardModifiers km = event->modifiers();
	keyModifier_[Prefs::ShiftKey] = km&Qt::ShiftModifier;
	keyModifier_[Prefs::CtrlKey] = km&Qt::ControlModifier;
	keyModifier_[Prefs::AltKey] = km&Qt::AltModifier;
	
	// Get current active model
	Model* source = aten_->currentModelOrFrame();
	if (source == NULL)
	{
		printf("Pointless Viewer::keyReleaseEvent - no source model.\n");
		Messenger::exit("Viewer::keyReleaseEvent");
		return;
	}

	// Set some useful flags...
	bool manipulate = FALSE;
	for (int n=0; n<3; n++)
	{
		if (keyModifier_[n])
		{
			switch (prefs.keyAction(Prefs::ModifierKey(n)))
			{
				case (Prefs::ManipulateKeyAction):
					manipulate = TRUE;
					break;
				default:
					break;
			}
		}
	}
	
	// Mode-specific
	switch (selectedMode_)
	{
		case (UserAction::DrawFragmentAction):
			// Refresh if Shift status has changed
			if (keyModifier_[Prefs::ShiftKey] != oldshift) update();
			break;
		default:
			break;
	}

	event->ignore();
}
