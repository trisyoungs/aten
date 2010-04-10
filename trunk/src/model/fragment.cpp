/*
	*** Fragment Model Data
	*** src/model/fragment.cpp
	Copyright T. Youngs 2007-2010

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUE ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "base/messenger.h"
#include "classes/prefs.h"
#include "model/fragment.h"
#include "model/model.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"

/*
// Fragment
*/

// Constructor
Fragment::Fragment()
{
	// Private variables
	masterModel_ = NULL;
	masterLinkAtom_ = NULL;
	masterLinkPartner_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set data from source model
bool Fragment::setMasterModel(Model *m)
{
	msg.enter("Fragment::setMasterModel");
	masterModel_ = m;

	// Define link (anchor) atom
	if (masterModel_->nUnknownAtoms() == 0)
	{
		msg.print(" ... Warning - Fragment model has no defined anchor point. Assuming first atom.\n");
		masterLinkAtom_ = masterModel_->atoms();
	}
	else if (masterModel_->nUnknownAtoms() > 1)
	{
		msg.print(" ... Warning - Fragment model has multiple anchor points. Using lowest ID.\n");
		for (masterLinkAtom_ = masterModel_->atoms(); masterLinkAtom_ != NULL; masterLinkAtom_ = masterLinkAtom_->next) if (masterLinkAtom_->element() == 0) break;
	}
	else for (masterLinkAtom_ = masterModel_->atoms(); masterLinkAtom_ != NULL; masterLinkAtom_ = masterLinkAtom_->next) if (masterLinkAtom_->element() == 0) break;
	if (masterLinkAtom_ == NULL)
	{
		msg.print(" ... Error: No link atom defined for fragment '%s'. Fragment will be removed from list.\n", masterModel_->name());
		msg.exit("Fragment::setMasterModel");
		return FALSE;
	}

	// Find link partner
	if (masterLinkAtom_->nBonds() != 1) masterLinkPartner_ = masterLinkAtom_->bonds()->item->partner(masterLinkAtom_);
	else if (masterLinkAtom_ == masterModel_->atoms()) masterLinkPartner_ = masterLinkAtom_->next;
	else masterLinkPartner_ = masterModel_->atoms();

	// Create icon
	// Store current rendering style so we can reset afterwards
	Atom::DrawStyle ds = prefs.renderStyle();
	prefs.setRenderStyle(Atom::SphereStyle);

	// Centre model at 0,0,0 here...
	masterModel_->selectAll();
	masterModel_->centre(0.0,0.0,0.0,FALSE,FALSE,FALSE);
	masterModel_->selectNone();

	// Generate pixmap for fragment
	int screenbits = prefs.screenObjects();
	prefs.setScreenObjects(prefs.offScreenObjects());
	gui.offscreenWidget->setRenderSource(masterModel_);
	gui.offscreenView.postRedisplay();
	gui.offscreenView.setOffScreenRendering(TRUE);

	/*if (prefs.useFrameBuffer() == FALSE) icon_ = gui.offscreenWidget->renderPixmap(100, 100, FALSE);
	else icon_ = */QPixmap::fromImage(gui.offscreenWidget->grabFrameBuffer());

	prefs.setScreenObjects(screenbits);

	// Reconfigure canvas to widget size (necessary if image size was changed)
	gui.offscreenView.configure(gui.offscreenWidget->width(), gui.offscreenWidget->height());
	gui.offscreenWidget->setRenderSource(NULL);

	msg.exit("Fragment::setMasterModel");
	return TRUE;
}

// Return model pointer
Model *Fragment::masterModel()
{
	return masterModel_;
}

// Finalise structure, preparing master model for use
void Fragment::finalise()
{
	msg.enter("Fragment::finalise");
	// Final tweaks to fragment model - put link atom at 0,0,0
		masterModel_->selectAll();
		masterModel_->translateSelectionLocal(-masterLinkAtom_->r());
		masterModel_->selectNone();
	msg.exit("Fragment::finalise");
}

// Set icon (from pixmap)
void Fragment::setIcon(QPixmap &pixmap)
{
	icon_ = pixmap;
}

// Return pixmap
QIcon &Fragment::icon()
{
	return icon_;
}

/*
// Fragment Group
*/

// Constructor
FragmentGroup::FragmentGroup()
{
	// Private variables

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set name of group
void FragmentGroup::setName(const char *s)
{
	name_ = s;
}

// Return name of group
const char *FragmentGroup::name()
{
	return name_.get();
}

// Return number of fragments in group
int FragmentGroup::nFragments()
{
	return fragments_.nItems();
}

// Add new fragment
Fragment *FragmentGroup::addFragment()
{
	return fragments_.add();
}

// Remove existing fragment
void *FragmentGroup::removeFragment(Fragment *frag)
{
	fragments_.remove(frag);
}

// Return first fragment in group
Fragment *FragmentGroup::fragments()
{
	return fragments_.first();
}
