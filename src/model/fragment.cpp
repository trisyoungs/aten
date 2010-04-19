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
#include "model/clipboard.h"
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

// Set link partner
void Fragment::setLinkPartner()
{
	if (masterLinkAtom_ == NULL)
	{
		printf("Internal Error: No link atom set, so can't set link partner for fragment.\n");
		masterLinkPartner_ = NULL;
		return;
	}
	if (masterLinkAtom_->nBonds() == 0) masterLinkPartner_ = NULL;
	else masterLinkPartner_ = masterLinkAtom_->bonds()->item->partner(masterLinkAtom_);
}

// Set data from source model
bool Fragment::setMasterModel(Model *m)
{
	msg.enter("Fragment::setMasterModel");
	masterModel_ = m;

	// Define initial link (anchor) atom as first non-element (XX) atom in model (if there is one)
	if (masterModel_->nUnknownAtoms() == 0)
	{
		msg.print(Messenger::Verbose, " ... Fragment model '%s' has no defined anchor point. Assuming first atom.\n", masterModel_->name());
		masterLinkAtom_ = masterModel_->atoms();
	}
	else for (masterLinkAtom_ = masterModel_->atoms(); masterLinkAtom_ != NULL; masterLinkAtom_ = masterLinkAtom_->next) if (masterLinkAtom_->element() == 0) break;

	// Find link partner
	setLinkPartner();

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

	if (prefs.useFrameBuffer() == FALSE) icon_ = gui.offscreenWidget->renderPixmap(100, 100, FALSE);
	else icon_ = QPixmap::fromImage(gui.offscreenWidget->grabFrameBuffer());

	prefs.setScreenObjects(screenbits);

	// Reconfigure canvas to widget size (necessary if image size was changed)
	gui.offscreenView.configure(gui.offscreenWidget->width(), gui.offscreenWidget->height());
	gui.offscreenWidget->setRenderSource(NULL);

	gui.offscreenView.setOffScreenRendering(FALSE);

	// Final tweaks to fragment model - put link atom at 0,0,0
	masterModel_->selectAll();
	masterModel_->translateSelectionLocal(-masterLinkAtom_->r());
	masterModel_->selectNone();

	// Copy master model to orientedModel_ and anchoredModel_
	orientedModel_.copy(masterModel_);
	anchoredModel_.copy(masterModel_);

	// Reset rendering style
	prefs.setRenderStyle(ds);

	msg.exit("Fragment::setMasterModel");
	return TRUE;
}

// Return model pointer
Model *Fragment::masterModel()
{
	return masterModel_;
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

// Cycle link atom
void Fragment::cycleLinkAtom()
{
	masterLinkAtom_ = masterLinkAtom_->next;
	if (masterLinkAtom_ == NULL) masterLinkAtom_ = masterModel_->atoms();
	setLinkPartner();
	// Re-translate masterModel_, orientedModel_, and anchoredModel_ so new link atom is at the origin
	masterModel_->selectAll();
	masterModel_->translateSelectionLocal(-masterLinkAtom_->r());
	masterModel_->selectNone();
	Atom *i = anchoredModel_.atom(masterLinkAtom_->id());
	anchoredModel_.selectAll();
	anchoredModel_.translateSelectionLocal(-i->r());
	anchoredModel_.selectNone();
	i = orientedModel_.atom(masterLinkAtom_->id());
	orientedModel_.selectAll();
	orientedModel_.translateSelectionLocal(-i->r());
	orientedModel_.selectNone();
}

// Reset oriented model
void Fragment::resetOrientedModel()
{
	// Copy masterModel_ over to orientedModel_
	orientedModel_.copy(masterModel_);
	Atom *i = orientedModel_.atom(masterLinkAtom_->id());
	orientedModel_.selectAll();
	orientedModel_.translateSelectionLocal(-i->r());
	orientedModel_.selectNone();
}

// Rotate oriented model according to screen delta
void Fragment::rotateOrientedModel(double dx, double dy)
{
	orientedModel_.selectAll();
	orientedModel_.rotateSelectionWorld(dx,dy);
	orientedModel_.selectNone();
}

// Return oriented model pointer
Model *Fragment::orientedModel()
{
	return &orientedModel_;
}

// Paste oriented model to target model
void Fragment::pasteOrientedModel(Vec3<double> origin, Model *target)
{
	msg.enter("Fragment::pasteOrientedModel");

	// Select all atoms and translate model to correct origin
	orientedModel_.selectAll();
	orientedModel_.translateSelectionLocal(origin);

	// Deselect any anchor atoms
	orientedModel_.deselectElement(0);

	// Paste to the target model, bonding the anchor and linkPartners if a bond was there before
	Clipboard clip;
	clip.copySelection(&orientedModel_);
	clip.pasteToModel(target, FALSE);

	// Translate orientedModel_ back to its previous position
	orientedModel_.selectAll();
	orientedModel_.translateSelectionLocal(-origin);
	orientedModel_.selectNone();

	msg.exit("Fragment::pasteOrientedModel");
}

// Adjust anchored model rotation (from mouse delta)
void Fragment::rotateAnchoredModel(double dx, double dy)
{
	msg.enter("Fragment::rotateAnchoredModel");

	// If a link partner exists, adjust axis rotation value. Otherwise, free rotate model
	if (masterLinkPartner_ != NULL)
	{
		Atom *linkPartner = anchoredModel_.atom(masterLinkPartner_->id());
		Atom *linkAtom = anchoredModel_.atom(masterLinkAtom_->id());
		Vec3<double> ref = anchoredModel_.cell()->mimd(linkPartner, linkAtom);
		ref.normalise();
		anchoredModel_.selectAll();
		anchoredModel_.rotateSelectionVector(Vec3<double>(), ref, -dy);
		anchoredModel_.selectNone();
	}
	else
	{
		anchoredModel_.selectAll();
		anchoredModel_.rotateSelectionWorld(dx,dy);
		anchoredModel_.selectNone();
	}
	msg.exit("Fragment::rotateAnchoredModel");
}

// Return anchored model, oriented to attach to specified atom
Model *Fragment::anchoredModel(Atom *anchorpoint, bool replacehydrogen)
{
	msg.enter("Fragment::anchoredModel");

	// Determine vector along which our reference vector should point
	Vec3<double> orientation;
	if ((!replacehydrogen) || (anchorpoint->nHydrogens()) == 0) orientation = anchorpoint->nextBondVector();
	else
	{
		// Find first hydrogen attached to anchorpoint
		Refitem<Bond,int> *ri;
		for (ri = anchorpoint->bonds(); ri != NULL; ri = ri->next) if (ri->item->partner(anchorpoint)->element() == 1) break;
		orientation = anchorpoint->parent()->cell()->mimd(ri->item->partner(anchorpoint), anchorpoint);
		orientation.normalise();
	}

	// Have we a valid attachment point?
	if (orientation.magnitude() < 0.1)
	{
		msg.exit("Fragment::anchoredModel");
		return NULL;
	}

	// Calculate reference vector in fragment, if a linkPartner is specified
	Vec3<double> ref;
	if (masterLinkPartner_ != NULL)
	{
		Atom *linkPartner = anchoredModel_.atom(masterLinkPartner_->id());
		Atom *linkAtom = anchoredModel_.atom(masterLinkAtom_->id());
		ref = anchoredModel_.cell()->mimd(linkPartner, linkAtom);
		ref.normalise();
	}
	else ref.zero();

	// Calculate cross product to get rotation axis, and dot product to get rotation angle
	Vec3<double> xp = ref * orientation;
	double angle = acos(ref.dp(orientation)) * DEGRAD;

	// Copy original model and reorient
	anchoredModel_.selectAll();
	if (xp.magnitude() > 0.001) anchoredModel_.rotateSelectionVector(Vec3<double>(), xp, -angle);
	anchoredModel_.selectNone();

	msg.exit("Fragment::anchoredModel");
	return &anchoredModel_;	
}

// Paste anchored model to target model
void Fragment::pasteAnchoredModel(Atom *anchorpoint, bool replacehydrogen, Model *target)
{
	msg.enter("Fragment::pasteAnchoredModel");

	// Set up anchored model in correct geometry - have we a valid attachment point?
	if (!anchoredModel(anchorpoint, replacehydrogen))
	{
		msg.exit("Fragment::pasteAnchoredModel");
		return;
	}

	// Get pointers to link and partner atoms, determine if there was a bond there, and then delete link atom
	Bond::BondType bt = Bond::nBondTypes;
	Atom *linkPartner, *linkAtom;
	if (masterLinkPartner_ != NULL)
	{
		linkPartner = anchoredModel_.atom(masterLinkPartner_->id());
		bt = (masterLinkAtom_->findBond(masterLinkPartner_) == NULL ? Bond::nBondTypes : masterLinkAtom_->findBond(masterLinkPartner_)->type());
	}
	linkAtom = anchoredModel_.atom(masterLinkAtom_->id());
	anchoredModel_.deleteAtom(linkAtom);

	// Remove any other anchorpoints, and translate model to correct position relative to anchor
	anchoredModel_.selectElement(0);
	anchoredModel_.selectionDelete();
	anchoredModel_.selectAll();
	anchoredModel_.translateSelectionLocal(anchorpoint->r());

	// If we are to replace a hydrogen in the target model, delete it first
	if (replacehydrogen && (anchorpoint->nHydrogens() != 0))
	{
		// Find first hydrogen attached to anchorpoint (this is what we have assumed in orienting the fragment)
		Refitem<Bond,int> *ri;
		for (ri = anchorpoint->bonds(); ri != NULL; ri = ri->next) if (ri->item->partner(anchorpoint)->element() == 1) break;
		target->deleteAtom(ri->item->partner(anchorpoint));
	}

	// Paste to the target model, bonding the anchor and linkPartners if a bond was there before
	Clipboard clip;
	clip.copyAll(&anchoredModel_);
	clip.pasteToModel(target, FALSE);
	if (bt != Bond::nBondTypes) target->bondAtoms(anchorpoint->id(), target->nAtoms() - anchoredModel_.nAtoms() + linkPartner->id(), bt);

	// Since we've physically altered anchoredModel_, re-copy it
	anchoredModel_.copy(masterModel_);
	linkAtom = anchoredModel_.atom(masterLinkAtom_->id());
	anchoredModel_.selectAll();
	anchoredModel_.translateSelectionLocal(-linkAtom->r());
	anchoredModel_.selectNone();

	msg.exit("Fragment::pasteAnchoredModel");
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
void FragmentGroup::removeFragment(Fragment *frag)
{
	fragments_.remove(frag);
}

// Return first fragment in group
Fragment *FragmentGroup::fragments()
{
	return fragments_.first();
}

