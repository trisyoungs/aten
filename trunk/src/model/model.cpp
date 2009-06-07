/*
	*** Model functions
	*** src/model/model.cpp
	Copyright T. Youngs 2007-2009

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

#include "model/model.h"
#include "model/undostate.h"
#include "model/undoevent.h"
#include "model/clipboard.h"
#include "methods/calculable.h"
#include "classes/prefs.h"
#include "classes/site.h"
#include "classes/grid.h"
#include "classes/forcefieldbound.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"
#include "base/pattern.h"

// Constructors
Model::Model()
{
	// Private variables
	nSelected_ = 0;
	nMarked_ = 0;
	camera_.set(0.0,0.0,-10.0);
	cameraMatrix_.rows[2].set(0.0,0.0,1.0,-10.0);
	projectionPoint_ = -1;
	cameraRotation_ = 0.0;
	mass_ = 0.0;
	density_ = 0.0;
	nUnknownAtoms_ = 0;
	translateScale_ = 1.0;
	forcefield_ = NULL;
	namesForcefield_ = NULL;
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	filter_ = NULL;
	currentUndoState_ = NULL;
	currentRedoState_ = NULL;
	recordingState_ = NULL;
	name_ = "NewModel";
	trajectoryParent_ = NULL;
	trajectoryFilter_ = NULL;
	trajectoryHeaderFunction_ = NULL;
	trajectoryFrameFunction_ = NULL;
	trajectoryOffsets_ = NULL;
	highestFrameOffset_ = 0;
	frameSize_ = 0;
	nFileFrames_ = 0;
	renderFromSelf_ = TRUE;
	framesAreCached_ = FALSE;
	frameIndex_ = -1;
	trajectoryPlaying_ = FALSE;
	currentFrame_ = NULL;
	componentPattern_ = NULL;
	nRequested_ = 0;
	moveAllowed_[MonteCarlo::Insert] = TRUE;
	moveAllowed_[MonteCarlo::Delete] = FALSE;
	moveAllowed_[MonteCarlo::Translate] = TRUE;
	moveAllowed_[MonteCarlo::Rotate] = TRUE;
	moveAllowed_[MonteCarlo::ZMatrix] = FALSE;
	undoRedoEnabled_ = FALSE;
	cell_.setParent(this);
	bondingCuboids_ = NULL;
	bondingOverlays_ = NULL;
	nCuboids_ = 0;
	// Allocate SGInfo Seitz matrix arrays
	spacegroup_.MaxList = 192;
	spacegroup_.ListSeitzMx = new T_RTMx[192];
	spacegroup_.ListRotMxInfo = new T_RotMxInfo[192];

	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructor
Model::~Model()
{
	clearBonding();
	grids_.clear();
	atoms_.clear();
	patterns_.clear();
	distances_.clear();
	angles_.clear();
	torsions_.clear();
	// Delete sginfo arrays
	delete[] spacegroup_.ListSeitzMx;
	delete[] spacegroup_.ListRotMxInfo;
}

// Sets the filename of the model
void Model::setFilename(const char *s)
{
	filename_ = s;
}

// Return the stored filename of the model
const char *Model::filename() const
{
	return filename_.get();
}

// Sets the file filter of the model
void Model::setFilter(Tree *f)
{
	filter_ = f;
}

// Return the stored file filter of the model
Tree *Model::filter() const
{
	return filter_;
}

// Sets the name of the model
void Model::setName(const char *s)
{
	changeLog.add(Log::Misc);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		ModelRenameEvent *newchange = new ModelRenameEvent;
		newchange->set(name_.get(), s);
		recordingState_->addEvent(newchange);
	}
	name_ = s;
}

// Return the name of the model
const char *Model::name() const
{
	return name_.get();
}

// Clear
void Model::clear()
{
	glyphs_.clear();
	clearAtoms();
	patterns_.clear();
	frames_.clear();
	// Reset logs and log points
	changeLog.reset();
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	projectionPoint_ = -1;
}


/*
// Labelling
*/

// Add label to atom
void Model::addLabel(Atom *i, Atom::AtomLabel al)
{
	int oldlabels = i->labels();
	i->addLabel(al);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		LabelEvent *newchange = new LabelEvent;
		newchange->set(i->id(), oldlabels, i->labels());
		recordingState_->addEvent(newchange);
	}
}

// Remove atom label
void Model::removeLabel(Atom *i, Atom::AtomLabel al)
{
	int oldlabels = i->labels();
	i->removeLabel(al);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		LabelEvent *newchange = new LabelEvent;
		newchange->set(i->id(), oldlabels, i->labels());
		recordingState_->addEvent(newchange);
	}
}

// Clear labelling from atom
void Model::clearLabels(Atom *i)
{
	int oldlabels = i->labels();
	i->clearLabels();
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		LabelEvent *newchange = new LabelEvent;
		newchange->set(i->id(), oldlabels, 0);
		recordingState_->addEvent(newchange);
	}
}

// Clear atom labelling
void Model::clearAllLabels()
{
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) clearLabels(i);
}

// Clear all labels in selection
void Model::selectionClearLabels()
{
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) clearLabels(i);
}

// Remove specific labels in selection
void Model::selectionRemoveLabels(Atom::AtomLabel al)
{
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) removeLabel(i, al);
}

// Add atom labels
void Model::selectionAddLabels(Atom::AtomLabel al)
{
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) addLabel(i, al);
}

/*
// OTHER STUFF
*/

void Model::printCoords() const
{
	msg.enter("Model::printCoords");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		printf("Atom  %3i  %s  %11.6f  %11.6f  %11.6f  %9.6f\n", i->id(), elements().symbol(i), i->r().x, i->r().y, i->r().z, i->charge());
	//	printf("Atom  %3i  %s  %11.6f  %11.6f  %11.6f  %9.6f  %s\n",i->id(),elements().symbol(i),r.x,r.y,r.z,
	//		i->get_charge(),(ff == NULL ? " " : ff->name(i)));
	}
	msg.exit("Model::printCoords");
}

// Bohr to Angstrom
void Model::bohrToAngstrom()
{
	// Convert coordinates and cell from Bohr to Angstrom
	msg.enter("Model::bohrToAngstrom");
	// Coordinates
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) i->r() *= ANGBOHR;
	// Cell
	Cell::CellType ct = cell_.type();
	if (ct != Cell::NoCell)
	{
		Vec3<double> lengths = cell_.lengths();
		lengths *= ANGBOHR;
		cell_.set(lengths,cell_.angles());
	}
	changeLog.add(Log::Coordinates);
	msg.exit("Model::bohrToAngstrom");
}

// Reset atom tempi's
void Model::resetTempi(int value)
{
	msg.enter("Model::resetTempi");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) i->tempi = value;
	msg.exit("Model::resetTempi");
}

// Clear charges
void Model::clearCharges()
{
	msg.enter("Model::clearCharges");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) chargeAtom(i, 0.0);
	msg.exit("Model::clearCharges");
}

// Print
void Model::print()
{
	msg.enter("Model::print");
	msg.print("   Name : %s\n", name_.get());
	msg.print("   File : %s\n", filename_.get());
	msg.print("   Mass : %f\n", mass_);
	if (cell_.type() != Cell::NoCell) msg.print("   Cell : %s\nDensity : %f %s\n", Cell::cellType(cell_.type()), density_, Prefs::densityUnit(prefs.densityUnit()));
	msg.print("  Atoms : %i\n", atoms_.nItems());
	msg.print(" Id     El   FFType         X             Y             Z              Q        S  \n");
	// Print from pattern definition if possible, otherwise just use model atom list
	Atom *i;
	int n;
	if (patterns_.nItems() != 0)
		for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
		{
			i = p->firstAtom();
			for (n=0; n<p->totalAtoms(); n++)
			{
				i->printSummary();
				i = i->next;
			}
		}
	else for (i = atoms_.first(); i != NULL; i = i->next) i->printSummary();
	msg.exit("Model::print");
}

// Print points information
void Model::printLogs()
{
	msg.print("Logs for model '%s':\n",name_.get());
	changeLog.print();
	msg.print("Expression point : %i\n", expressionPoint_);
	msg.print("  Patterns point : %i\n", patternsPoint_);
	msg.print("Projection point : %i\n", projectionPoint_);
}

// Print Forces
void Model::printForces()
{
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		printf("%4i %3s  %14.6e  %14.6e  %14.6e\n", i->id(), elements().symbol(i), i->f().x, i->f().y, i->f().z);
	}
}

// Copy model
void Model::copy(Model *srcmodel)
{
	// Clear any current contents of the model
	clear();
	// Copy all atoms with a clipboard
	Clipboard clip;
	clip.copyAll(srcmodel);
	clip.pasteToModel(this);
	// Copy unit cell
	cell_ = srcmodel->cell_;
}

// Copy atom data from specified model
void Model::copyAtomData(Model *srcmodel, int dat)
{
	msg.enter("Model::copyAtomData");
	// Simple failsafe - check atom numbers in each are the same
	if (atoms_.nItems() != srcmodel->atoms_.nItems())
	{
		printf("Model::copyAtomData <<<< Models have different numbers of atoms (%i/%i) >>>>\n", atoms_.nItems(), srcmodel->atoms_.nItems());
		msg.exit("Model::copyAtomData");
		return;
	}
	Atom *i, *j;
	j = srcmodel->atoms_.first();
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		// Copy data items referenced in 'dat'
		if ((dat&Atom::PositionData) || (dat == Atom::AllData)) i->r() = j->r();
		if ((dat&Atom::ForceData) || (dat == Atom::AllData)) i->f() = j->f();
		if ((dat&Atom::VelocityData) || (dat == Atom::AllData)) i->v() = j->v();
		if ((dat&Atom::ElementData) || (dat == Atom::AllData)) i->setElement(j->element());
		if ((dat&Atom::ChargeData) || (dat == Atom::AllData)) i->setCharge(j->charge());
		if ((dat&Atom::FixedData) || (dat == Atom::AllData)) i->setPositionFixed(j->isPositionFixed());
		j = j->next;
	}
	//msg.print(Messenger::Verbose,"Copied data for %i atoms from model '%s' to model '%s'.\n", count);
// name(), srcmodel->name());
	msg.exit("Model::copyAtomData");
}

// Copy range of atom data from specified model
void Model::copyAtomData(Model *srcmodel, int dat, int startatom, int ncopy)
{
	msg.enter("Model::copyAtomData[range]");
	// Simple failsafe - check atom numbers in each are the same
	int numatoms = atoms_.nItems();
	if (numatoms != srcmodel->atoms_.nItems())
	{
		printf("Model::copyAtomData[range] <<<< Models have different numbers of atoms (%i/%i) >>>>\n", numatoms, srcmodel->atoms_.nItems());
		msg.exit("Model::copyAtomData[range]");
		return;
	}
	// Check limits of requested copy
	int finishatom = startatom + ncopy;
	if (ncopy > 0) 
	{
		if (startatom >= numatoms) printf("Model::copyAtomData[range] <<<< Start atom (%i) is past end of model contents >>>>\n",startatom);
		else if (finishatom > numatoms) printf("Model::copyAtomData[range] <<<< End atom too high (%i c.f. N=%i) >>>>\n",finishatom,numatoms);
		else
		{
			// Get staticatoms arrays from both models
			Atom **ii = atomArray();
			Atom **jj = srcmodel->atomArray();
			for (int n=startatom; n<finishatom; n++)
			{
				// Copy data items referenced in 'dat'
				if ((dat&Atom::PositionData) || (dat == Atom::AllData)) ii[n]->r() = jj[n]->r();
				if ((dat&Atom::ForceData) || (dat == Atom::AllData)) ii[n]->f() = jj[n]->f();
				if ((dat&Atom::VelocityData) || (dat == Atom::AllData)) ii[n]->v() = jj[n]->v();
				if ((dat&Atom::ElementData) || (dat == Atom::AllData)) ii[n]->setElement(jj[n]->element());
				if ((dat&Atom::ChargeData) || (dat == Atom::AllData)) ii[n]->setCharge(jj[n]->charge());
				if ((dat&Atom::FixedData) || (dat == Atom::AllData)) ii[n]->setPositionFixed(jj[n]->isPositionFixed());
			}
			msg.print(Messenger::Verbose,"Copied data for %i atoms starting at %i from model '%s' to model '%s'.\n", ncopy, startatom, name_.get(), srcmodel->name_.get());
		}
	}
	msg.exit("Model::copyAtomData[range]");
}

// Calculate and return RMS of current atomic forces
double Model::calculateRmsForce()
{
	msg.enter("Model::calculateRmsForce");
	double rmsforce = 0.0;
	Atom **modelatoms = atomArray();
	for (int i=0; i<atoms_.nItems(); i++)
	{
		rmsforce += modelatoms[i]->f().x * modelatoms[i]->f().x;
		rmsforce += modelatoms[i]->f().y * modelatoms[i]->f().y;
		rmsforce += modelatoms[i]->f().z * modelatoms[i]->f().z;
	}
	rmsforce /= atoms_.nItems();
	msg.exit("Model::calculateRmsForce");
	return sqrt(rmsforce);
}
