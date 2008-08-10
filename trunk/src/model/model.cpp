/*
	*** Model functions
	*** src/model/model.cpp
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

#include "model/model.h"
#include "base/aten.h"
#include "base/elements.h"
#include "classes/pattern.h"
#include "classes/clipboard.h"
#include "classes/site.h"
#include "classes/glyph.h"
#include "methods/calculable.h"

// Constructors
Model::Model()
{
	// Private variables
	nSelected_ = 0;
	camera_.set(0.0,0.0,-10.0);
	cameraMatrix_.rows[2].set(0.0,0.0,1.0,-10.0);
	projectionPoint_ = -1;
	cameraRotation_ = 0.0;
	orthoSize_ = 5.0;
	for (int n=0; n<Change::nChangeLogs; n++) logs_[n] = 0;
	spacegroup_ = 0;
	spacegroupSetting_ = 1;
	mass_ = 0.0;
	density_ = 0.0;
	translateScale_ = 1.0;
	forcefield_ = NULL;
	namesForcefield_ = NULL;
	savePoint_ = 0;
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	filter_ = NULL;
	currentUndoState_ = NULL;
	currentRedoState_ = NULL;
	recordingState_ = NULL;
	name_ = "NewModel";
	trajectoryParent_ = NULL;
	trajectoryFilter_ = NULL;
	trajectoryFile_ = NULL;
	trajectoryFirstFrame_ = (streampos) 0;
	trajectoryLastFrame_ = (streampos) 0;
	frameSize_ = 0;
	nCachedFrames_ = 0;
	nTrajectoryFrames_ = 0;
	renderFromSelf_ = TRUE;
	trajectoryCached_ = FALSE;
	trajectoryPosition_ = 0;
	trajectoryPlaying_ = FALSE;
	currentFrame_ = NULL;
	componentPattern_ = NULL;
	nRequested_ = 0;
	moveAllowed_[MonteCarlo::Insert] = TRUE;
	moveAllowed_[MonteCarlo::Delete] = FALSE;
	moveAllowed_[MonteCarlo::Translate] = TRUE;
	moveAllowed_[MonteCarlo::Rotate] = TRUE;
	moveAllowed_[MonteCarlo::ZMatrix] = FALSE;
	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructor
Model::~Model()
{
	clearBonding();
	atoms_.clear();
	patterns_.clear();
	measurements_.clear();
}

// Sets the filename of the model
void Model::setFilename(const char *s)
{
	filename_ = s;
}

// Return the stored filename of the model
const char *Model::filename()
{
	return filename_.get();
}

// Sets the file filter of the model
void Model::setFilter(Filter *f)
{
	filter_ = f;
}

// Return the stored file filter of the model
Filter *Model::filter()
{
	return filter_;
}

// Sets the name of the model
void Model::setName(const char *s)
{
	name_ = s;
}

// Return the name of the model
const char *Model::name()
{
	return name_.get();
}

// Return the mass of the molecule
double Model::mass()
{
	return mass_;
}

// Return the density of the model
double Model::density()
{
	return density_;
}

// Log change
void Model::logChange(Change::ChangeLog cl)
{
	if (cl >= Change::TotalLog) printf("Invalid log quantity passed.\n");
	logs_[cl] ++;
	// For all logs except Change::CameraLog we also update the total log
	if (cl != Change::CameraLog) logs_[Change::TotalLog] ++;
}

// Copy logs
void Model::copyLogs(int *newlogs)
{
	logs_[Change::StructureLog] = newlogs[Change::StructureLog];
	logs_[Change::CoordinateLog] = newlogs[Change::CoordinateLog];
	logs_[Change::SelectionLog] = newlogs[Change::SelectionLog];
	logs_[Change::GlyphLog] = newlogs[Change::GlyphLog];
	logs_[Change::GridLog] = newlogs[Change::GridLog];
}

// Clear
void Model::clear()
{
	glyphs_.clear();
	clearAtoms();
	patterns_.clear();
	frames_.clear();
	// Reset logs and log points
	for (int n=0; n<Change::nChangeLogs; n++) logs_[n] = 0;
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	projectionPoint_ = -1;
}

// Calculate mass
void Model::calculateMass()
{
	// Calculate the mass of the atoms in the model.
	msg.enter("Model::calculateMass");
	mass_ = 0.0;
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) mass_ += elements.atomicMass(i);
	msg.exit("Model::calculateMass");
}

/*
// Forcefields
*/

// Assign charges from forcefield
void Model::assignForcefieldCharges()
{
	// Assign atom-type charges from the currently associated forcefield to the model
	// Perform forcefield typing if necessary
	msg.enter("Model::assignForcefieldCharges");
	Atom *i;
	Forcefield *xff, *patff;
	if (!arePatternsValid())
	{
		msg.print("Cannot assign atomic charges without a valid pattern setup.\n");
		msg.exit("Model::assignForcefieldCharges");
		return;
	}
	typeAll();
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		// Grab current model (global) forcefield
		xff = forcefield_;	
		patff = p->forcefield();
		// Grab pattern forcefield in preference to model's
		if (patff != NULL) xff = patff;
		if (xff == NULL) msg.print("No forcefield is currently assigned to pattern %s. No charges assigned.\n",p->name());
		else
		{
			i = p->firstAtom();
			int ptotalatoms = p->totalAtoms();
			int count = 0;
			while (count < ptotalatoms)
			{
				chargeAtom(i, i->type()->charge());
				i = i->next;
				count ++;
			}
			// Charge atoms in representative pattern molecule
			for (i = p->molecule->atoms(); i != NULL; i = i->next) chargeAtom(i, i->type()->charge());
		}
	}
	msg.exit("Model::assignForcefieldCharges");
}

// Set model's forcefield
void Model::setForcefield(Forcefield *newff)
{
	// Change the associated forcefield of the model to 'newff'
	if (forcefield_ != newff)
	{
		invalidateExpression();
		forcefield_ = newff;
		msg.print("Forcefield '%s' now associated with model '%s'.\n",forcefield_->name(),name_.get());
	}
}

// Remove typing from the model
void Model::removeTyping()
{
	// Remove all atom typing from the current model
	msg.enter("Model::removeTyping");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) setAtomtype(i, NULL, FALSE);
	msg.exit("Model::removeTyping");
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
		Change *newchange = recordingState_->addChange();
		newchange->set(Change::LabelEvent,i->id(),oldlabels,i->labels());
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
		Change *newchange = recordingState_->addChange();
		newchange->set(-Change::LabelEvent,i->id(),oldlabels,i->labels());
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
		Change *newchange = recordingState_->addChange();
		newchange->set(Change::LabelEvent,i->id(),oldlabels,0);
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

void Model::printCoords()
{
	msg.enter("Model::printCoords");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		printf("Atom  %3i  %s  %11.6f  %11.6f  %11.6f  %9.6f\n", i->id(), elements.symbol(i), i->r().x, i->r().y, i->r().z, i->charge());
	//	printf("Atom  %3i  %s  %11.6f  %11.6f  %11.6f  %9.6f  %s\n",i->id(),elements.symbol(i),r.x,r.y,r.z,
	//		i->get_charge(),(ff == NULL ? " " : ff->name(i)));
	}
	msg.exit("Model::printCoords");
}

// Calculate the density of the system (if periodic)
void Model::calculateDensity()
{
	msg.enter("Model::calculateDensity");
	if (cell_.type() != Cell::NoCell)
	{
		// Calculate density in the units specified by prefs.density_internal
		switch (prefs.densityUnit())
		{
			case (Prefs::GramsPerCm):
				density_ = (mass_ / AVOGADRO) / (cell_.volume() / 1.0E24);
				break;
			case (Prefs::AtomsPerAngstrom):
				density_ = atoms_.nItems() / cell_.volume();
				break;
		}
	}
	else density_ = -1.0;
	msg.exit("Model::calculateDensity");
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
	logChange(Change::CoordinateLog);
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

// Print log information
void Model::printLogs()
{
	msg.print("Logs for model '%s':\n",name_.get());
	msg.print("Structure [%i], Coordinates [%i], Visual [%i], Selection [%i], Camera [%i], Glyph [%i], Total [%i]\n", logs_[Change::StructureLog], logs_[Change::CoordinateLog], logs_[Change::VisualLog], logs_[Change::SelectionLog], logs_[Change::CameraLog], logs_[Change::GlyphLog], logs_[Change::TotalLog]);
	msg.print("Expression point : %i\n", expressionPoint_);
	msg.print("  Patterns point : %i\n", patternsPoint_);
	msg.print("Projection point : %i\n", projectionPoint_);
}

// Print Forces
void Model::printForces()
{
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		printf("%4i %3s  %14.6e  %14.6e  %14.6e\n", i->id(), elements.symbol(i), i->f().x, i->f().y, i->f().z);
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
		if ((dat&Atom::FixedData) || (dat == Atom::AllData)) i->setPositionFixed(j->hasFixedPosition());
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
				if ((dat&Atom::FixedData) || (dat == Atom::AllData)) ii[n]->setPositionFixed(jj[n]->hasFixedPosition());
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

/*
// Logs
*/

// Return the log quantity specified
int Model::log(Change::ChangeLog cl)
{
	return logs_[cl];
}

// Reset all logs to zero
void Model::resetLogs()
{
	for (int i=0; i<Change::nChangeLogs; i++) logs_[i] = 0;
}

// Set the save point log for the model
void Model::updateSavePoint()
{
	savePoint_ = logs_[Change::StructureLog] + logs_[Change::CoordinateLog];
}

// Return if the model has been modified since last being saved
bool Model::isModified()
{
	return (savePoint_ == (logs_[Change::StructureLog] + logs_[Change::CoordinateLog]) ? FALSE : TRUE);
}
