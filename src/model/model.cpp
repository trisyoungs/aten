/*
	*** Model Functions
	*** src/model/model.cpp
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

#include "model/model.h"
#include "undo/undostate.h"
#include "undo/model_rename.h"
#include "methods/calculable.h"
#include "base/site.h"
#include "base/grid.h"
#include "base/pattern.h"

ATEN_USING_NAMESPACE

// Constructors
Model::Model() : ListItem<Model>()
{
	// Private variables
	parent_ = NULL;
	visible_ = false;
	type_ = Model::ParentModelType;

	// Camera / View / render
	modelViewMatrix_.setIdentity();
	modelViewMatrix_[14] = -10.0;
	renderSource_ = Model::ModelSource;
	renderGroupPoint_ = -1;
	renderFromVibration_ = false;
	repeatCellsNegative_.set(0,0,0);
	repeatCellsPositive_.set(0,0,0);

	// Properties
	name_ = "NewModel";
	mass_ = 0.0;
	nUnknownAtoms_ = 0;
	translateScale_ = 1.0;
	forcefield_ = NULL;
	namesForcefield_ = NULL;
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	expressionVdwOnly_ = false;
	cell_.setParent(this);
	rmsForce_ = 0.0;
	zMatrixPoint_ = -1;

	// File / Undo
	plugin_ = NULL;
	currentUndoState_ = NULL;
	currentRedoState_ = NULL;
	recordingState_ = NULL;
	undoRedoEnabled_ = false;

	// Trajectory
	trajectoryPlugin_ = NULL;
	trajectoryFramesAreCached_ = false;
	trajectoryPlaying_ = false;
	trajectoryCurrentFrame_ = NULL;
	trajectoryPropagateParentStyle_ = false;
	
	// Component
	componentInsertionPolicy_ = Model::NoPolicy;
	componentPartition_ = 0;
	componentPopulation_ = 0;
	componentDensity_ = 1.0;
	componentRotatable_ = true;

	// Misc Function Data
	bondingCuboids_ = NULL;
	bondingOverlays_ = NULL;
	nCuboids_ = 0;

	// Vibration info
	vibrationCurrentFrame_ = NULL;
	vibrationForward_ = true;
	vibrationFrameIndex_ = -1;

	// Icon
	iconPoint_ = -1;
}

// Destructor
Model::~Model()
{
	clearBonding();
	grids_.clear();
	atoms_.clear();
	bonds_.clear();
	patterns_.clear();
	distanceMeasurements_.clear();
	angleMeasurements_.clear();
	torsionMeasurements_.clear();
}

// Sets the filename of the model
void Model::setFilename(QString filename)
{
	filename_ = filename;
}

// Return the stored filename of the model
QString Model::filename() const
{
	return filename_;
}

// Sets the plugin used to load the model
void Model::setPlugin(FilePluginInterface* plugin)
{
	plugin_ = plugin;
}

// Return the plugin used to load the model
FilePluginInterface* Model::plugin() const
{
	return plugin_;
}

// Sets the name of the model
void Model::setName(QString name)
{
	logChange(Log::Misc);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		ModelRenameEvent* newchange = new ModelRenameEvent;
		newchange->set(name_, name);
		recordingState_->addEvent(newchange);
	}
	name_ = name;
}

// Return the name of the model
QString Model::name() const
{
	return name_;
}

// Clear
void Model::clear()
{
	glyphs_.clear();
	clearAtoms();
	patterns_.clear();
	trajectoryFrames_.clear();
	vibrationFrames_.clear();

	// Reset logs and log points
	changeLog_.reset();
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	zMatrixPoint_ = -1;
	iconPoint_ = -1;
	renderGroupPoint_ = -1;
	icon_ = QIcon();
}

// Print
void Model::print() const
{
	Messenger::enter("Model::print");
	Messenger::print("   Name : %s", qPrintable(name_));
	Messenger::print("   File : %s", qPrintable(filename_));
	Messenger::print("   Mass : %f", mass_);
	if (cell_.type() != UnitCell::NoCell) Messenger::print("   Cell : %s\nDensity : %f %s", UnitCell::cellType(cell_.type()), density(), Prefs::densityUnit(prefs.densityUnit()));
	Messenger::print("  Atoms : %i", atoms_.nItems());
	Messenger::print(" Id     El   FFType    FFId         X             Y             Z              Q      Sel Fix");
	// Print from pattern definition if possible, otherwise just use model atom list
	Atom* i;
	int n;
	if (patterns_.nItems() != 0)
		for (Pattern* p = patterns_.first(); p != NULL; p = p->next)
		{
			i = p->firstAtom();
			for (n=0; n<p->totalAtoms(); n++)
			{
				i->printSummary();
				i = i->next;
			}
		}
	else for (i = atoms_.first(); i != NULL; i = i->next) i->printSummary();
	Messenger::exit("Model::print");
}

// Copy model
void Model::copy(Model* srcmodel)
{
	// Clear any current contents of the model
	clear();
	// Copy name
	name_ = srcmodel->name_;
	// Copy atoms
	for (Atom* i = srcmodel->atoms(); i != NULL; i = i->next) addCopy(i);
	// Copy bonds
	for (Bond* b = srcmodel->bonds(); b != NULL; b = b->next) bondAtoms(b->atomI()->id(), b->atomJ()->id(), b->type());
	// Copy unit cell
	cell_ = srcmodel->cell_;
	// Copy component information
	componentInsertionPolicy_ = srcmodel->componentInsertionPolicy_;
	componentDensity_ = srcmodel->componentDensity_;
	componentPartition_ = srcmodel->componentPartition_;
	componentPopulation_ = srcmodel->componentPopulation_;
	componentRotatable_ = srcmodel->componentRotatable_;
	// Copy forcefield pointer
	forcefield_ = srcmodel->forcefield_;
}

// Copy atom data from specified model
void Model::copyAtomData(Model* srcmodel, int dat)
{
	Messenger::enter("Model::copyAtomData");
	// Simple failsafe - check atom numbers in each are the same
	if (atoms_.nItems() != srcmodel->atoms_.nItems())
	{
		printf("Model::copyAtomData <<<< Models have different numbers of atoms (%i/%i) >>>>\n", atoms_.nItems(), srcmodel->atoms_.nItems());
		Messenger::exit("Model::copyAtomData");
		return;
	}
	Atom* i, *j;
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
	//Messenger::print(Messenger::Verbose, "Copied data for %i atoms from model '%s' to model '%s'.", count);
// name(), srcmodel->name());
	Messenger::exit("Model::copyAtomData");
}

// Copy range of atom data from specified model
void Model::copyAtomData(Model* srcmodel, int dat, int startatom, int ncopy)
{
	Messenger::enter("Model::copyAtomData[range]");
	// Simple failsafe - check atom numbers in each are the same
	int numatoms = atoms_.nItems();
	if (numatoms != srcmodel->atoms_.nItems())
	{
		printf("Model::copyAtomData[range] <<<< Models have different numbers of atoms (%i/%i) >>>>\n", numatoms, srcmodel->atoms_.nItems());
		Messenger::exit("Model::copyAtomData[range]");
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
			Atom** ii = atomArray();
			Atom** jj = srcmodel->atomArray();
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
			Messenger::print(Messenger::Verbose, "Copied data for %i atoms starting at %i from model '%s' to model '%s'.", ncopy, startatom, qPrintable(name_), qPrintable(srcmodel->name()));
		}
	}
	Messenger::exit("Model::copyAtomData[range]");
}

// Set parent model of model (for frames)
void Model::setParent(Model* m)
{
	parent_ = m;
}

// Return parent model of model (for frames)
Model* Model::parent() const
{
	return parent_;
}

// Set model type
void Model::setType(Model::ModelType mt)
{
	type_ = mt;
}

// Return model type
Model::ModelType Model::type()
{
	return type_;
}

// Set whether model is visible
void Model::setVisible(bool b)
{
	visible_ = b;
}

// Return whether model is visible
bool Model::isVisible()
{
	return visible_;
}
