/*
	*** Model Functions
	*** src/model/model.cpp
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
#include "gui/gui.h"

// Constructors
Model::Model() : ListItem<Model>()
{
	// Private variables
	parent_ = NULL;
	visible_ = FALSE;

	// Camera / View / render
	modelViewMatrix_.setIdentity();
	modelViewMatrix_[14] = -10.0;
	renderSource_ = Model::ModelSource;
	renderFromVibration_ = FALSE;
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
	expressionVdwOnly_ = FALSE;
	cell_.setParent(this);
	rmsForce_ = 0.0;
	zMatrixPoint_ = -1;

	// File / Undo
	filter_ = NULL;
	currentUndoState_ = NULL;
	currentRedoState_ = NULL;
	recordingState_ = NULL;
	undoRedoEnabled_ = FALSE;

	// Trajectory
	trajectoryFilter_ = NULL;
	trajectoryHeaderFunction_ = NULL;
	trajectoryFrameFunction_ = NULL;
	trajectoryOffsets_ = NULL;
	trajectoryHighestFrameOffset_ = 0;
	trajectoryFrameSize_ = 0;
	nTrajectoryFileFrames_ = 0;
	trajectoryFramesAreCached_ = FALSE;
	trajectoryFrameIndex_ = -1;
	trajectoryPlaying_ = FALSE;
	trajectoryCurrentFrame_ = NULL;
	trajectoryPropagateParentStyle_ = FALSE;
	
	// Component
	componentInsertionPolicy_ = Model::NoPolicy;
	componentPartition_ = 0;
	componentPopulation_ = 0;
	componentDensity_ = 1.0;
	componentRotatable_ = TRUE;

	// Misc Function Data
	bondingCuboids_ = NULL;
	bondingOverlays_ = NULL;
	nCuboids_ = 0;

	// Vibration info
	vibrationCurrentFrame_ = NULL;
	vibrationForward_ = TRUE;
	vibrationFrameIndex_ = -1;
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
void Model::setFilter(Tree* f)
{
	filter_ = f;
}

// Return the stored file filter of the model
Tree* Model::filter() const
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
	trajectoryFrames_.clear();
	vibrationFrames_.clear();
	// Reset logs and log points
	changeLog.reset();
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	zMatrixPoint_ = -1;
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

// Regenerate icon
void Model::regenerateIcon()
{
	if (gui.applicationType() == QApplication::Tty) return;
	msg.enter("Model::regenerateIcon");
	bool framemodel = prefs.frameCurrentModel(), frameview = prefs.frameWholeView(), viewglobe = prefs.viewRotationGlobe();
	
	prefs.setFrameCurrentModel(FALSE);
	prefs.setFrameWholeView(FALSE);
	prefs.setViewRotationGlobe(FALSE);

	changeLog.add(Log::Style);
	icon_ = engine().renderModelIcon(this);

	prefs.setFrameCurrentModel(framemodel);
	prefs.setFrameWholeView(frameview);
	prefs.setViewRotationGlobe(viewglobe);
	
	msg.exit("Model::regenerateIcon");
}

// Return icon
QIcon &Model::icon()
{
	return icon_;
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

/*
// Labelling
*/

// Add label to atom
void Model::addLabel(Atom* i, Atom::AtomLabel al)
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
	changeLog.add(Log::Labels);
}

// Remove atom label
void Model::removeLabel(Atom* i, Atom::AtomLabel al)
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
	changeLog.add(Log::Labels);
}

// Clear labelling from atom
void Model::clearLabels(Atom* i)
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
	changeLog.add(Log::Labels);
}

// Clear atom labelling
void Model::clearAllLabels()
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) clearLabels(i);
	changeLog.add(Log::Labels);
}

// Clear all labels in selection
void Model::selectionClearLabels()
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) clearLabels(i);
	changeLog.add(Log::Labels);
}

// Remove specific labels in selection
void Model::selectionRemoveLabels(Atom::AtomLabel al)
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) removeLabel(i, al);
	changeLog.add(Log::Labels);
}

// Add atom labels
void Model::selectionAddLabels(Atom::AtomLabel al)
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) addLabel(i, al);
	changeLog.add(Log::Labels);
}

/*
// OTHER STUFF
*/

void Model::printCoords() const
{
	msg.enter("Model::printCoords");
	for (Atom* i = atoms_.first(); i != NULL; i = i->next)
	{
		printf("Atom  %3i  %s  %11.6f  %11.6f  %11.6f  %9.6f\n", i->id(), Elements().symbol(i), i->r().x, i->r().y, i->r().z, i->charge());
	//	printf("Atom  %3i  %s  %11.6f  %11.6f  %11.6f  %9.6f  %s\n",i->id(),Elements().symbol(i),r.x,r.y,r.z,
	//		i->get_charge(),(ff == NULL ? " " : ff->name(i)));
	}
	msg.exit("Model::printCoords");
}

// Bohr to Angstrom
void Model::bohrToAngstrom()
{
	msg.enter("Model::bohrToAngstrom");
	// Coordinates
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) i->r() *= ANGBOHR;
	// Cell
	UnitCell::CellType ct = cell_.type();
	if (ct != UnitCell::NoCell)
	{
		Vec3<double> lengths = cell_.lengths();
		lengths *= ANGBOHR;
		cell_.set(lengths,cell_.angles());
	}
	changeLog.add(Log::Coordinates);
	msg.exit("Model::bohrToAngstrom");
}

// Clear charges
void Model::clearCharges()
{
	msg.enter("Model::clearCharges");
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) atomSetCharge(i, 0.0);
	msg.exit("Model::clearCharges");
}

// Print
void Model::print() const
{
	msg.enter("Model::print");
	msg.print("   Name : %s\n", name_.get());
	msg.print("   File : %s\n", filename_.get());
	msg.print("   Mass : %f\n", mass_);
	if (cell_.type() != UnitCell::NoCell) msg.print("   Cell : %s\nDensity : %f %s\n", UnitCell::cellType(cell_.type()), density(), Prefs::densityUnit(prefs.densityUnit()));
	msg.print("  Atoms : %i\n", atoms_.nItems());
	msg.print(" Id     El   FFType    FFId         X             Y             Z              Q      Sel Fix\n");
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
	msg.exit("Model::print");
}

// Print points information
void Model::printLogs() const
{
	msg.print("Logs for model '%s':\n",name_.get());
	changeLog.print();
	msg.print("Expression point : %i\n", expressionPoint_);
	msg.print("  Patterns point : %i\n", patternsPoint_);
}

// Print Forces
void Model::printForces() const
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next)
	{
		printf("%4i %3s  %14.6e  %14.6e  %14.6e\n", i->id(), Elements().symbol(i), i->f().x, i->f().y, i->f().z);
	}
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
	for (Bond *b = srcmodel->bonds(); b != NULL; b = b->next) bondAtoms(b->atomI()->id(), b->atomJ()->id(), b->type());
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
	msg.enter("Model::copyAtomData");
	// Simple failsafe - check atom numbers in each are the same
	if (atoms_.nItems() != srcmodel->atoms_.nItems())
	{
		printf("Model::copyAtomData <<<< Models have different numbers of atoms (%i/%i) >>>>\n", atoms_.nItems(), srcmodel->atoms_.nItems());
		msg.exit("Model::copyAtomData");
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
	//msg.print(Messenger::Verbose,"Copied data for %i atoms from model '%s' to model '%s'.\n", count);
// name(), srcmodel->name());
	msg.exit("Model::copyAtomData");
}

// Copy range of atom data from specified model
void Model::copyAtomData(Model* srcmodel, int dat, int startatom, int ncopy)
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
			Atom* *ii = atomArray();
			Atom* *jj = srcmodel->atomArray();
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

// Return RMS of last calculated atomic forces
double Model::rmsForce() const
{
	return rmsForce_;
}
