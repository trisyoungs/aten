/*
	*** Model cell functions
	*** src/model/cell.cpp
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
#include "base/pattern.h"
#include "model/clipboard.h"
#include "undo/undostate.h"
#include "undo/cell_set.h"

ATEN_USING_NAMESPACE

// Reassemble molecule/fragment, beginning at supplied atom, returning COG of atoms
Vec3<double> Model::reassembleFragment(Atom* i, int referenceBit, int& count, bool centreOfMass)
{
	i->addBit(referenceBit);
	Atom* j;
	Vec3<double> mim, total = i->r();
	if (centreOfMass) total *= ElementMap::atomicMass(i);
	++count;
	selectAtom(i, true);
	for (RefListItem<Bond,int>* bref = i->bonds(); bref != NULL; bref = bref->next)
	{
		j = bref->item->partner(i);
		if (!j->hasBit(referenceBit))
		{
			// MIM this atom with 'i'
			mim = cell_.mim(j, i);
			positionAtom(j, mim);
			total += reassembleFragment(j, referenceBit, count, centreOfMass);
		}
	}
	return total;
}

// Determine COG or COM of reassembled fragment without actually reassembling it
Vec3<double> Model::reassembleFragment(Atom* i, Vec3<double> referencePos, int referenceBit, int& count, bool centreOfMass)
{
	i->addBit(referenceBit);
	Atom* j;
	Vec3<double> mim, total = i->r();
	if (centreOfMass) total *= ElementMap::atomicMass(i);
	++count;
	selectAtom(i, true);
	for (RefListItem<Bond,int>* bref = i->bonds(); bref != NULL; bref = bref->next)
	{
		j = bref->item->partner(i);
		if (!j->hasBit(referenceBit))
		{
			// MIM this atom with the supplied referencePos
			mim = cell_.mim(j, referencePos);
			total += reassembleFragment(j, mim, referenceBit, count, centreOfMass);
		}
	}
	return total;
}

// Return reference to unit cell structure
UnitCell& Model::cell()
{
	return cell_;
}

// Set cell (vectors)
void Model::setCell(Vec3<double> lengths, Vec3<double> angles)
{
	Messenger::enter("Model::setCell[vectors]");
	Matrix oldaxes = cell_.axes();
	bool oldhs = (cell_.type() == UnitCell::NoCell ? false : true);
	// Set new axes 
	cell_.set(lengths, angles);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellSetEvent* newchange = new CellSetEvent;
		newchange->set(oldaxes, cell_.axes(), oldhs, true);
		recordingState_->addEvent(newchange);
	}
	logChange(Log::Cell);
	Messenger::exit("Model::setCell[vectors]");
}

// Set cell (axes)
void Model::setCell(Matrix axes)
{
	Messenger::enter("Model::setCell[axes]");
	Matrix oldaxes = cell_.axes();
	bool oldhs = (cell_.type() == UnitCell::NoCell ? false : true);
	// Set new axes 
	cell_.set(axes);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellSetEvent* newchange = new CellSetEvent;
		newchange->set(oldaxes, cell_.axes(), oldhs, true);
		recordingState_->addEvent(newchange);
	}
	logChange(Log::Cell);
	Messenger::exit("Model::setCell[axes]");
}

// Set cell (parameter)
void Model::setCell(UnitCell::CellParameter cp, double value)
{
	Messenger::enter("Model::setCell[parameter]");
	Matrix oldaxes = cell_.axes();
	bool oldhs = (cell_.type() == UnitCell::NoCell ? false : true);
	// Set new parameter value
	cell_.setParameter(cp, value);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellSetEvent* newchange = new CellSetEvent;
		newchange->set(oldaxes, cell_.axes(), oldhs, true);
		recordingState_->addEvent(newchange);
	}
	logChange(Log::Cell);
	Messenger::exit("Model::setCell[parameter]");
}

// Set cell (other Cell pointer)
bool Model::setCell(UnitCell* newcell)
{
	if (newcell == NULL)
	{
		Messenger::print("Error: NULL UnitCell pointer passed to Model::setCell().");
		return false;
	}
	else
	{
		Matrix oldaxes = cell_.axes();
		bool oldhs = (cell_.type() == UnitCell::NoCell ? false : true);
		cell_ = *newcell;
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			CellSetEvent* newchange = new CellSetEvent;
			newchange->set(oldaxes, cell_.axes(), oldhs, true);
			recordingState_->addEvent(newchange);
		}
	}
	logChange(Log::Cell);
	return true;
}

// Set cell (other Cell reference)
bool Model::setCell(UnitCell& newcell)
{
	Matrix oldaxes = cell_.axes();
	bool oldhs = (cell_.type() == UnitCell::NoCell ? false : true);
	cell_ = newcell;
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellSetEvent* newchange = new CellSetEvent;
		newchange->set(oldaxes, cell_.axes(), oldhs, true);
		recordingState_->addEvent(newchange);
	}

	logChange(Log::Cell);
	return true;
}

// Remove cell
void Model::removeCell()
{
	Messenger::enter("Model::removeCell");
	if (recordingState_ != NULL)
	{
		CellSetEvent* newchange = new CellSetEvent;
		newchange->set(cell_.axes(), cell_.axes(), cell_.type() != UnitCell::NoCell, false);
		recordingState_->addEvent(newchange);
	}
	cell_.reset();
	logChange(Log::Cell);
	Messenger::exit("Model::removeCell");
}

// Fold All Atoms
void Model::foldAllAtoms()
{
	Messenger::enter("Model::foldAllAtoms");
	// Standard fold - individual atoms
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) positionAtom(i, cell_.fold(i));
	logChange(Log::Coordinates);
	Messenger::exit("Model::foldAllAtoms");
}

// Fold All Molecules
void Model::foldAllMolecules()
{
	Messenger::enter("Model::foldAllMolecules");
	int n, m, count;
	Vec3<double> cog;
	
	// Loop over atoms, searching for one which hasn't yet been used.
	clearAtomBits();
	for (Atom* i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->hasBit(1)) continue;
		count = 0;
		selectNone(true);
		cog = reassembleFragment(i, 1, count, false);
		cog /= count;
		// Is the centre of geometry inside the unit cell?
		// If it isn't, translate all atoms so that it is.
		if (!cell_.isInsideCell(cog)) translateSelectionLocal(cell_.fold(cog) - cog, true);
	}
	Messenger::exit("Model::foldAllMolecules");
}

// Apply individual symmetry generator to current atom selection
void Model::pack(Generator* gen)
{
	Messenger::enter("Model::pack[generator]");
	Clipboard clip;
	Vec3<double> newr;
	int oldnatoms;

	// Ignore the identity operator, and leave if there are no atoms marked
	if ((gen == 0) || (marked_.nItems() == 0))
	{
		Messenger::enter("Model::pack[generator]");
		return;
	}
	Messenger::print(Messenger::Verbose, "...Applying generator '%s'", qPrintable(gen->name()));

	// Store current number of atoms in model
	oldnatoms = atoms_.nItems();
	// Copy selection to clipboard
	clip.copyMarked(this);
	clip.pasteToModel(this, false);
	for (Atom* i = atoms_[oldnatoms]; i != NULL; i = i->next)
	{
		// Get the position of the newly-pasted atom
		newr = cell_.realToFrac(i->r());
		// Apply the rotation and translation
		newr = gen->matrix().transform(newr);
// 		newr +=  cell_.transpose() * gen->translation;
		newr = cell_.fracToReal(newr);
		positionAtom(i, newr);
	}
	Messenger::exit("Model::pack[generator]");
}

// Apply model's spacegroup symmetry generators
void Model::pack()
{
	Messenger::enter("Model::pack");

	// Spacegroup should already have been set by a successful call to Model::setSpacegroup()
	if ((cell_.spacegroupId() == 0) && (cell_.nGenerators() == 0))
	{
		Messenger::print("Crystal packing cannot be performed - no spacegroup set in model and no custom generators defined.");
		Messenger::exit("Model::pack");
		return;
	}
	
	// Generators work on the current selection, so mark all atoms currently in the cell
	selectAll(true);
	if (cell_.spacegroupId() != 0)
	{
		Messenger::print("Packing cell from previous spacegroup definition.");
		Generator gen;
		// Code copied verbatim from http://cci.lbl.gov/sginfo/sginfo_loop_symops.html and modified slightly to use Aten's classes
		int iList, f, nTrV, iTrV, nLoopInv, iLoopInv;
		const int *TrV;
		const T_RTMx *lsmx;
		
		T_SgInfo *spacegroup = cell_.spacegroup();
		nLoopInv = Sg_nLoopInv(spacegroup);
		
		nTrV = spacegroup->LatticeInfo->nTrVector;
		TrV = spacegroup->LatticeInfo->TrVector;
		
		for (iTrV = 0; iTrV < nTrV; iTrV++, TrV += 3)
		{
			for (iLoopInv = 0; iLoopInv < nLoopInv; iLoopInv++)
			{
				if (iLoopInv == 0) f =  1;
				else               f = -1;
		
				lsmx = spacegroup->ListSeitzMx;
				
				for (iList = 0; iList < spacegroup->nList; iList++, lsmx++)
				{
					gen.setRotationRow(0, f*lsmx->s.R[0], f*lsmx->s.R[1], f*lsmx->s.R[2]);
					gen.setRotationRow(1, f*lsmx->s.R[3], f*lsmx->s.R[4], f*lsmx->s.R[5]);
					gen.setRotationRow(2, f*lsmx->s.R[6], f*lsmx->s.R[7], f*lsmx->s.R[8]);

					gen.setTranslation( iModPositive(f * lsmx->s.T[0] + TrV[0], STBF), iModPositive(f * lsmx->s.T[1] + TrV[1], STBF), iModPositive(f * lsmx->s.T[2] + TrV[2], STBF), STBF);
				
					pack(&gen);
				}
			}
		}
	}
	else
	{
	 	Messenger::print("Packing cell from manually-defined generator list...");
		for (Generator* g = cell_.generators(); g != NULL; g = g->next) pack(g);
	}
	
	// Select overlapping atoms and delete
	selectOverlaps(0.1, true);
	selectionDelete(true);

	Messenger::exit("Model::pack");
}

// Scale cell and contents
bool Model::scaleCell(const Vec3<double>& scale, bool useCog)
{
	Messenger::enter("Model::scaleCell");

	Vec3<double> oldCog, delta, newPos;
	UnitCell newCell;
	Matrix newAxes;
	Atom* i;
	RefListItem<Atom,int>* ri;

	// First, make sure we have a cell and a valid pattern (if using cog)
	if (cell_.type() == UnitCell::NoCell)
	{
		Messenger::print("No cell to scale.");
		Messenger::exit("Model::scaleCell");
		return false;
	}

	// Copy original cell axes, expand and save for later
	newAxes = cell_.axes();
	newAxes.columnMultiply(scale);
	newCell.set(newAxes);

	// Cycle over patterns, get COG, convert to old fractional coordinates, then
	// use new cell to get new local coordinates.
	foldAllAtoms();
	if (useCog)
	{
		// Clear atom bits and marked selection
		clearAtomBits();
		for (i = atoms_.first(); i != NULL; i = i->next)
		{
			if (i->bit() == 1) continue;

			// Perform tree select from the current atom
			selectNone(true);
			selectTree(i, true);

			// Get centre of geometry of current marked selection
			oldCog = selectionCentreOfGeometry(true);

			// Get COG delta using new cell
			delta = newCell.fracToReal(cell_.realToFrac(oldCog)) - oldCog;

			// Adjust positions of marked atoms and set their bits
			for (ri = marked_.first(); ri != NULL; ri = ri->next)
			{
				positionAtom(ri->item, ri->item->r() + delta);
				ri->item->setBit(1);
			}
		}
	}
	else
	{
		// Reposition individual atoms
		for (i = atoms_.first(); i != NULL; i = i->next)
		{
			newPos = newCell.fracToReal(cell_.realToFrac(i->r()));
			positionAtom(i, newPos);
		}
	}

	// Set new cell and update model
	setCell(newAxes);
	logChange(Log::Coordinates);
	Messenger::exit("Model::scaleCell");
	return true;
}

// Replicate Cell
void Model::replicateCell(const Vec3<double>& negativeCells, const Vec3<double>& positiveCells, bool foldBefore, bool trimAfter)
{
	Messenger::enter("Model::replicateCell");
	int count;
	bool stop;
	Vec3<double> tvec;
	Matrix newaxes, oldaxes;
	Clipboard originalClip, clip;

	// If this isn't a periodic model, exit
	if (cell_.type() == UnitCell::NoCell)
	{
		Messenger::print("No cell to replicate.");
		Messenger::exit("Model::replicateCell");
		return;
	}

	// Perform an atomic fold on the crystal before we begin
	if (foldBefore) foldAllAtoms();

	// Copy model contents to clipboard ready for pasting and then clear the model
	clip.copyAll(this);
	clear();

	// Create new unit cell
	oldaxes = cell_.axes();
	newaxes = oldaxes;

	// Set new unit cell dimensions
	tvec.set(positiveCells.x- negativeCells.x, positiveCells.y- negativeCells.y, positiveCells.z- negativeCells.z);
	newaxes.columnMultiply(tvec);
	setCell(newaxes);

	// Paste in whole copies of the original cell - don't worry about fractional cells yet
	Vec3<int> ineg, ipos;
	int ii, jj, kk;
	ineg.set(int(floor(negativeCells.x)), int(floor(negativeCells.y)), int(floor(negativeCells.z)));
	ipos.set(int(ceil(positiveCells.x))-1, int(ceil(positiveCells.y))-1, int(ceil(positiveCells.z))-1);

	// Set up progress indicator
	count = ( (ipos.x - ineg.x) + 1) * ( (ipos.y - ineg.y) + 1) * ( (ipos.z - ineg.z) + 1);
	Task* task = Messenger::initialiseTask("Creating cell copies", count);

	// Create cell copies
	count = 0;
	stop = false;
	for (ii = ineg.x; ii <= ipos.x; ii++)
	{
		for (jj = ineg.y; jj <= ipos.y; jj++)
		{
			for (kk = ineg.z; kk <= ipos.z; kk++)
			{
				// Set base translation vector for this replication
				tvec = oldaxes.columnAsVec3(0) * ii;
				tvec += oldaxes.columnAsVec3(1) * jj;
				tvec += oldaxes.columnAsVec3(2) * kk;
				clip.pasteToModel(this,tvec);
				Messenger::print(Messenger::Verbose, "Created copy for vector %8.4f %8.4f %8.4f",tvec.x,tvec.y,tvec.z);
				if (!Messenger::incrementTaskProgress(task))
				{
					stop = true;
					break;
				}
			}
			if (stop) break;
		}
		if (stop) break;
	}
	Messenger::terminateTask(task);

	// Select all atoms and shift if negative replication values were provided
	selectAll();
	tvec = oldaxes.columnAsVec3(0) * -negativeCells.x;
	tvec += oldaxes.columnAsVec3(1) * -negativeCells.y;
	tvec += oldaxes.columnAsVec3(2) * -negativeCells.z;
	translateSelectionLocal(tvec);
	selectNone();

	// Now trim off atoms that are outside the new cell
	if (trimAfter)
	{
		bool delatom;
		Atom* i, *j;
		Vec3<double> fracr;
		Matrix cellinverse = cell_.inverse();
	
		Task* task = Messenger::initialiseTask("Trimming excess atoms...", atoms_.nItems());
		i = atoms_.first();
		count = 0;
		while (i != NULL)
		{
			delatom = false;
			// Convert coordinates to fractional coords and test them
			fracr = cellinverse.transform(i->r());
			if ((fracr.x < -0.001) || (fracr.x >= 1.001)) delatom = true;
			else if ((fracr.y < -0.001) || (fracr.y >= 1.001)) delatom = true;
			else if ((fracr.z < -0.001) || (fracr.z >= 1.001)) delatom = true;
			if (delatom)
			{
				j = i->next;
				deleteAtom(i);
				i = j;
			}
			else i = i->next;
			if (!Messenger::updateTaskProgress(task, ++count)) break;
		}
		Messenger::terminateTask(task);
	}

	logChange(Log::Structure);
	Messenger::exit("Model::replicateCell");
}

// Rotate cell and contents
void Model::rotateCell(int axis, double angle)
{
	Messenger::enter("Model::rotateCell");
	if (cell_.type() == UnitCell::NoCell)
	{
		Messenger::print("This model has no cell, and so it can't be rotated.");
		Messenger::exit("Model::rotateCell");
		return;
	}
	Matrix rotmat;
	if (axis == 0) rotmat.createRotationX(angle);
	else if (axis == 1) rotmat.createRotationY(angle);
	else if (axis == 2) rotmat.createRotationZ(angle);
	// Create new cell axes
	Matrix axes = cell_.axes();
// 	axes *= rotmat;				// TODO
// // 	cell_.set(axes);
// 	Vec3<double> lengths, angles;
// 	lengths = cell_.lengths();
// 	angles = cell_.angles();
// 	double temp;
// 	temp = lengths.z;
// 	lengths.z = lengths.y;
// 	lengths.y = temp;
// 	temp = angles.z;
// 	angles.z = angles.y;
// 	angles.y = temp;
// 	cell_.set(lengths, angles);
	// Ensure that our new axes point along positive directions
	
	// Transform atoms
	markAll();
	Vec3<double> origin;
	matrixTransformSelection(origin,rotmat,true);
	
	Messenger::exit("Model::rotateCell");
}

// Calculate and return the density of the system (if periodic)
double Model::density() const
{
	Messenger::enter("Model::density");
	double density;
	if (cell_.type() != UnitCell::NoCell)
	{
		// Calculate density in the units specified by prefs
		switch (prefs.densityUnit())
		{
			case (Prefs::GramsPerCm):
				density = (mass_ / AVOGADRO) / (cell_.volume() / 1.0E24);
				break;
			case (Prefs::AtomsPerAngstrom):
				density = atoms_.nItems() / cell_.volume();
				break;
			default:
				break;
		}
	}
	else density = -1.0;
	Messenger::exit("Model::density");
	return density;
}

	// Return whether the model is periodic
bool Model::isPeriodic() const
{
	return (cell_.type() != UnitCell::NoCell);
}
