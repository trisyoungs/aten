/*
	*** Model cell functions
	*** src/model/cell.cpp
	Copyright T. Youngs 2007-2012

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
#include "model/undoevent.h"
#include "model/undostate.h"
#include "base/spacegroup.h"
#include "base/generator.h"
#include "base/progress.h"
#include "classes/prefs.h"

// Return pointer to unit cell structure
UnitCell *Model::cell()
{
	return &cell_;
}

// Set cell (vectors)
void Model::setCell(Vec3<double> lengths, Vec3<double> angles)
{
	msg.enter("Model::setCell[vectors]");
	Matrix oldaxes = cell_.axes();
	bool oldhs = (cell_.type() == UnitCell::NoCell ? FALSE : TRUE);
	// Set new axes 
	cell_.set(lengths, angles);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellEvent *newchange = new CellEvent;
		newchange->set(oldaxes, cell_.axes(), oldhs, TRUE);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Camera);
	changeLog.add(Log::Cell);
	msg.exit("Model::setCell[vectors]");
}

// Set cell (axes)
void Model::setCell(Matrix axes)
{
	msg.enter("Model::setCell[axes]");
	Matrix oldaxes = cell_.axes();
	bool oldhs = (cell_.type() == UnitCell::NoCell ? FALSE : TRUE);
	// Set new axes 
	cell_.set(axes);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellEvent *newchange = new CellEvent;
		newchange->set(oldaxes, cell_.axes(), oldhs, TRUE);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Camera);
	changeLog.add(Log::Cell);
	msg.exit("Model::setCell[axes]");
}

// Set cell (parameter)
void Model::setCell(UnitCell::CellParameter cp, double value)
{
	msg.enter("Model::setCell[parameter]");
	Matrix oldaxes = cell_.axes();
	bool oldhs = (cell_.type() == UnitCell::NoCell ? FALSE : TRUE);
	// Set new parameter value
	cell_.setParameter(cp, value);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellEvent *newchange = new CellEvent;
		newchange->set(oldaxes, cell_.axes(), oldhs, TRUE);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Camera);
	changeLog.add(Log::Cell);
	msg.exit("Model::setCell[parameter]");
}

// Set cell (other Cell pointer)
void Model::setCell(UnitCell *newcell)
{
	if (newcell == NULL) printf("Warning: NULL UnitCell pointer passed to Model::setCell().\n");
	else
	{
		Matrix oldaxes = cell_.axes();
		bool oldhs = (cell_.type() == UnitCell::NoCell ? FALSE : TRUE);
		cell_ = *newcell;
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			CellEvent *newchange = new CellEvent;
			newchange->set(oldaxes, cell_.axes(), oldhs, TRUE);
			recordingState_->addEvent(newchange);
		}
	}
	changeLog.add(Log::Camera);
	changeLog.add(Log::Cell);
}

// Remove cell
void Model::removeCell()
{
	msg.enter("Model::removeCell");
	changeLog.add(Log::Camera);
	if (recordingState_ != NULL)
	{
		CellEvent *newchange = new CellEvent;
		newchange->set(cell_.axes(), cell_.axes(), cell_.type() != UnitCell::NoCell, FALSE);
		recordingState_->addEvent(newchange);
	}
	cell_.reset();
	changeLog.add(Log::Cell);
	msg.exit("Model::removeCell");
}

// Fold All Atoms
void Model::foldAllAtoms()
{
	msg.enter("Model::foldAllAtoms");
	// Standard fold - individual atoms
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) positionAtom(i, cell_.fold(i));
	changeLog.add(Log::Coordinates);
	msg.exit("Model::foldAllAtoms");
}

// Fold All Molecules
void Model::foldAllMolecules()
{
	msg.enter("Model::foldAllMolecules");
	int n,m;
	Atom *i, *first = NULL;
	Pattern *p;
	// Molecular fold - fold first atom, others in molecule are MIM'd to this point
	if (!createPatterns())
	{
		msg.print("Molecular fold cannot be performed without a valid pattern definition.\n");
		msg.exit("Model::foldAllMolecules");
		return;
	}
	i = atoms_.first();
	for (p = patterns_.first(); p != NULL; p = p->next)
	{
		for (m=0; m<p->nMolecules(); m++)
		{
			for (n=0; n<p->nAtoms(); n++)
			{
				// If its the first atom, fold and store pointer. If not, MIM w.r.t. stored atom
				if (n == 0)
				{
					positionAtom(i, cell_.fold(i));
					first = i;
				}
				else positionAtom(i, cell_.mim(i,first));
				i = i->next;
			}
		}
	}
	changeLog.add(Log::Coordinates);
	msg.exit("Model::foldAllMolecules");
}

// Apply individual symmetry generator to current atom selection
void Model::pack(Generator *gen)
{
	msg.enter("Model::pack[generator]");
	Clipboard clip;
	Vec3<double> newr;
	int oldnatoms;
	// Ignore the identity operator, and leave if there are no atoms marked
	if ((gen == 0) || (marked_.nItems() == 0))
	{
		msg.enter("Model::pack[generator]");
		return;
	}
	msg.print(Messenger::Verbose,"...Applying generator '%s'\n", gen->name());
	// Store current number of atoms in model
	oldnatoms = atoms_.nItems();
	// Copy selection to clipboard
	clip.copyMarked(this);
	clip.pasteToModel(this, FALSE);
	for (Atom *i = atoms_[oldnatoms]; i != NULL; i = i->next)
	{
		// Get the position of the newly-pasted atom
		newr = cell_.realToFrac(i->r());
		// Apply the rotation and translation
		newr = gen->matrix().transform(newr);
// 		newr +=  cell_.transpose() * gen->translation;
		newr = cell_.fracToReal(newr);
		positionAtom(i, newr);
	}
	msg.exit("Model::pack[generator]");
}

// Apply model's spacegroup symmetry generators
void Model::pack()
{
	msg.enter("Model::pack");
	// Spacegroup should already have been set by a successful call to Model::setSpacegroup()
	if ((cell_.spacegroupId() == 0) && (cell_.nGenerators() == 0))
	{
		msg.print("Crystal packing cannot be performed - no spacegroup set in model and no custom generators defined.\n");
		msg.exit("Model::pack");
		return;
	}
	
	// Generators work on the current selection, so mark all atoms currently in the cell
	selectAll(TRUE);
	if (cell_.spacegroupId() != 0)
	{
		msg.print("Packing cell from previous spacegroup definition.\n");
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
	 	msg.print("Packing cell from manually-defined generator list...\n");
		for (Generator *g = cell_.generators(); g != NULL; g = g->next) pack(g);
	}
	
	// Select overlapping atoms and delete
	selectOverlaps(0.1, TRUE);
	selectionDelete(TRUE);
	msg.exit("Model::pack");
}

// Scale cell and contents
bool Model::scaleCell(const Vec3<double> &scale, bool usecog, bool calcenergy)
{
	msg.enter("Model::scaleCell");
	Vec3<double> oldcog, newcog, newpos;
	UnitCell newcell;
	Matrix newaxes;
	double olde = 0.0, newe;
	bool success;
	int n,m;
	Atom *i;
	// First, make sure we have a cell and a valid pattern (if using cog)
	if (cell_.type() == UnitCell::NoCell)
	{
		msg.print("No cell to scale.\n");
		msg.exit("Model::scaleCell");
		return FALSE;
	}
	if (usecog)
	{
 		if (!createPatterns())
		{
			msg.print("Cell contents cannot be scaled by their centres of geometry if a proper pattern definition does not exist.\n");
			msg.exit("Model::scaleCell");
			return FALSE;
		}
	}
	
	// Create expression for energy calculation?
	if (calcenergy) calcenergy = createExpression();

	// Copy original cell axes, expand and save for later
	newaxes = cell_.axes();
	newaxes.columnMultiply(scale);
	newcell.set(newaxes);
	// We need a working configuration (for COG calculations)
	foldAllAtoms();
	if (calcenergy)
	{
		olde = totalEnergy(this, success);
		if (!success) msg.print("Energy will not be calculated...\n");
	}
	// Cycle over patterns, get COG, convert to old fractional coordinates, then
	// use new cell to get new local coordinates.
	if (usecog)
	{
		for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
		{
			i = p->firstAtom();
			for (n=0; n<p->nMolecules(); n++)
			{
				// Get fractional coordinate COG of this molecule
				oldcog = p->calculateCog(n,this);
				// Get new COG using new cell
				newcog = newcell.fracToReal(cell_.realToFrac(oldcog));
				// Set new atom positions
				for (m=0; m<p->nAtoms(); m++)
				{
					newpos = cell_.mim(i,oldcog) - oldcog + newcog;
					positionAtom(i,newpos);
					i = i->next;
				}
			}
		}
	}
	else
	{
		// Reposition individual atoms
		for (i = atoms_.first(); i != NULL; i = i->next)
		{
			newpos = newcell.fracToReal(cell_.realToFrac(i->r()));
			positionAtom(i,newpos);
		}
	}

	// Calculate new energy before leaving...
	if (calcenergy && success)
	{
		newe = totalEnergy(this, success);
		msg.print("Energy change was %12.7e %s\n", newe-olde, Prefs::energyUnit(prefs.energyUnit()));
	}

	// Set new cell and update model
	setCell(newaxes);
	changeLog.add(Log::Coordinates);
	msg.exit("Model::scaleCell");
	return TRUE;
}

// Replicate Cell
void Model::replicateCell(const Vec3<double> &neg, const Vec3<double> &pos)
{
	msg.enter("Model::replicateCell");
	int count;
	bool stop;
	Vec3<double> tvec;
	Matrix newaxes, oldaxes;
	Clipboard originalClip, clip;

	// If this isn't a periodic model, exit
	if (cell_.type() == UnitCell::NoCell)
	{
		msg.print("No cell to replicate.\n");
		msg.exit("Model::replicateCell");
		return;
	}

	// Perform an atomic fold on the crystal before we begin
	if (prefs.replicateFold()) foldAllAtoms();

	// Copy model contents to clipboard ready for pasting and then clear the model
	clip.copyAll(this);
	clear();

	// Create new unit cell
	oldaxes = cell_.axes();
	newaxes = oldaxes;

	// Set new unit cell dimensions
	tvec.set(pos.x-neg.x, pos.y-neg.y, pos.z-neg.z);
	newaxes.columnMultiply(tvec);
	setCell(newaxes);

	// Paste in whole copies of the original cell - don't worry about fractional cells yet
	Vec3<int> ineg, ipos;
	int ii, jj, kk;
	ineg.set(int(floor(neg.x)), int(floor(neg.y)), int(floor(neg.z)));
	ipos.set(int(ceil(pos.x))-1, int(ceil(pos.y))-1, int(ceil(pos.z))-1);

	// Set up progress indicator
	count = ( (ipos.x - ineg.x) + 1) * ( (ipos.y - ineg.y) + 1) * ( (ipos.z - ineg.z) + 1);
	bool pid = progress.initialise("Creating cell copies...", count, FALSE);

	// Create cell copies
	count = 0;
	stop = FALSE;
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
				//tvec.print();
				clip.pasteToModel(this,tvec);
				msg.print(Messenger::Verbose,"Created copy for vector %8.4f %8.4f %8.4f\n",tvec.x,tvec.y,tvec.z);
				if (!progress.update(pid)) stop = TRUE;
			}
			if (stop) break;
		}
		if (stop) break;
	}
	progress.terminate(pid);

	// Select all atoms and shift if negative replication values were provided
	selectAll();
	tvec = oldaxes.columnAsVec3(0) * -neg.x;
	tvec += oldaxes.columnAsVec3(1) * -neg.y;
	tvec += oldaxes.columnAsVec3(2) * -neg.z;
	translateSelectionLocal(tvec);
	selectNone();

	// Now trim off atoms that are outside the new cell
	if (prefs.replicateTrim())
	{
		bool delatom;
		Atom *i, *j;
		Vec3<double> fracr;
		Matrix cellinverse = cell_.inverse();
	
		int pid = progress.initialise("Trimming excess atoms...", atoms_.nItems(), FALSE);
		i = atoms_.first();
		count = 0;
		while (i != NULL)
		{
			delatom = FALSE;
			// Convert coordinates to fractional coords and test them
			fracr = cellinverse.transform(i->r());
			if ((fracr.x < -0.001) || (fracr.x >= 1.001)) delatom = TRUE;
			else if ((fracr.y < -0.001) || (fracr.y >= 1.001)) delatom = TRUE;
			else if ((fracr.z < -0.001) || (fracr.z >= 1.001)) delatom = TRUE;
			if (delatom)
			{
				j = i->next;
				deleteAtom(i);
				i = j;
			}
			else i = i->next;
			if (!progress.update(pid,++count)) break;
		}
		progress.terminate(pid);
	}

	changeLog.add(Log::Structure);
	msg.exit("Model::replicateCell");
}

// Rotate cell and contents
void Model::rotateCell(int axis, double angle)
{
	msg.enter("Model::rotateCell");
	if (cell_.type() == UnitCell::NoCell)
	{
		msg.print("This model has no cell, and so it can't be rotated.\n");
		msg.exit("Model::rotateCell");
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
	matrixTransformSelection(origin,rotmat,TRUE);
	
	msg.exit("Model::rotateCell");
}

// Frac to Real
void Model::fracToReal()
{
	msg.enter("Model::fracToReal");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) i->r() = cell_.fracToReal(i->r());
	msg.exit("Model::fracToReal");
}

// Calculate and return the density of the system (if periodic)
double Model::density() const
{
	msg.enter("Model::density");
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
	msg.exit("Model::density");
	return density;
}

