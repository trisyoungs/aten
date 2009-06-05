/*
	*** Model cell functions
	*** src/model/cell.cpp
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
#include "base/pattern.h"
#include "model/clipboard.h"
#include "model/undoevent.h"
#include "model/undostate.h"
#include "base/spacegroup.h"
#include "base/generator.h"
#include "main/aten.h"
#include "classes/prefs.h"

// Return pointer to unit cell structure
Cell *Model::cell()
{
	return &cell_;
}

// Set cell (vectors)
void Model::setCell(Vec3<double> lengths, Vec3<double> angles)
{
	msg.enter("Model::setCell[vectors]");
	Vec3<double> oldlengths = cell_.lengths();
	Vec3<double> oldangles = cell_.angles();
	bool oldhs = (cell_.type() == Cell::NoCell ? FALSE : TRUE);
	// Set new axes 
	cell_.set(lengths, angles);
	calculateDensity();
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellEvent *newchange = new CellEvent;
		newchange->set(oldlengths, oldangles, lengths, angles, oldhs, TRUE);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Visual);
	msg.exit("Model::setCell[vectors]");
}

// Set cell (axes)
void Model::setCell(Mat3<double> axes)
{
	msg.enter("Model::setCell[axes]");
	Vec3<double> oldlengths = cell_.lengths();;
	Vec3<double> oldangles = cell_.angles();
	bool oldhs = (cell_.type() == Cell::NoCell ? FALSE : TRUE);
	// Set new axes 
	cell_.set(axes);
	calculateDensity();
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellEvent *newchange = new CellEvent;
		newchange->set(oldlengths, oldangles, cell_.lengths(), cell_.angles(), oldhs, TRUE);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Visual);
	msg.exit("Model::setCell[axes]");
}

// Set cell (parameter)
void Model::setCell(Cell::CellParameter cp, double value)
{
	msg.enter("Model::setCell[parameter]");
	Vec3<double> oldlengths = cell_.lengths();
	Vec3<double> oldangles = cell_.angles();
	bool oldhs = (cell_.type() == Cell::NoCell ? FALSE : TRUE);
	// Set new parameter value
	cell_.setParameter(cp, value);
	calculateDensity();
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		CellEvent *newchange = new CellEvent;
		newchange->set(oldlengths, oldangles, cell_.lengths(), cell_.angles(), oldhs, TRUE);
		recordingState_->addEvent(newchange);
	}
	changeLog.add(Log::Visual);
	msg.exit("Model::setCell[parameter]");
}

// Set cell (other Cell pointer)
void Model::setCell(Cell *newcell)
{
	if (newcell == NULL) printf("Warning: NULL Cell pointer passed to Model::setCell().\n");
	else
	{
		Vec3<double> oldlengths = cell_.lengths();
		Vec3<double> oldangles = cell_.angles();
		bool oldhs = (cell_.type() == Cell::NoCell ? FALSE : TRUE);
		cell_ = *newcell;
		calculateDensity();
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			CellEvent *newchange = new CellEvent;
			newchange->set(oldlengths, oldangles, cell_.lengths(), cell_.angles(), oldhs, TRUE);
			recordingState_->addEvent(newchange);
		}
	}
	changeLog.add(Log::Visual);
}

// Remove cell
void Model::removeCell()
{
	msg.enter("Model::removeCell");
	changeLog.add(Log::Visual);
// 	changeLog.add(Log::Structure);
	if (recordingState_ != NULL)
	{
		CellEvent *newchange = new CellEvent;
		newchange->set(cell_.lengths(), cell_.angles(), cell_.lengths(), cell_.angles(), cell_.type() == Cell::NoCell, FALSE);
		recordingState_->addEvent(newchange);
	}
	cell_.reset();
	msg.exit("Model::removeCell");
}

// Fold All Atoms
void Model::foldAllAtoms()
{
	msg.enter("Model::foldAllAtoms");
	// Standard fold - individual atoms
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) cell_.fold(i, this);
	changeLog.add(Log::Coordinates);
	msg.exit("Model::foldAllAtoms");
}

// Fold All Molecules
void Model::foldAllMolecules()
{
	msg.enter("Model::foldAllMolecules");
	int n,m;
	Atom *i, *first;
	Pattern *p;
	// Molecular fold - fold first atom, others in molecule are MIM'd to this point
	if (!autocreatePatterns(FALSE))
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
					cell_.fold(i, this);
					first = i;
				}
				else i->r() = cell_.mim(i,first);
				i = i->next;
			}
		}
	}
	changeLog.add(Log::Coordinates);
	msg.exit("Model::foldAllMolecules");
}

// Set spacegroup info
void Model::setSpacegroup(const char *sg)
{
	msg.enter("Model::setSpacegroup");
	// This is basically a chunk of verbatim code from 'sgquick.c'

	// Do a table lookup of the sg text (assume volume is 'A')
	const T_TabSgName *tsgn = FindTabSgNameEntry(sg, 'A');
	if (tsgn == NULL)
	{
		msg.print("Unable to find spacegroup '%s'.\n", sg);
		msg.exit("Model::setSpacegroup");
		return;
	}
	// Check for hexagonal basis, and whether to force rhombohedral basis
	if (strcmp(tsgn->Extension, "H") == 0)
	{
		if (!prefs.forceRhombohedral()) msg.print("Warning: Spacegroup has hexagonal basis.\n");
		else
		{
			Dnchar newname(128);
			newname = tsgn->SgLabels;
			newname.cat(":R");
			tsgn = FindTabSgNameEntry(newname.get(), 'A');
			if (tsgn == NULL)
			{
				msg.print("Unable to find spacegroup '%s'.\n", sg);
				msg.exit("Model::setSpacegroup");
				return;
			}
			msg.print("Spacegroup %s forced into rhombohedral basis.\n", tsgn->SgLabels);
		}
	}
	cell_.setSpacegroupId(tsgn->SgNumber);
	cell_.setSpacegroup(tsgn->HallSymbol);	

	// Initialize the SgInfo structure
	InitSgInfo(&spacegroup_);
	spacegroup_.TabSgName = tsgn;
	
	// Translate the Hall symbol and generate the whole group
	ParseHallSymbol(tsgn->HallSymbol, &spacegroup_);
	if (SgError != NULL) return;
	
	/* Do some book-keeping and derive crystal system, point group,
	and - if not already set - find the entry in the internal
	table of space group symbols
	*/
	int i = CompleteSgInfo(&spacegroup_);

	msg.print(Messenger::Verbose, "Space group belongs to the %s crystal system.\n", XS_Name[spacegroup_.XtalSystem]);

	msg.exit("Model::setSpacegroup");
}

// Apply individual symmetry generator to current atom selection
void Model::pack(Generator *gen)
{
	msg.enter("Model::pack[generator]");
	Clipboard clip;
	Vec3<double> newr;
	int oldnatoms;
	// Ignore the identity operator, and leave if there are no atoms marked
	if ((gen == 0) || (nMarked_ == 0))
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
		newr *= gen->matrix();
// 		newr +=  cell_.transpose() * gen->translation;
		i->r() = cell_.fracToReal(newr);
		cell_.fold(i, this);
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
	// Generators work on the current selection, so select all atoms currently in the cell
	selectAll(TRUE);
	if (cell_.spacegroupId() != 0)
	{
		msg.print("Packing cell from previous spacegroup definition.\n");
		Generator gen;
		// Loop over the Seitz matrix definitions in the SGInfo structure
		for (int n=0; n<spacegroup_.nList; ++n)
		{
			// Create a generator from the Seitz matrix data
			gen.set(spacegroup_.ListSeitzMx[n].a);
			pack(&gen);
			// Perform inversion 
			if (spacegroup_.Centric == -1)
			{
				gen.negateMatrix();
				pack(&gen);
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
bool Model::scaleCell(const Vec3<double> &scale, bool usecog)
{
	msg.enter("Model::scaleCell");
	Vec3<double> oldcog, newcog, newpos;
	Cell newcell;
	Mat3<double> newaxes;
	bool calcenergy;
	double olde, newe;
	int n,m;
	Atom *i;
	// First, make sure we have a cell and a valid pattern (if using cog
	if (cell_.type() == Cell::NoCell)
	{
		msg.print("No cell to scale.\n");
		msg.exit("Model::scaleCell");
		return FALSE;
	}
	if (usecog && (!autocreatePatterns(!usecog)))
	{
		msg.exit("Model::scaleCell");
		return FALSE;
	}
	calcenergy = createExpression();
	// Copy original cell axes, expand and save for later
	newaxes = cell_.axes();
	newaxes.rowMultiply(scale);
	newcell.set(newaxes);
	// We need a working configuration (for COG calculations)
	foldAllAtoms();
	if (calcenergy) olde = totalEnergy(this);
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
	if (calcenergy)
	{
		newe = totalEnergy(this);
		msg.print("Energy change was %12.7e %s\n", newe-olde, Prefs::energyUnit(prefs.energyUnit()));
	}
	// Set new cell and update model
	setCell(newaxes);
	changeLog.add(Log::Coordinates);
	msg.exit("Model::scaleCell");
	return TRUE;
}

// // Replicate Cell
// void Model::replicateCell(const Vec3<double> &neg, const Vec3<double> &pos)
// {
// 	msg.enter("Model::replicateCell");
// 	int count;
// 	bool stop;
// 	Vec3<double> tvec;
// 	Mat3<double> newaxes, oldaxes;
// 
// 	// If this isn't a periodic model, exit
// 	if (cell_.type() == Cell::NoCell)
// 	{
// 		msg.print("No cell to replicate.\n");
// 		msg.exit("Model::replicateCell");
// 		return;
// 	}
// 
// 	// Perform an atomic fold on the crystal before we begin
// 	if (prefs.replicateFold()) foldAllAtoms();
// 
// 	// Create two clipboards - copy the original model to one of them
// 	Clipboard originalClip, clip;
// 	originalClip.copyAll(this);
// 
// 	// Create the new unit cell in the original model
// 	oldaxes = cell_.axes();
// 	// Copy model to clipboard ready for pasting
// 	clip.copyAll(this);
// 	// Take transpose of old and new axes for convenient multiplication
// 	newaxes = oldaxes;
// 
// 	// Set new unit cell dimensions
// 	tvec.set(pos.x+1.0-neg.x, pos.y+1.0-neg.y, pos.z+1.0-neg.z);
// 	newaxes.rowMultiply(tvec);
// 	setCell(newaxes);
// 
// 	// Clear the original model
// 	clear();
// 
// 	// Re-centre the clipboard copy so it is at the new cell origin
// 	originalClip.translate(-cell_.centre());	// See above
// 
// 	// Paste in whole copies of the original cell - don't worry about fractional cells yet
// 	Vec3<int> ineg, ipos;
// 	int ii, jj, kk;
// 	ineg.set(int(floor(neg.x)), int(floor(neg.y)), int(floor(neg.z)));
// 	ipos.set(int(ceil(pos.x)), int(ceil(pos.y)), int(ceil(pos.z)));
// 
// 	// Set up progress indicator
// 	count = ( (ipos.x - ineg.x) + 1) * ( (ipos.y - ineg.y) + 1) * ( (ipos.z - ineg.z) + 1);
// 	aten.initialiseProgress("Creating cell copies...", count);
// 
// 	// Create cell copies
// 	count = 0;
// 	stop = FALSE;
// 	for (ii = 0; ii <= (ipos.x - ineg.x); ii++)
// 	{
// 		for (jj = 0; jj <= (ipos.y - ineg.y); jj++)
// 		{
// 			for (kk = 0; kk <= (ipos.z - ineg.z); kk++)
// 			{
// 				// Set base translation vector for this replication
// 				tvec = oldaxes.rows[0] * ii;
// 				tvec += oldaxes.rows[1] * jj;
// 				tvec += oldaxes.rows[2] * kk;
// 				//tvec.print();
// 				clip.pasteToModel(this,tvec);
// 				msg.print(Messenger::Verbose,"Created copy for vector %8.4f %8.4f %8.4f\n",tvec.x,tvec.y,tvec.z);
// 				if (!aten.updateProgress(++count))
// 				{
// 					stop = TRUE;
// 					break;
// 				}
// 			}
// 			if (stop) break;
// 		}
// 		if (stop) break;
// 	}
// 	aten.cancelProgress();
// 
// 	// Deselect all atoms
// 	selectNone();
// 
// 	// Now trim off atoms that are outside the new cell
// 	if (prefs.replicateTrim())
// 	{
// 		bool delatom;
// 		Atom *i, *j;
// 		Vec3<double> fracr;
// 		Mat3<double> cellinverse = cell_.inverseTranspose();
// 	
// 		aten.initialiseProgress("Trimming excess atoms...", atoms_.nItems());
// 		i = atoms_.first();
// 		count = 0;
// 		while (i != NULL)
// 		{
// 			delatom = FALSE;
// 			// Convert coordinates to fractional coords and test them
// 			fracr = cellinverse * i->r();
// 			if ((fracr.x < -0.001) || (fracr.x >= 1.001)) delatom = TRUE;
// 			else if ((fracr.y < -0.001) || (fracr.y >= 1.001)) delatom = TRUE;
// 			else if ((fracr.z < -0.001) || (fracr.z >= 1.001)) delatom = TRUE;
// 			if (delatom)
// 			{
// 				j = i->next;
// 				deleteAtom(i);
// 				i = j;
// 			}
// 			else i = i->next;
// 			if (!aten.updateProgress(++count)) break;
// 		}
// 		aten.cancelProgress();
// 	}
// 
// 	changeLog.add(Log::Structure);
// 	msg.exit("Model::replicateCell");
// }

// Replicate Cell
void Model::replicateCell(const Vec3<double> &neg, const Vec3<double> &pos)
{
	msg.enter("Model::replicateCell");
	int count;
	bool stop;
	Vec3<double> tvec;
	Mat3<double> newaxes, oldaxes;
	Clipboard originalClip, clip;

	// If this isn't a periodic model, exit
	if (cell_.type() == Cell::NoCell)
	{
		msg.print("No cell to replicate.\n");
		msg.exit("Model::replicateCell");
		return;
	}

	// Perform an atomic fold on the crystal before we begin
	if (prefs.replicateFold()) foldAllAtoms();

	// Copy model to clipboard ready for pasting
	clip.copyAll(this);

	// Create new unit cell
	oldaxes = cell_.axes();
	newaxes = oldaxes;

	// Set new unit cell dimensions
	tvec.set(pos.x-neg.x, pos.y-neg.y, pos.z-neg.z);
	newaxes.rowMultiply(tvec);
	setCell(newaxes);

	// Paste in whole copies of the original cell - don't worry about fractional cells yet
	Vec3<int> ineg, ipos;
	int ii, jj, kk;
	ineg.set(int(floor(neg.x)), int(floor(neg.y)), int(floor(neg.z)));
	ipos.set(int(ceil(pos.x))-1, int(ceil(pos.y))-1, int(ceil(pos.z))-1);

	// Set up progress indicator
	count = ( (ipos.x - ineg.x) + 1) * ( (ipos.y - ineg.y) + 1) * ( (ipos.z - ineg.z) + 1);
	aten.initialiseProgress("Creating cell copies...", count);

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
				tvec = oldaxes.rows[0] * ii;
				tvec += oldaxes.rows[1] * jj;
				tvec += oldaxes.rows[2] * kk;
				//tvec.print();
				clip.pasteToModel(this,tvec);
				msg.print(Messenger::Verbose,"Created copy for vector %8.4f %8.4f %8.4f\n",tvec.x,tvec.y,tvec.z);
				if (!aten.updateProgress()) stop = TRUE;
			}
			if (stop) break;
		}
		if (stop) break;
	}
	aten.cancelProgress();

	// Select all atoms and shift if negative replication values were provided
	selectAll();
	tvec = oldaxes.rows[0] * -neg.x;
	tvec += oldaxes.rows[1] * -neg.y;
	tvec += oldaxes.rows[2] * -neg.z;
	translateSelectionLocal(tvec);
	selectNone();

	// Now trim off atoms that are outside the new cell
	if (prefs.replicateTrim())
	{
		bool delatom;
		Atom *i, *j;
		Vec3<double> fracr;
		Mat3<double> cellinverse = cell_.inverseTranspose();
	
		aten.initialiseProgress("Trimming excess atoms...", atoms_.nItems());
		i = atoms_.first();
		count = 0;
		while (i != NULL)
		{
			delatom = FALSE;
			// Convert coordinates to fractional coords and test them
			fracr = cellinverse * i->r();
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
			if (!aten.updateProgress(++count)) break;
		}
		aten.cancelProgress();
	}

	changeLog.add(Log::Structure);
	msg.exit("Model::replicateCell");
}

// Rotate cell and contents
void Model::rotateCell(int axis, double angle)
{
	msg.enter("Model::rotateCell");
	if (cell_.type() == Cell::NoCell)
	{
		msg.print("This model has no cell, and so it can't be rotated.\n");
		msg.exit("Model::rotateCell");
		return;
	}
	Mat3<double> rotmat;
	if (axis == 0) rotmat.createRotationX(angle);
	else if (axis == 1) rotmat.createRotationY(angle);
	else if (axis == 2) rotmat.createRotationZ(angle);
	// Create new cell axes
	Mat3<double> axes = cell_.axes();
	axes *= rotmat;
// 	cell_.set(axes);
	Vec3<double> lengths, angles;
	lengths = cell_.lengths();
	angles = cell_.angles();
	double temp;
	temp = lengths.z;
	lengths.z = lengths.y;
	lengths.y = temp;
	temp = angles.z;
	angles.z = angles.y;
	angles.y = temp;
	cell_.set(lengths, angles);
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

// Return the density of the model
double Model::density() const
{
	return density_;
}
