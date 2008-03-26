/*
	*** Model cell functions
	*** src/model/cell.cpp
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
#include "classes/pattern.h"
#include "classes/clipboard.h"
#include "templates/vector3.h"
#include "templates/matrix3.h"
#include "base/spacegroup.h"
#include "base/generator.h"
#include "base/master.h"
#include "base/constants.h"
#include "parse/parser.h"
#include <math.h>
#include <iostream>

// Return pointer to unit cell structure
Cell *Model::cell()
{
	return &cell_;
}

// Sets the spacegroup of the model
void Model::setSpacegroup(int i)
{
	spacegroup_ = i;
}

// Sets the spacegroup setting
void Model::setSpacegroupSetting(int i)
{
	spacegroupSetting_ = i;
}

// Return the spacegroup of the model
int Model::spacegroup()
{
	return spacegroup_;
}

// Set cell (vectors)
void Model::setCell(Vec3<double> lengths, Vec3<double> angles)
{
	dbgBegin(Debug::Calls,"Model::setCell[vectors]");
	Vec3<double> oldlengths = cell_.lengths();
	Vec3<double> oldangles = cell_.angles();
	// Set new axes 
	cell_.set(lengths, angles);
	logChange(LOG_STRUCTURE);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		Change *newchange = recordingState_->addChange();
		newchange->set(UE_CELL, &oldlengths, &oldangles, &lengths, &angles);
	}
	dbgEnd(Debug::Calls,"Model::setCell[vectors]");
}

// Set cell (axes)
void Model::setCell(Mat3<double> axes)
{
	dbgBegin(Debug::Calls,"Model::setCell[axes]");
	Vec3<double> oldlengths = cell_.lengths();
	Vec3<double> oldangles = cell_.angles();
	// Set new axes 
	cell_.set(axes);
	logChange(LOG_STRUCTURE);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		Change *newchange = recordingState_->addChange();
		newchange->set(UE_CELL, &oldlengths, &oldangles, &cell_.lengths(), &cell_.angles());
	}
	dbgEnd(Debug::Calls,"Model::setCell[axes]");
}

// Remove cell
void Model::removeCell()
{
	dbgBegin(Debug::Calls,"Model::removeCell");
	cell_.reset();
	logChange(LOG_VISUAL);
	logChange(LOG_STRUCTURE);
	dbgEnd(Debug::Calls,"Model::removeCell");
}

// Fold All Atoms
void Model::foldAllAtoms()
{
	dbgBegin(Debug::Calls,"Model::foldAllAtoms");
	// Standard fold - individual atoms
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) cell_.fold(i);
	logChange(LOG_COORDS);
	dbgEnd(Debug::Calls,"Model::foldAllAtoms");
}

// Fold All Molecules
void Model::foldAllMolecules()
{
	dbgBegin(Debug::Calls,"Model::foldAllMolecules");
	int n,m;
	Atom *i, *first;
	Pattern *p;
	// Molecular fold - fold first atom, other in molecule are MIM'd to this point
	if (!autocreatePatterns())
	{
		msg(Debug::None,"Model::foldAllMolecules : Molecular fold cannot be performed without a valid pattern definition.\n");
		dbgEnd(Debug::Calls,"Model::foldAllMolecules");
		return;
	}
	p = patterns_.first();
	i = atoms_.first();
	while (p != NULL)
	{
		for (m=0; m<p->nMols(); m++)
		{
			for (n=0; n<p->nAtoms(); n++)
			{
				// If its the first atom, fold and store pointer. If not, MIM w.r.t. stored atom
				if (n == 0)
				{
					cell_.fold(i);
					first = i;
				}
				else i->r() = cell_.mim(i,first);
				i = i->next;
			}
		}
		p = p->next;
	}
	logChange(LOG_COORDS);
	dbgEnd(Debug::Calls,"Model::foldAllMolecules");
}

// Apply individual symmetry generator to current atom selection
void Model::pack(int gen)
{
	dbgBegin(Debug::Calls,"Model::pack[gen,atom]");
	Clipboard clip;
	Vec3<double> newr;
	int oldnatoms;
	if (gen == 0)
	{
		// Ignore this operator since it is the identity operator
		dbgBegin(Debug::Calls,"Model::pack[gen,atom]");
		return;
	}
	msg(Debug::Verbose,"...Applying generator '%s' (no. %i)\n", master.generators[gen].description, gen);
	// Store current number of atoms in model
	oldnatoms = atoms_.nItems();
	// Copy selection to clipboard
	clip.copySelection(this);
	clip.pasteToModel(this, FALSE);
	for (Atom *i = atoms_[oldnatoms]; i != NULL; i = i->next)
	{
		// Get the position of the newly-pasted atom
		newr = i->r();
		// Apply the rotation and translation
		newr *= master.generators[gen].rotation;
		newr +=  cell_.transpose() * master.generators[gen].translation;
		cell_.fold(newr);
		i->r() = newr;
	}
	dbgEnd(Debug::Calls,"Model::pack[gen,atom]");
}

// Apply model's spacegroup symmetry generators
void Model::pack()
{
	dbgBegin(Debug::Calls,"Model::pack");
	if (spacegroup_ == 0) msg(Debug::None,"No spacegroup defined in model - no packing will be performed.\n");
	else
	{
		msg(Debug::None,"Packing cell according to spacegroup '%s'...\n", master.spacegroups[spacegroup_].name);
		selectAll();
		for (int n=0; n<master.spacegroups[spacegroup_].nGenerators; n++)
			pack(master.spacegroups[spacegroup_].generators[n]);
		// Select overlapping atoms and delete
		selectOverlaps(0.1);
		selectionDelete();
	}
	dbgEnd(Debug::Calls,"Model::pack");
}

// Scale cell and contents (molecule COGs)
void Model::scaleCell(const Vec3<double> &scale)
{
	dbgBegin(Debug::Calls,"Model::scaleCell");
	Vec3<double> oldcog, newcog, newpos;
	Cell newcell;
	Mat3<double> newaxes;
	bool calcenergy;
	double olde, newe;
	int n,m;
	Atom *i;
	// First, make sure we have a cell and a valid pattern
	if (cell_.type() == CT_NONE)
	{
		msg(Debug::None,"No cell to scale.\n");
		dbgEnd(Debug::Calls,"Model::scaleCell");
		return;
	}
	if (!autocreatePatterns())
	{
		dbgEnd(Debug::Calls,"Model::scaleCell");
		return;
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
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		i = p->firstAtom();
		for (n=0; n<p->nMols(); n++)
		{
			// Get fractional coordinate COG of this molecule
			oldcog = p->calculateCog(this,n);
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
	// Calculate new energy before leaving...
	if (calcenergy)
	{
		newe = totalEnergy(this);
		msg(Debug::None,"Energy change was %12.7e %s\n", newe-olde, text_from_EU(prefs.energyUnit()));
	}
	// Set new cell and update model
	setCell(newaxes);
	logChange(LOG_COORDS);
	dbgEnd(Debug::Calls,"Model::scaleCell");
}

// Replicate Cell
void Model::replicateCell(const Vec3<double> &neg, const Vec3<double> &pos)
{
	dbgBegin(Debug::Calls,"Model::replicateCell");
	int count;
	bool stop;
	Vec3<double> tvec;
	Mat3<double> newaxes, oldaxes;

	// If this isn't a periodic model, exit
	if (cell_.type() == CT_NONE)
	{
		msg(Debug::None,"No cell to replicate.\n");
		dbgEnd(Debug::Calls,"Model::replicateCell");
		return;
	}

	// Create two clipboards - copy the original model to one of them
	Clipboard originalClip, clip;
	originalClip.copyAll(this);
	// Centre this clipboard copy to put the atoms at 0,0,0
	originalClip.translate(-cell_.centre());   // This does not put the atoms at 0.0.0! 

	// Create the new unit cell in the original model
	oldaxes = cell_.axes();
	// Copy model to clipboard ready for pasting
	clip.copyAll(this);
	// Take transpose of old and new axes for convenient multiplication
	newaxes = oldaxes;

	// Set new unit cell dimensions
	tvec.set(pos.x+1.0-neg.x, pos.y+1.0-neg.y, pos.z+1.0-neg.z);
	newaxes.rowMultiply(tvec);
	setCell(newaxes);

	// Clear the original model
	clear();

	// Re-centre the clipboard copy so it is at the new cell origin
	originalClip.translate(-cell_.centre());	// See above

	// Paste in whole copies of the original cell - don't worry about fractional cells yet
	Vec3<int> ineg, ipos;
	int ii, jj, kk;
	ineg.set(int(floor(neg.x)), int(floor(neg.y)), int(floor(neg.z)));
	ipos.set(int(ceil(pos.x)), int(ceil(pos.y)), int(ceil(pos.z)));

	// Set up progress indicator
	count = ( (ipos.x - ineg.x) + 1) * ( (ipos.y - ineg.y) + 1) * ( (ipos.z - ineg.z) + 1);
	master.initialiseProgress("Creating cell copies...", count);

	// Create cell copies
	count = 0;
	stop = FALSE;
	for (ii = 0; ii <= (ipos.x - ineg.x); ii++)
	{
		for (jj = 0; jj <= (ipos.y - ineg.y); jj++)
		{
			for (kk = 0; kk <= (ipos.z - ineg.z); kk++)
			{
				// Set base translation vector for this replication
				tvec = oldaxes.rows[0] * ii;
				tvec += oldaxes.rows[1] * jj;
				tvec += oldaxes.rows[2] * kk;
				//tvec.print();
				clip.pasteToModel(this,tvec);
				msg(Debug::Verbose,"Created copy for vector %8.4f %8.4f %8.4f\n",tvec.x,tvec.y,tvec.z);
				if (!master.updateProgress(++count))
				{
					stop = TRUE;
					break;
				}
			}
			if (stop) break;
		}
		if (stop) break;
	}
	master.cancelProgress();

	// Deselect all atoms
	selectNone();

	// Now trim off atoms that are outside the new cell
	bool delatom;
	Atom *i, *j;
	Vec3<double> fracr;
	Mat3<double> cellinverse = cell_.inverseTranspose();

	master.initialiseProgress("Trimming excess atoms_...", atoms_.nItems());
	i = atoms_.first();
	count = 0;
	while (i != NULL)
	{
		delatom = FALSE;
		// Convert coordinates to fractional coords and test them
		fracr = cellinverse * i->r();
		if ((fracr.x < 0) || (fracr.x >= 1)) delatom = TRUE;
		else if ((fracr.y < 0) || (fracr.y >= 1)) delatom = TRUE;
		else if ((fracr.z < 0) || (fracr.z >= 1)) delatom = TRUE;
		if (delatom)
		{
			j = i->next;
			deleteAtom(i);
			i = j;
		}
		else i = i->next;
		if (!master.updateProgress(++count)) break;
	}
	master.cancelProgress();

	logChange(LOG_STRUCTURE);
	dbgEnd(Debug::Calls,"Model::replicateCell");
}

// Frac to Real
void Model::fracToReal()
{
	dbgBegin(Debug::Calls,"Model::fracToReal");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) i->r() = cell_.fracToReal(i->r());
	dbgEnd(Debug::Calls,"Model::fracToReal");
}
