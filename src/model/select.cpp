/*
	*** Model select functions
	*** src/model/select.cpp
	Copyright T. Youngs 2007-2011

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
#include "model/undoevent.h"
#include "model/undostate.h"
#include "classes/neta_parser.h"
#include "base/pattern.h"
#include "base/progress.h"
#include "gui/gui.h"

// Return the number of selected atoms
int Model::nSelected() const
{
	return selection_.nItems();
}

// Return the number of marked atoms
int Model::nMarked() const
{
	return marked_.nItems();
}

// Mark all atoms in model
void Model::markAll()
{
	selectAll(TRUE);
	msg.print(Messenger::Verbose, "All atoms marked.\n");
}

// Match marked atoms to current selection
void Model::markSelectedAtoms()
{
	selectNone(TRUE);
	for (Refitem<Atom,int> *ri = selection_.first(); ri != NULL; ri = ri->next) selectAtom(ri->item, TRUE);
	msg.print(Messenger::Verbose, "There are now %i atoms marked.\n", marked_.nItems());
}

// Select marked atoms
void Model::selectMarkedAtoms()
{
	for (Refitem<Atom,int> *ri = marked_.first(); ri != NULL; ri = ri->next) selectAtom(ri->item);
}

// Select Atom
void Model::selectAtom(Atom *i, bool markonly)
{
	if (markonly)
	{
		if (!i->isSelected(TRUE))
		{
			i->setSelected(TRUE, TRUE);
			// Add at correct position in list
			Refitem<Atom,int> *ri = marked_.first();
			if (ri == NULL) marked_.add(i);
			else if (ri->item->id() > i->id()) marked_.addStart(i);
			else
			{
				for (ri = marked_.last(); ri != NULL; ri = ri->prev) if (ri->item->id() < i->id()) break;
				marked_.addAfter(ri,i);
			}
		}
	}
	else
	{
		if (!i->isSelected())
		{
			i->setSelected(TRUE);
			selection_.add(i);
			changeLog.add(Log::Selection);
			// Add the change to the undo state (if there is one)
			if (recordingState_ != NULL)
			{
				SelectEvent *newchange = new SelectEvent;
				newchange->set(TRUE, i->id());
				recordingState_->addEvent(newchange);
			}
		}
	}
}

// Select Atom by ID
void Model::selectAtom(int id, bool markonly)
{
	if ((id < 0) || (id >= atoms_.nItems()))
	{
		printf("Internal Error: Invalid atom id %i requested for %s (nAtoms = %i)\n", id, markonly ? "marking" : "selection", atoms_.nItems());
	}
	Atom *i = atom(id);
	if (i != NULL) selectAtom(i, markonly);
}

// Deselect Atom
void Model::deselectAtom(Atom *i, bool markonly)
{
	if (markonly)
	{
		if (i->isSelected(TRUE))
		{
			i->setSelected(FALSE, TRUE);
			marked_.remove(i);
		}
	}
	else
	{
		if (i->isSelected())
		{
			i->setSelected(FALSE);
			selection_.remove(i);
			changeLog.add(Log::Selection);
			// Add the change to the undo state (if there is one)
			if (recordingState_ != NULL)
			{
				SelectEvent *newchange = new SelectEvent;
				newchange->set(FALSE, i->id());
				recordingState_->addEvent(newchange);
			}
		}
	}
}

// Deselect Atom by ID
void Model::deselectAtom(int id, bool markonly)
{
	if ((id < 0) || (id >= atoms_.nItems()))
	{
		printf("Internal Error: Invalid atom id %i requested for %s (nAtoms = %i)\n", id, markonly ? "unmarking" : "deselection", atoms_.nItems());
	}
	Atom *i = atom(id);
	if (i != NULL) deselectAtom(i, markonly);
}

// Toggle Selection State
void Model::selectionToggle(Atom *i, bool markonly)
{
	i->isSelected(markonly) ? deselectAtom(i, markonly) : selectAtom(i, markonly);
}

// Invert Current Selection
void Model::selectionInvert(bool markonly)
{
	msg.enter("Model::selectionInvert");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		i->isSelected(markonly) ? deselectAtom(i, markonly) : selectAtom(i, markonly);
	msg.exit("Model::selectionInvert");
}

// Delete Selected Atoms
void Model::selectionDelete(bool markonly)
{
	msg.enter("Model::selectionDelete");
	Atom *i, *tempi;
	int count = 0;
	bool cancelled = FALSE;
	// Attempt to be clever here for the sake of undo/redo, while avoiding renumbering at every step.
	// 1) First, delete all measurements and bonds to the selected atoms
	Refitem<Bond,int> *bref;
	for (Refitem<Atom,int> *ri = selection(markonly); ri != NULL; ri = ri->next)
	{
		i = ri->item;
		// Remove measurements
		removeMeasurements(i);
		// Delete All Bonds To Specific Atom
		bref = i->bonds();
		while (bref != NULL)
		{
			// Need to detach the bond from both atoms involved
			Bond *b = bref->item;
			Atom *j = b->partner(i);
			unbondAtoms(i,j,b);
			bref = i->bonds();
		}
		if (cancelled) break;
	}
	// 2) Delete the actual atoms
	if (!cancelled)
	{
		i = atoms_.last();
		while (i != NULL)
		{
			if (i->isSelected(markonly))
			{
				tempi = i->prev;
				removeAtom(i, TRUE);
				i = tempi;
			}
			else i = i->prev;
		}
	}
	// Renumber remaining atoms
	renumberAtoms();
	msg.exit("Model::selectionDelete");
}

// Expand Current Selection
void Model::selectionExpand(bool markonly)
{
	msg.enter("Model::selectionExpand");
	Atom *i;
	Refitem<Bond,int> *bref;
	// Store the current selection state in i->tempi
	for (i = atoms_.first(); i != NULL; i = i->next) i->tempi = i->isSelected(markonly);
	// Now use the temporary state to find atoms where we select atomic neighbours
	for (i = atoms_.first(); i != NULL; i = i->next)
		if (i->tempi) for (bref = i->bonds(); bref != NULL; bref = bref->next) selectAtom(bref->item->partner(i), markonly);
	msg.exit("Model::selectionExpand");
}

// Select All Atoms
void Model::selectAll(bool markonly)
{
	msg.enter("Model::selectAll");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (!i->isSelected(markonly)) selectAtom(i, markonly);
	msg.exit("Model::selectAll");
}

// Deselect All Atoms
void Model::selectNone(bool markonly)
{
	msg.enter("Model::selectNone");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected(markonly)) deselectAtom(i, markonly);
	msg.exit("Model::selectNone");
}

// Atom at Screen Coordinates
Atom *Model::atomOnScreen(double x1, double y1)
{
	// See if an atom exists under the coordinates x1,y1
	// Ignore hidden atoms.
	msg.enter("Model::atomOnScreen");
	Atom *closest = NULL;
	Vec3<double> wr;
	Vec4<double> sr;
	double closestz = 10000.0, dist, nclip = prefs.clipNear();
	y1 = gui.mainCanvas()->contextHeight() - y1;
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isHidden()) continue;
		wr = -modelToWorld(i->r(), &sr, prefs.styleRadius(i));
		if (wr.z > nclip)
		{
			dist = sqrt((sr.x - x1)*(sr.x - x1) + (sr.y - y1)*(sr.y - y1));
			if (dist < sr.w)	// Mouse is inside bounding sphere
			{
				if ((closest == NULL) || (wr.z < closestz))
				{
					closest = i;
					closestz = wr.z;
				}
			}
		}
	}
	msg.exit("Model::atomOnScreen");
	return closest;
}

// Select atoms within bounding box
void Model::selectBox(double x1, double y1, double x2, double y2, bool deselect)
{
	// Box selection - choose all the atoms within the selection area
	msg.enter("Model::selectBox");
	double t;
	Atom *i;
	Vec4<double> sr;
	y1 = gui.mainCanvas()->contextHeight() - y1;
	y2 = gui.mainCanvas()->contextHeight() - y2;
	// Handle 'reverse ranges' - make sure x1 < x2 and y1 < y2
	if (x1 > x2)
	{
		t=x1;
		x1=x2;
		x2=t;
	}
	if (y1 > y2)
	{
		t=y1;
		y1=y2;
		y2=t;
	}
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isHidden()) continue;
		modelToWorld(i->r(), &sr);
		if ((sr.x >= x1) && (sr.x <= x2) && (sr.y >= y1) && (sr.y <= y2)) (deselect ? deselectAtom(i) : selectAtom(i));
	}
	msg.exit("Model::selectBox");
}

// Tree Select
void Model::selectTree(Atom *i, bool markonly, bool deselect, Bond *omitbond)
{
	// The passed atom node is the starting point for the algorithm.
	// From here, select all atoms that are bound - if they are already
	// selected then ignore them. If they are not already selected, then
	// recursively call the routine on that atom.
	msg.enter("Model::selectTree");
	bool status;
	Atom *j;
	deselect ? deselectAtom(i, markonly) : selectAtom(i, markonly);
	for (Refitem<Bond,int> *bref = i->bonds(); bref != NULL; bref = bref->next)
	{
		if (bref->item == omitbond) continue;
		j = bref->item->partner(i);
		status = j->isSelected(markonly);
		if (deselect) status = !status;
		if (!status)
		{
			if (deselect) deselectAtom(j, markonly);
			else selectAtom(j, markonly);
			this->selectTree(j, markonly, deselect, omitbond);
		}
	}
	msg.exit("Model::selectTree");
}

// Select by Element
void Model::selectElement(Atom *target, bool markonly, bool deselect)
{
	// Select all atoms which are the same element as the atom i
	selectElement(target->element(), markonly, deselect);
}

// Select by element (from element)
void Model::selectElement(int el, bool markonly, bool deselect)
{
	// Select all atoms which are the same element as the atom with id 'target'
	msg.enter("Model::selectElement");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		if (i->element() == el) (deselect ? deselectAtom(i, markonly) : selectAtom(i, markonly));
	msg.exit("Model::selectElement");
}

// Deselect by Element
void Model::deselectElement(int el, bool markonly)
{
	// Select all atoms which are the same element as the atom i
	msg.enter("Model::deselectElement");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		if (i->element() == el) deselectAtom(i, markonly);
	msg.exit("Model::deselectElement");
}

// Select all atoms which match the provided type
int Model::selectType(int element, const char *typedesc, bool markonly, bool deselect)
{
	msg.enter("Model::selectType");
	Neta testat;
	testat.setCharacterElement(element);
	if (!netaparser.createNeta(&testat, typedesc, NULL))
	{
		msg.print("Failed to create type description.\n");
		msg.exit("Model::selectType");
		return -1;
	}
	int count = 0, matchscore = 0, atomscore, n;
	// Prepare for typing
	describeAtoms();
	// Loop over patterns and select atoms
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		Atom *i = p->firstAtom();
		for (n=0; n<p->nAtoms(); n++)
		{
			p->resetTempI(0);
			i->tempi = 1;
			atomscore = testat.matchAtom(i,p->ringList(),this);
			if (atomscore > 0)
			{
				// Select this atom in all pattern molecules
				p->selectAtom(n, markonly, deselect);
				count += p->nMolecules();
				matchscore = atomscore;
			}
			i = i->next;
		}
	}
	// Update model
	changeLog.add(Log::Selection);
	// Write results
	msg.print("Type description score = %i. Matched %i atoms.\n", matchscore, count);
	msg.exit("Model::selectType");
	return count;
}

// Select with bounding Sphere
void Model::selectRadial(Atom *target, double radius)
{
	// Select all atoms which are within the distance 'radius' from atom 'target'
	msg.enter("Model::selectRadial");
	Atom *i = atoms_.first();
	printf("Selection radius is %8.4f Angstroms\n",radius);
	while (i != NULL)
	{
		if (i == target) selectAtom(i);
		else if (distance(target,i) < radius) selectAtom(i);
		i = i->next;
	}
	msg.exit("Model::selectRadial");
}

// Select all atoms in specified pattern
void Model::selectPattern(Pattern *p, bool markonly, bool deselect)
{
	// Select all atoms covered by the specified pattern.
	msg.enter("Model::selectPattern");
	// Check that this pattern is valid and belongs to this model...
	bool found = FALSE;
	for (Pattern *modelp = patterns_.first(); modelp != NULL; modelp = modelp->next) if (p == modelp) found = TRUE;
	if (!found)
	{
		msg.print("Pattern does not belong to this model, or is out of date.\n");
		msg.exit("Model::selectPattern");
		return;
	}
	Atom *i = p->firstAtom();
	for (int n=0; n<p->totalAtoms(); n++)
	{
		deselect ? deselectAtom(i, markonly) : selectAtom(i, markonly);
		i = i->next;
	}
	msg.exit("Model::selectPattern");
}

// Get first selected
Refitem<Atom,int> *Model::selection(bool markonly) const
{
	if (markonly) return marked_.first();
	else return selection_.first();
}

// Return the nth selected atom in the model
Refitem<Atom,int> *Model::selected(int n)
{
	if ((n<0) || (n>=selection_.nItems())) printf("Array index for selection_ is out of bounds : %i.\n", n);
	else return selection_[n];
	return NULL;
}

// Select overlapping atoms
void Model::selectOverlaps(double tolerance, bool markonly)
{
	msg.enter("Model::selectOverlaps");
	int n, m, x, y, z, x2, y2, z2, checklist[8], count;
	double dist;
	Atom *i, *j;
	selectNone(markonly);
	// Create cuboid lists
	initialiseBondingCuboids();
	// Add all atoms to cuboid list
	for (i = atoms_.first(); i != NULL; i = i->next) addAtomToCuboid(i);
	// Loop over cuboids, checking distances with atoms in adjacent boxes
	Refitem<Atom,double> *ri, *rj;
	x = 0;
	y = 0;
	z = 0;
	count = 0;
	for (n = 0; n<nCuboids_; n++)
	{
		if (bondingCuboids_[n].nItems() != 0)
		{
// 			if (bondingCuboids_[n].nItems() > 0) printf("On cuboid %i (%ix%ix%i) which contains %i atoms\n", n, x, y, z, bondingCuboids_[n].nItems());
			// For each of the atoms in the cuboid, check distance with each atom in eight of the closest
			// overlay boxes. 
			checklist[0] = n;
			x2 = (x == (cuboidBoxes_.x-1) ? 0 : x+1);
			y2 = (y == (cuboidBoxes_.y-1) ? 0 : y+1);
			z2 = (z == (cuboidBoxes_.z-1) ? 0 : z+1);
// 			if (bondingCuboids_[n].nItems() > 0) printf("....xyz = %i,%i,%i, xyz2 = %i,%i,%i\n", x, y, z, x2, y2, z2);
			checklist[1] = x2*cuboidYZ_+y*cuboidBoxes_.z+z;
			checklist[2] = x2*cuboidYZ_+y2*cuboidBoxes_.z+z;
			checklist[3] = x2*cuboidYZ_+y*cuboidBoxes_.z+z2;
			checklist[4] = x2*cuboidYZ_+y2*cuboidBoxes_.z+z2;
			checklist[5] = x*cuboidYZ_+y2*cuboidBoxes_.z+z;
			checklist[6] = x*cuboidYZ_+y*cuboidBoxes_.z+z2;
			checklist[7] = x*cuboidYZ_+y2*cuboidBoxes_.z+z2;
			for (ri = bondingCuboids_[n].first(); ri != NULL; ri = ri->next)
			{
				i = ri->item;
				if (i->isSelected(markonly)) continue;
				for (m=0; m<8; m++)
				{
					for (rj = bondingOverlays_[checklist[m]].first(); rj != NULL; rj = rj->next)
					{
						j = rj->item;
						if ((j->isSelected(markonly)) || (i == j)) continue;
						dist = cell_.distance(i,j);
						if (dist < tolerance)
						{
							msg.print(Messenger::Verbose, "Atom %i (%s) is %f from atom %i (%s).\n", j->id()+1, elements().symbol(j), dist, i->id()+1, elements().symbol(i));
							selectAtom(j, markonly);
							++count;
						}
					}
				}
			}
		}
		// Increase x,y,z indices for box lookup
		z ++;
		if (z == cuboidBoxes_.z)
		{
			z = 0;
			y ++;
			if (y == cuboidBoxes_.y)
			{
				y = 0;
				x ++;
			}
		}
	}
	// Free bonding cuboids
	freeBondingCuboids();
	msg.print("%i overlapping atoms selected.\n", count);
	msg.exit("Model::selectOverlaps");
}


// Select atoms (or molecule COGs) inside of the current unit cell
void Model::selectInsideCell(bool moleculecogs, bool markonly)
{
	msg.enter("Model::selectInsideCell");
	Vec3<double> pos;
	// If using molecule COGs, need a valid pattern definition
	if (moleculecogs)
	{
		createPatterns();
		int m,n,id = 0;
		for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
		{
			for (m=0; m<p->nMolecules(); ++m)
			{
				// Get COG of molecule
				pos = cell_.realToFrac(p->calculateCog(m));
				if ((pos.x < 1) && (pos.y < 1) && (pos.z < 1)) for (n=0; n<p->nAtoms(); ++n) selectAtom(id+n, markonly);
				id += p->nAtoms();
			}
		}
	}
	else
	{
		for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		{
			pos = cell_.realToFrac(i->r());
			if ((pos.x < 1) && (pos.y < 1) && (pos.z < 1)) selectAtom(i, markonly);
		}
	}
	msg.exit("Model::selectInsideCell");
}

// Select atoms (or molecule COGs) outside of the current unit cell
void Model::selectOutsideCell(bool moleculecogs, bool markonly)
{
	msg.enter("Model::selectOutsideCell");
	Vec3<double> pos;
	// If using molecule COGs, need a valid pattern definition
	if (moleculecogs)
	{
		createPatterns();
		int m,n,id = 0;
		for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
		{
			for (m=0; m<p->nMolecules(); ++m)
			{
				// Get COG of molecule
				pos = cell_.realToFrac(p->calculateCog(m));
				if ((pos.x > 1) || (pos.y > 1) || (pos.z > 1)) for (n=0; n<p->nAtoms(); ++n) selectAtom(id+n, markonly);
				id += p->nAtoms();
			}
		}
	}
	else
	{
		for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		{
			pos = cell_.realToFrac(i->r());
			if ((pos.x > 1) || (pos.y > 1) || (pos.z > 1)) selectAtom(i, markonly);
		}
	}
	msg.exit("Model::selectOutsideCell");
}

// Perform a Miller 'selection' on the cell contents
void Model::selectMiller(int h, int k, int l, bool inside, bool markonly)
{
	msg.enter("Model::selectMiller");
	if (cell_.type() == UnitCell::NoCell)
	{
		msg.print("Can't use Miller planes on a non-periodic model.\n");
		msg.exit("Model::selectMiller");
		return;
	}
	double c, d;
	Vec3<double> hkl(h == 0 ? 0 : 1.0/h, k == 0 ? 0 : 1.0/k, l == 0 ? 0 : 1.0/l), r;
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		r = cell_.realToFrac(i->r());
		c = r.dp(hkl);
		r.set(1-r.x,1-r.y,1-r.z);
		d = r.dp(hkl);
		if (inside) { if ((c > 1.0) && (d > 1.0)) selectAtom(i, markonly); }
		else if ((c < 1.0) || (d < 1.0)) selectAtom(i, markonly);
	}
	msg.exit("Model::selectMiller");
}

// Select atoms within distance from a line (i.e. cylinder select)
void Model::selectLine(Vec3<double> line, Vec3<double> point, double dr, bool markonly)
{
	msg.enter("Model::selectLine");
	// See: A Programmers Geometry, Bowyer and Woodwark, Butterworths (pub.), 1983, p99
	// Line equation is :
	//		x = point.x + line.x * t
	//		y = point.y + line.y * t
	//		z = point.z + line.z * t
	// Check line parameters
	double denom = line.dp(line);
	if (denom < 1.0e-6)
	{
		msg.print("Line parameters appear to be corrupt.\n");
		msg.exit("Model::selectLine");
		return;
	}
	line.normalise();
	Vec3<double> r, v, dummy, origin;
	double xyyx, xzzx, yzzy, dist;
	for (int pass = 0; pass < 4; ++pass)
	{
		origin = point;
		if (pass > 0) origin += cell_.axes().columnAsVec3(pass-1);
		for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		{
			if (i->isSelected()) continue;
			r = i->r() - origin;
			xyyx = line.x*r.y - line.y*r.x;
			xzzx = line.x*r.z - line.z*r.x;
			yzzy = line.y*r.z - line.z*r.y;
			v.x = line.y*xyyx + line.z*xzzx;
			v.y = line.z*yzzy - line.x*xyyx;
			v.z = -line.x*xzzx - line.y*yzzy;
			v = cell_.mimVector(v, dummy);
			dist = v.magnitude();
			if (dist < dr) selectAtom(i, markonly);
		}
	}

}
