/*
	*** Model view functions
	*** src/model/build.cpp

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
#include "base/elements.h"

/*
// Sketching functions
*/

// Add hydrogens to model
void model::hydrogen_satisfy()
{
	// Cycles over atoms in model, and works out how many hydrogens (and in which geometry) should be added to each
	dbg_begin(DM_CALLS,"model::hydrogen_satisfy");
	int numh, tbo, nsingle, ndouble;
	atom *i = atoms.first();
	while (i != NULL)
	{
		// Step 1 - Work out how many single-bonds (i.e. hydrogens) we need to add to satisfy the atom's valency
		// Calculate total bond order of atom and work out single bond deficit
		tbo = i->total_bond_order();
		numh = (elements.valency(i)*2 - tbo) / 2;
		// Step 2 - Work out geometry that we'll add hydrogens in, based on the atom's valency
		nsingle = i->count_bonds(BT_SINGLE);
		ndouble = i->count_bonds(BT_DOUBLE);
		if (numh != 0)
		{
			// Simplest cases - atom has no bonds or all single bonds - we add in a tetrahedral geometry
			if (i->get_nbonds() == 0 || i->get_nbonds() == nsingle) i->add_hydrogens(numh,HG_TETRAHEDRAL,this);
			// Otherwise, must work out the correct geometry to add hydrogens in...
			else if (ndouble != 0) i->add_hydrogens(numh,HG_PLANAR,this);
			else i->add_hydrogens(numh,HG_LINEAR,this);
		}
		i = i->next;
	}
	set_atom_colours(NULL);
	project_all();
	dbg_end(DM_CALLS,"model::hydrogen_satisfy");
}
