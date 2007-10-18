/*
	*** Model pattern functions
	*** src/model/pattern.cpp

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
#include "classes/atomaddress.h"
#include "base/master.h"
#include "base/elements.h"

// Add Pattern Node
pattern *model::add_pattern(int mols, int numatoms, const char *patname)
{
	dbg_begin(DM_CALLS,"model::add_pattern");
	// Determine starting atom...
	pattern *lastp = patterns.last();
	int start = (lastp == NULL ? 0 : lastp->get_startatom() + lastp->get_nmols() * lastp->get_natoms());
	pattern *newpnode = patterns.add();
	newpnode->set_owner(this);
	newpnode->set_name(patname);
	newpnode->initialise(patterns.size()-1,start,mols,numatoms);
	msg(DM_VERBOSE,"New pattern '%s' added - startatom %i, %i mols, %i atoms per mol.\n",patname,start,mols,numatoms);
	if ((start + mols*numatoms) == atoms.size())
	{
		msg(DM_NONE,"Pattern description completed (spans %i atoms).\n",atoms.size());
		energy.resize(patterns.size());
		int npat = make_plist();
		// Create representative molecules
		msg(DM_NONE,"Creating representative molecules...");
		create_pattern_molecules();
		msg(DM_NONE,"Done.\n");
		// Patterns depend only on the properties / relation of the atoms, and not the positions..
		patterns_point = logs[LOG_STRUCTURE];
	}
	else if ((start + mols*numatoms) > atoms.size()) msg(DM_NONE,"New pattern '%s' extends %i atoms past number of atoms in owner model.\n",patname,(start + mols*numatoms) - atoms.size());
	dbg_end(DM_CALLS,"model::add_pattern");
	return newpnode;
}

// Cut pattern
void model::cut_pattern(pattern *source)
{
	patterns.cut(source);
}

// Own pattern
void model::own_pattern(pattern *source, bool own)
{
	dbg_begin(DM_CALLS,"model::add_pattern");
	// Set the starting atom from the last pattern in the model's list
	pattern *p = patterns.last();
	int start = (p == NULL ? 0 : p->get_startatom() + p->get_nmols() * p->get_natoms());
	// Add the pattern onto the end of the current list
	patterns.own(source);
	energy.resize(patterns.size());
	// Set startatom and endatom to be coherent with the models current list
	source->set_contents(start,-1,-1);
	source->set_id(patterns.size()-1);
	//source->set_id(patterns.size()-1);
	if (own) source->set_owner(this);
	dbg_end(DM_CALLS,"model::add_pattern");
}

// Create pointer list of patterns
int model::make_plist()
{
	// Make an array of pattern pointers for convenience. Returns the number of patterns in the list.
	dbg_begin(DM_CALLS,"model::make_plist");
	if (plist != NULL) delete[] plist;
	// Determine the size of the new list first
	pattern *p = patterns.first();
	int count = 0;
	while (p != NULL)
	{
		if (!p->is_fixed()) count ++;
		p = p->next;
	}
	plist = new pattern*[count];
	// Fill the list
	p = patterns.first();
	count = 0;
	while (p != NULL)
	{
		if (!p->is_fixed())
		{
			plist[count] = p;
			count ++;
		}
		p = p->next;
	}
	dbg_end(DM_CALLS,"model::make_plist");
	return count;
}

// Set 'fixed' propety of patterns
void model::set_patterns_fixed(int upto)
{
	// Set all patterns in the current model to be 'fixed'
	dbg_begin(DM_CALLS,"model::set_patterns_fixed");
	pattern *p = patterns.first();
	int count = 0;
	while (p != NULL)
	{
		if (count == upto) break;
		p->set_fixed(TRUE);
		p = p->next;
		count ++;
	}
	dbg_end(DM_CALLS,"model::set_patterns_fixed");
}

// Determine the locality of the supplied atom
atomaddress *model::locate_atom(atom *i)
{
	dbg_begin(DM_CALLS,"model::locate_atom");
	int n, patternno, molno, atomno, id;
	pattern *p;
	if (!autocreate_patterns())
	{
		msg(DM_NONE,"model::locate_atom : No valid pattern available for model.\n");
		dbg_end(DM_CALLS,"model::locate_atom");
		return NULL;
	}
	id = i->get_id();
	// First, find the pattern the atom is covered by
	patternno = -1;
	p = patterns.first();
	while (p != NULL)
	{
		if ((id >= p->get_startatom()) && (id <= p->get_endatom()))
		{
			patternno = p->get_id();
			break;
		}
		p = p->next;
	}
	if (patternno == -1)
	{
		printf("Fatal error - could not find owner pattern for atom!\n");
		dbg_end(DM_CALLS,"model::locate_atom");
		return NULL;
	}
	// Next, find its molecule id
	id -= p->get_startatom();
	molno = id / p->get_natoms();
	// Finally, get the atom offset
	atomno = id % p->get_natoms();
	// Create structure, store values, and return
	atomaddress *newaddress = new atomaddress;
	newaddress->set_pattern(p);
	newaddress->set_molecule(molno);
	newaddress->set_offset(atomno);
	dbg_end(DM_CALLS,"model::locate_atom");
	return newaddress;
}

// Clar patterns
void model::clear_patterns()
{
	dbg_begin(DM_CALLS,"model::clear_patterns");
	patterns.clear();
	patterns_point = -1;
	expression_point = -1;
	msg(DM_NONE,"Pattern list cleared for model '%s'.\n",name.get());
	dbg_end(DM_CALLS,"model::clear_patterns");
}

// Autocreate patterns
bool model::autocreate_patterns()
{
	// Determine the pattern (molecule) layout of the model
	dbg_begin(DM_CALLS,"model::autocreate_patterns");
	int n, atomid, nsel2, lastnatoms, nmols, npat, laststart, totalselected;
	bool same;
	//string atomdna, bonddna, lastatomdna, lastbonddna, emp, lastemp;
	static dnchar atomdna, bonddna, lastatomdna, lastbonddna, emp, lastemp;
	atomdna.create_empty(1024);
	bonddna.create_empty(1024);
	lastatomdna.create_empty(1024);
	lastbonddna.create_empty(1024);
	emp.create_empty(1024);
	lastemp.create_empty(1024);
	pattern *p;
	atom *i, *j;
	// Check current pattern first...
	if (patterns_are_valid())
	{
		dbg_end(DM_CALLS,"model::autocreate_patterns");
		return TRUE;
	}
	// Delete all old nodes first.
	msg(DM_NONE,"Autodetecting patterns for model '%s'..\n",name.get());
	patterns.clear();
	reset_tempi(0);
	totalselected = 0;
	// If there are no atoms in the molecule, exit here.
	if (atoms.size() == 0)
	{
		msg(DM_NONE,"No patterns defined for model '%s' - no atoms present.\n",name.get());
		patterns_point = logs[LOG_STRUCTURE];
		dbg_end(DM_CALLS,"model::autocreate_patterns");
		return TRUE;
	}
	// To autodetect, we start off at atoms_head in the model, tree-select this atom and copy the selection to the clipboard. With the reflist so created on the clipboard, we make a 'fingerprint' of the selection from the element symbols of the atoms (in order). This is then the criteria which we match subsequent selections. Find the next 'not-yet-selected' atom and perform another tree selection. Check against the previous fingerprint, and if its the same just increase the nmols counter by one. If it's different, assume its the start of a new type of molecule and reset the counters.
	lastnatoms = 0;
	laststart = 0;
	lastatomdna = "_NULL_";
	lastemp = "_NULL_";
	lastbonddna = "_NULL_";
	atomid = 0;
	nmols = 0;
	i = atoms.first();
	while (atomid != atoms.size())
	{
		select_none();
		// Select molecule starting at atom 'i' and calculate fingerprint
		select_tree(i);
		selection_get_atom_fingerprint(atomdna);
		selection_get_bond_fingerprint(bonddna);
		selection_get_empirical(emp);
		// Compare this with the previous fingerprints
		same = TRUE;
		if (lastnatoms != nselected) same = FALSE;
		if (atomdna != lastatomdna) same = FALSE;
		if (bonddna != lastbonddna) same = FALSE;
		if (same) nmols ++;
		else
		{
			// Not the same as the last stored pattern, so start a new one
			// Store the old pattern data first (if there was one)
			if (lastnatoms != 0)
			{
				msg(DM_VERBOSE,"New pattern found:\n");
				msg(DM_VERBOSE,"Pattern Atom DNA = [%s]\n",lastatomdna.get());
				msg(DM_VERBOSE,"Pattern Bond DNA = [%s]\n",lastbonddna.get());
				p = add_pattern(nmols,lastnatoms,lastemp.get());
				// Check the newly-created pattern
				//if (!p->validate()) result = FALSE;
			}
			laststart = atomid;
			lastnatoms = nselected;
			lastatomdna = atomdna;
			lastbonddna = bonddna;
			lastemp = emp;
			nmols = 1;
		}
		// We insist that the molecule consists of consecutively ordered atoms, otherwise we can't proceed, so count the number of selected
		// atoms in those that we now skip (if != nselected then we must force a 1*N pattern)
		nsel2 = 0;
		atomid += nselected;
		for (n=0; n<nselected; n++)
		{
			if (i->is_selected()) nsel2 ++;
			i = i->next;
		}
		if (nsel2 != nselected)
		{
			msg(DM_NONE,"Warning - model cannot be divided into molecules because of non-ordered atoms.\nPattern for model will be 1*N.\n");
			// Remove any patterns added so far and set values so we create a generic 1*N pattern instead
			patterns.clear();
			laststart = 0;
			nmols = 1;
			lastnatoms = atoms.size();
			lastemp = "All";
			lastatomdna = "---";
			lastbonddna = "---";
			break;
		}
	}
	// Store last pattern data
	msg(DM_VERBOSE,"Pattern Name = %s\n",lastemp.get());
	msg(DM_VERBOSE,"Pattern Atom DNA = [%s]\n",lastatomdna.get());
	msg(DM_VERBOSE,"Pattern Bond DNA = [%s]\n",lastbonddna.get());
	p = add_pattern(nmols,lastnatoms,lastemp.get());

	// Patterns depend only on the properties / relation of the atoms, and not the positions..
	patterns_point = logs[LOG_STRUCTURE];

	// Update pattern lists
	//gui.update_pattern_list(); TODO
	dbg_end(DM_CALLS,"model::autocreate_patterns");
	return TRUE;
}

// Create representative molecules for patterns
void model::create_pattern_molecules()
{
	dbg_begin(DM_CALLS,"model::create_pattern_molecules");
	for (pattern *p = patterns.first(); p != NULL; p = p->next)
	{
		// Just select the first molecule in the pattern, and copy-paste to the model
		select_none();
		atom *i = p->get_firstatom();
		for (int n=0; n<p->get_natoms(); n++)
		{
			select_atom(i);
			i = i->next;
		}
		// Copy selection which now represents one molecule of this pattern
		master.privclip.copy_selection(this);
		// Clear the pattern's representative molecule
		p->molecule.clear();
		master.privclip.paste_to_model(&p->molecule);
		p->molecule.set_name(p->get_name());
		p->molecule.centre();
		p->molecule.select_none();
	}
	select_none();
	dbg_end(DM_CALLS,"model::create_pattern_molecules");
}

// Get empirical formula of selection
void model::selection_get_empirical(dnchar &target)
{
	dbg_begin(DM_CALLS,"model::selection_get_empirical");
	int elcount[NELEMENTS];
	//string result;
	target.clear();
	int n;
	// Reset element counters
	for (n=0; n<NELEMENTS; n++) elcount[n] = 0;
	atom *i = atoms.first();
	while (i != NULL)
	{
		if (i->is_selected()) elcount[i->get_element()] ++;
		i = i->next;
	}
	// Construct element string
	for (n=NELEMENTS-1; n>0; n--)
		if (elcount[n] != 0) 
		{
			target.cat(elements.symbol(n));
			if (elcount[n] > 1) target.cat(itoa(elcount[n]));
		}
	dbg_end(DM_CALLS,"model::selection_get_empirical");
}

// Get atom fingerprint of current selection
void model::selection_get_atom_fingerprint(dnchar &target)
{
	dbg_begin(DM_CALLS,"model::selection_get_atom_fingerprint");
	target.clear();
	atom *i = get_first_selected();
	if (i == NULL)
	{
		dbg_end(DM_CALLS,"model::selection_get_atom_fingerprint");
		return;
	}
	int lastel = i->get_element(), newel;
	int count = 1;
	for (i = i->next; i != NULL; i = i->get_next_selected())
	{
		// Check this element against the last. If the last element is the same, increase the counter. If different, append to the string
		newel = i->get_element();
		if (newel == lastel) count ++;
		else
		{
			target.cat(elements.symbol(i));
			target.cat(itoa(count));
			lastel = newel;
			count = 0;
		}
	}
	// Check for last element chunk
	if (count != 0)
	{
		target.cat(elements.symbol(lastel));
		target.cat(itoa(count));
	}
	dbg_end(DM_CALLS,"model::selection_get_atom_fingerprint");
}

// Get bond fingerprint of current selection
void model::selection_get_bond_fingerprint(dnchar &target)
{
	dbg_begin(DM_CALLS,"model::selection_get_bond_fingerprint");
	target.clear();
	int count = 0, diff;
	refitem<bond> *ri;
	atom *i = atoms.first();
	atom *j;
	while (i != NULL)
	{
		if (i->is_selected()) 
		{
			ri = i->get_bonds();
			while (ri != NULL)
			{
				j = ri->item->get_partner(i);
				diff = j->get_id() - i->get_id();
				if (diff > 0)
				{
					target.cat(itoa(count));
					target += '-';
					target.cat(itoa(count + diff));
					target += ' ';
				}
				ri = ri->next;
			}
			count ++;
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"model::selection_get_bond_fingerprint");
}

// Find pattern by name
pattern *model::find_pattern(const char *s)
{
	dbg_begin(DM_CALLS,"model::find_pattern");
	pattern *p = NULL;
	for (p = patterns.first(); p != NULL; p = p->next)
		if (strcmp(p->get_name(),s) == 0) break;
	if (p == NULL) msg(DM_NONE,"Pattern '%s' does not exist in model '%s'.\n",s,name.get());
	dbg_end(DM_CALLS,"model::find_pattern");
	return p;
}

// Charge pattern atom
void model::charge_pattern_atom(pattern *p, int id, double q)
{
	dbg_begin(DM_CALLS,"model::charge_pattern_atom");
	int n,m,pnatoms;
	atom *i = p->get_firstatom();
	pnatoms = p->get_natoms();
	// Skip on the 'i' pointer so it points to the atom 'id' in the first molecule of the pattern
	for (n=0; n<id; n++) i = i->next;
	i->set_charge(q);
	// Loop over molecules-1, setting charge and then skipping on pnatoms
	for (n=1; n<p->get_nmols(); n++)
	{
		for (m=0; m<pnatoms; m++) i = i->next;
		i->set_charge(q);
	}
	dbg_end(DM_CALLS,"model::charge_pattern_atom");
}

/* Recreate model from patterns
void model::recreate_from_patterns(model *srcmodel)
{
	dbg_begin(DM_CALLS,"model::recreate_from_patterns");
	// From the coordinates in the source model supplied and the reflist of patterns supplied, clear and recreate the model to contain a copy of the config. For each pattern node, paste in 'n' copies of the representative pattern model (to retain bonding) and then set coordinates from the source model.
	int n, m, cfgi;
	pattern *p;
	patatom *pa;
	atom *i;
	gui.pause_rendering();
	atoms.clear();
	msg(DM_NONE,"Reconstructing model from pattern / model combination:\n");
	msg(DM_NONE,"There are %i patterns in the model...\n",patterns.size());
	// Paste in molecules
	msg(DM_NONE,"Creating atom space...");
	for (p = patterns.first(); p != NULL; p = p->next)
	{
		master.privclip.copy_all(&p->molecule);
		for (n=0; n<p->get_nmols(); n++) master.privclip.paste_to_model(this);
	}
	msg(DM_NONE,"Done.\n");
	// Adjust atom properties
	msg(DM_NONE,"Setting atom properties...");
	i = atoms.first();
	vec3<double> *cfgr = cfg->get_r();
	for (p = patterns.first(); p != NULL; p = p->next)
	{
		cfgi = p->get_startatom();
		for (n=0; n<p->get_nmols(); n++)
			for (pa = p->atoms.first(); pa != NULL; pa = pa->next)
			{
				i->set_coords(cfgr[cfgi]);
				i->set_fftype(pa->get_data());
				cfgi ++;
				i = i->next;
			}
	}
	msg(DM_NONE,"Done.\n");
	// Set start / end atom pointers in patterns
	msg(DM_NONE,"Setting atom pointers in patterns...");
	i = atoms.first();
	for (p = patterns.first(); p != NULL; p = p->next)
	{
		// Set the starting atom of the pattern (and new totalatoms)
		p->set_firstatom(i);
		p->calc_totalatoms();
		// Skip through atoms in pattern to get the next starting atom
		for (n=0; n<p->get_nmols(); n++)
			for (m=0; m<p->get_natoms(); m++) i = i->next;
		// Set lastatom - if i == NULL (as will be the case for the last pattern) use the model pointer
		p->set_lastatom( i == NULL ? atoms.last() : i );
	}
	msg(DM_NONE,"Done.\n");
	gui.resume_rendering();
	dbg_end(DM_CALLS,"model::recreate_from_patterns");
} */

