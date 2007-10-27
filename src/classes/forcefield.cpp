/*
	*** Molecular mechanics forcefield
	*** src/classes/forcefield.cpp
	Copyright T. Youngs 2007

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

#include "classes/forcefield.h"
#include "base/prefs.h"
#include "base/debug.h"

// Constructors 
ffparams::ffparams()
{
	for (int i=0; i<MAXFFPARAMDATA; i++) data[i] = 0.0;
	#ifdef MEMDEBUG
		memdbg.create[MD_FF_PARAMS] ++;
	#endif
}

ffatom::ffatom()
{
	prev = NULL;
	next = NULL;
	name.set("Unnamed");
	ffid = -1;
	q = 0.0;
	vdwstyle = VF_UNSPECIFIED;
	generator = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_FF_ATOM] ++;
	#endif
}

ffbound::ffbound()
{
	prev = NULL;
	next = NULL;
	type = FFC_NITEMS;
	#ifdef MEMDEBUG
		memdbg.create[MD_FF_BOUND] ++;
	#endif
}

forcefield::forcefield()
{
	// Clear the variables in the structure
	next = NULL;
	prev = NULL;
	rules = FFR_NORULES;
	ngendata = 0;
	convertgen = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_FORCEFIELD] ++;
	#endif
}

// Destructors
ffparams::~ffparams()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_FF_PARAMS] ++;
	#endif
}

ffatom::~ffatom()
{
	if (generator != NULL) delete[] generator;
	#ifdef MEMDEBUG
		memdbg.destroy[MD_FF_ATOM] ++;
	#endif
}

ffbound::~ffbound()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_FF_BOUND] ++;
	#endif
}

forcefield::~forcefield()
{
	// Delete all parameter lists
	if (convertgen != NULL) delete[] convertgen;
	#ifdef MEMDEBUG
		memdbg.destroy[MD_FORCEFIELD] ++;
	#endif
}

// Search FF for type ID
ffatom *forcefield::find_type(int query)
{
	// Search for the ffid specified and return the internal integer id (i.e. position in atomtype list)
	dbg_begin(DM_CALLS,"forcefield::find_type[int]");
	ffatom *result;
	for (result = atomtypes.first(); result != NULL; result = result->next)
		if (query == result->ffid) break;
	dbg_end(DM_CALLS,"forcefield::find_type[int]");
	return result;
}

// Search FF for type name
ffatom *forcefield::find_type(const char *query)
{
	// Search for the atomname specified and return the internal integer id (i.e. position in atomtype list)
	// We return the first occurrence we find (since there may be more than one - only ffid need be unique)
	// Search both names and equivalents (since aliases may be defined that are not themselves defined as atomtypes)
	dbg_begin(DM_CALLS,"forcefield::find_type[char]");
	ffatom *result;
	for (result = atomtypes.first(); result != NULL; result = result->next)
		if ((result->name == query) || (result->equiv == query)) break;
	dbg_end(DM_CALLS,"forcefield::find_type[char]");
	return result;
}

// Return description of ffid
atomtype *forcefield::get_atomtype_of_ffid(int i)
{
	dbg_begin(DM_CALLS,"forcefield::get_atomtype_of_ffid");
	ffatom *result = NULL;
	for (result = atomtypes.first(); result != NULL; result = result->next)
		if (result->ffid == i) break;
	if (result == NULL) printf("forcefield::get_atomtype_of_ffid <<<< FFID %i not found in forcefield >>>>\n",i);
	dbg_end(DM_CALLS,"forcefield::get_atomtype_of_ffid");
	return &result->typedesc;
}

// Match two forcefield type strings
int forcefield::match_type(const char *test, const char *target)
{
	// Text-match the single atomtype 'i' to the passed string.
	// Wildcard '*' in the atomname matches rest of string.
	// Return 1 for a wildcard, '0' for an exact match, and an arbitrary '10' for no match
	if (strcmp(test,target) == 0) return 0;
	bool wild = FALSE, failed = FALSE;
	int length = max(strlen(test),strlen(target));
	for (int n=0; n <length; n++)
	{
		if ((target[n] == '*') || (test[n] == '*'))
		{
			wild = TRUE;
			break;
		}
		else if (test[n] != target[n])
		{
			failed = TRUE;
			break;
		}
	}
	if (failed) return 10;
	if (wild) return 1;
	printf("forcefield::match_type <<<< Weird error - missed exact match? >>>>\n");
	return 10;
}

// Match atom types to names
int forcefield::match_types(ffatom *ffi, ffatom *ffj, const dnchar &ti, const dnchar &tj)
{
	// Type Match routines - string match the name of atomtypes 'i' and 'j' to the string'd atomtypes
	// specified in the bond / angle / torsion data supplied. Only check 'one way round' - the routine
	// must be called again with i and j swapped over to test the inverse case.
	// Matches against 'equiv' atomnames.
	dbg_begin(DM_CALLS,"forcefield::match_types");
	int matchi, matchj;
	// Best case - exact, direct match:
	if ((ffi->equiv == ti) && (ffj->equiv == tj))
	{
		dbg_end(DM_CALLS,"forcefield::match_types");
		return 0;
	}
	// No such luck, so match each atom separately
	matchi = match_type(ffi->equiv,ti);
	matchj = match_type(ffj->equiv,tj);
	dbg_end(DM_CALLS,"forcefield::match_types");
	return (matchi + matchj);
}

// Scoring method for the following parameter-search routines works as follows:
//	0  : Exact match to all parameters
//	1-9: Partial match with wildcards
//	10+: One or more parameters did not match

// Find bond type
ffbound *forcefield::find_bond(ffatom *ffi, ffatom *ffj)
{
	// Search the forcefield for the bond definition for the interaction of the atom types i-j
	// Return NULL if no match found ('result' remains 'NULL' if no kind of match is found).
	dbg_begin(DM_CALLS,"forcefield::find_bond");
	ffbound *result = NULL;
	int matchij, matchji, bestmatch;
	bestmatch = 10;
	ffbound *b = bonds.first();
	while (b != NULL)
	{
		// See how close the match is between the atom forcefield types and the bond specification
		matchij = match_types(ffi,ffj,b->types[0],b->types[1]);
		matchji = match_types(ffj,ffi,b->types[0],b->types[1]);
		if (matchji < matchij) matchij = matchji;	// Take the better (smaller) of the two results
		if (matchij < 10)
		{
			if (matchij < bestmatch)	// Better match
			{
				result = b;
				bestmatch = matchij;
			}
		}
		if (bestmatch == 0) break;
		b = b ->next;
	}
	dbg_end(DM_CALLS,"forcefield::find_bond");
	return result;
}

// Find angle type
ffbound *forcefield::find_angle(ffatom *ffi, ffatom *ffj, ffatom *ffk)
{
	// Search the forcefield for the angle definition for the interaction of the atom types i-j-k
	// Return NULL is no match found.
	dbg_begin(DM_CALLS,"forcefield::find_angle");
	ffbound *result = NULL;
	int matchj, matchik, matchki, bestmatch;
	bestmatch = 10;
	ffbound *a = angles.first();
	while (a != NULL)
	{
		// See how close the match is between the atom forcefield types and the angle specification
		// Check the central atom of the angle first
		matchj = match_type(ffj->equiv,a->types[1]);
		if (matchj != 10)
		{
			matchik = match_types(ffi,ffk,a->types[0],a->types[2]);
			matchki = match_types(ffk,ffi,a->types[0],a->types[2]);
			if (matchki < matchik) matchik = matchki;	// Take the better of the two results
			matchik += matchj;		// Add on the score from the central atom
			if (matchik < 10)
			{
				if (matchik < bestmatch)
				{
					result = a;
					bestmatch = matchik;
				}
			}
		}
		if (bestmatch == 0) break;		// Early exit for an exact match
		a = a ->next;
	}
	dbg_end(DM_CALLS,"forcefield::find_angle");
	return result;
}

// Find torsion type
ffbound *forcefield::find_torsion(ffatom *ffi, ffatom *ffj, ffatom *ffk, ffatom *ffl)
{
	// Search the forcefield for the torsion definition for the interaction of the atom types i-j-k-l
	// Return NULL is no match found.
	dbg_begin(DM_CALLS,"forcefield::find_torsion");
	ffbound *result = NULL;
	int matchil, matchli, matchjk, matchkj, matchijkl, matchlkji, bestmatch;
	bestmatch = 10;
	ffbound *t = torsions.first();
	while (t != NULL)
	{
		// See how close the match is between the atom forcefield types and the torsion specification
		matchil = match_types(ffi,ffl,t->types[0],t->types[3]);
		matchli = match_types(ffl,ffi,t->types[0],t->types[3]);
		matchjk = match_types(ffj,ffk,t->types[1],t->types[2]);
		matchkj = match_types(ffk,ffj,t->types[1],t->types[2]);
		matchijkl = matchil + matchjk;
		matchlkji = matchli + matchkj;
		if (matchlkji < matchijkl) matchijkl = matchlkji;
		if (matchijkl < 10)
		{
			if (matchijkl < bestmatch)
			{
				result = t;
				bestmatch = matchijkl;
			}
		}
		if (bestmatch == 0) break;
		t = t->next;
	}
	dbg_end(DM_CALLS,"forcefield::find_torsion");
	return result;
}

void forcefield::convert_parameters(energy_unit ff_eunit)
{
	// Convert units of all the energetic parameters within the forcefield from the unit supplied into program internal units (specified in prefs)
	// Check for 'NULL' pointers for ff_param variables (for e.g. rule-based forcefields)
	dbg_begin(DM_CALLS,"forcefield::convert_parameters");
	ffparams *p;
	ffbound *b;
	ffatom *ffa;
	int n;
	// VDW and Generator Data
	// Note: First loop (for VDW) is from 1,n+1 instead of 0,n since we skip the dummy atom type which is n=0, present for all ffs.
	for (ffa = atomtypes.first(); ffa != NULL; ffa = ffa->next)
	{
		p = &ffa->params;
		switch (ffa->get_funcform())
		{
			case (VF_UNSPECIFIED):
				break;
			case (VF_LJ):
				p->data[VF_LJ_EPS] = prefs.convert_energy(p->data[VF_LJ_EPS],ff_eunit);
				break;
			default:
				printf("Don't know how to convert forcefield parameters for this VDW type.\n");
		}
		// Only convert those parameters for which the 'convertgen[]' flag is TRUE
		if (ffa->generator != NULL)
		{
			for (n=0; n<ngendata; n++)
				if (convertgen[n]) ffa->generator[n] = prefs.convert_energy(ffa->generator[n],ff_eunit);
		}
	}
	// Bonds 
	for (b = bonds.first(); b != NULL; b = b->next)
	{
		p = &b->params;
		switch (b->funcform.bondfunc)
		{
			case (BF_UNSPECIFIED):
				break;
			case (BF_HARMONIC):
				p->data[BF_HARMONIC_K] = prefs.convert_energy(p->data[BF_HARMONIC_K],ff_eunit);
				break;
			default:
				printf("Don't know how to convert forcefield parameters for this bond type.\n");
		}
	}
	// Angles
	for (b = angles.first(); b != NULL; b = b->next)
	{
		p = &b->params;
		switch (b->funcform.anglefunc)
		{
			case (AF_UNSPECIFIED):
				break;
			case (AF_HARMONIC):
				p->data[AF_HARMONIC_K] = prefs.convert_energy(p->data[AF_HARMONIC_K],ff_eunit);
				break;
			default:
				printf("Don't know how to convert forcefield parameters for this angle type.\n");
		}
	}
	// Torsions
	for (b = torsions.first(); b != NULL; b = b->next)
	{
		p = &b->params;
		switch (b->funcform.torsionfunc)
		{
			case (TF_UNSPECIFIED):
				break;
			case (TF_COSINE):
				p->data[TF_COSINE_K] = prefs.convert_energy(p->data[TF_COSINE_K],ff_eunit);
				break;
			case (TF_COS3):
				p->data[TF_COS3_K1] = prefs.convert_energy(p->data[TF_COS3_K1],ff_eunit);
				p->data[TF_COS3_K2] = prefs.convert_energy(p->data[TF_COS3_K2],ff_eunit);
				p->data[TF_COS3_K3] = prefs.convert_energy(p->data[TF_COS3_K3],ff_eunit);
				break;
			default:
				printf("Don't know how to convert forcefield parameters for this torsion type.\n");
		}
	}
	dbg_end(DM_CALLS,"forcefield::convert_parameters");
}
