/*
	*** Forcefield atom type
	*** src/classes/atomtype.cpp
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

#include "classes/atomtype.h"
#include "classes/atom.h"
#include "classes/bond.h"
#include "classes/ring.h"
#include "classes/forcefield.h"
#include "templates/reflist.h"
#include "base/elements.h"
#include "file/parse.h"

string leader = "";

// Atom environment
const char *AG_keywords[AG_NITEMS] = { "none", "free", "linear", "tshape", "trigonal", "tetrahedral", "sqplanar", "tbp", "octahedral" };
atom_geom AG_from_text(const char *s)
	{ return (atom_geom) enum_search("atom geometry",AG_NITEMS,AG_keywords,s); }
const char *text_from_AG(atom_geom i)
	{ return AG_keywords[i]; }

// Geometries about atomic centres
const char *AE_strings[AE_NITEMS+1] = { "Unspecified", "Unbound atom", "Aliphatic sp3", "Resonant sp2", "Triple-bond sp", "Aromatic sp2" };
const char *text_from_AE(atom_env i)
	{ return AE_strings[i+1]; }

// Constructors
atomtype::atomtype()
{
	// Must set all values to the 'don't care / unspecifed state'
	el = 0;
	env = AE_UNSPECIFIED;
	geom = AG_NONE;
	os = 99;
	nallowedel = 0;
	nbonds = -1;
	allowed_el = NULL;
	bound_bond = BT_UNSPECIFIED;
	nrepeat = 1;
	acyclic = FALSE;
	prev = NULL;
	next = NULL;
	#ifdef MEMDEBUG
	memdbg.create[MD_ATOMTYPE] ++;
	#endif
}

ringtype::ringtype()
{
	ringsize = -1;
	prev = NULL;
	next = NULL;
	#ifdef MEMDEBUG
	memdbg.create[MD_RINGTYPE] ++;
	#endif
}

// Destructors
atomtype::~atomtype()
{
	// Need to destroy all ring structures and bound atom list
	if (allowed_el != NULL) delete[] allowed_el;
	#ifdef MEMDEBUG
	memdbg.destroy[MD_ATOMTYPE] ++;
	#endif
}

ringtype::~ringtype()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_RINGTYPE] ++;
	#endif
}

/*
// AtomType routines
*/

// Set element list in atomtype
void atomtype::set_elements(const char *ellist, forcefield *ff)
{
	// Add elements from the comma-separated ellist string as possible matches for this atomtype
	dbg_begin(DM_CALLS,"atomtype::set_elements");
	int n, count;
	ffatom *ffa;
	dnchar temp;
	// Find number of elements given in list...
	parser.get_args_delim(ellist,PO_DEFAULTS);
	// Use 'nargs' to allocate element list
	nallowedel = parser.get_nargs();
	allowed_el = new int[nallowedel];
	count = 0;
	// Go through items in 'element' list...
	msg(DM_TYPING,"  %i atom types/elements given for atomtype : ",nallowedel);
	for (n=0; n<parser.get_nargs(); n++)
	{
		// If name begins with a '$' then we expect an atomtype id/name and not an element
		if (parser.argc(n)[0] == '$')
		{
			// Copy string and remove leading '$'
			temp = parser.argc(n);
			temp.erasestart(1);
			// Search for the atomtype pointer with ffid in 'temp' in the forcefield supplied
			if (ff != NULL)
			{
				// Were we given a number (ffid) or a string (type name)?
				ffa = (temp.is_numeric() ? ff->find_type(temp.as_integer()) : ff->find_type(temp.get()));
				if (ffa == NULL)
				{
					// TODO Does this need a warning? Will we be able to handle recursive typeid checks properly?
					msg(DM_NONE,"Forcefield type ID/name %s has not yet been defined in the forcefield.\n",temp.get());
				}
				else allowed_types.add(ffa);
			}
			else printf("atomtype::set_elements <<<< Type ID/Name found in list, but no forcefield passed >>>>\n");
			msg(DM_TYPING,"%s ",parser.argc(n));
		}
		else
		{
			el = elements.find(parser.argc(n),ZM_ALPHA);
			if (el == 0)
			{
				nallowedel --;
				msg(DM_NONE,"Warning : Unrecognised element in list of bound atoms: '%s'\n",parser.argc(n));
				msg(DM_TYPING,"?%s? ",parser.argc(n));
			}
			else
			{
				allowed_el[count] = el;
				count ++;
				msg(DM_TYPING,"%s ",parser.argc(n));
			}
		}
	}
	msg(DM_TYPING,"\n");
	dbg_end(DM_CALLS,"atomtype::set_elements");
}

// Print Atom Type data
void atomtype::print()
{
	leader += "--";
	printf("%s Element :",leader.c_str());
	if (nallowedel == 0) printf(" Any");
	else for (int n=0; n<nallowedel; n++) printf(" %s",elements.name(allowed_el[n]));
	printf("\n");
	printf("%s  Repeat : %i\n",leader.c_str(),nrepeat);
	if (boundlist.size() != 0)
	{
		printf("%s   Atoms : \n",leader.c_str());
		for (atomtype *xat = boundlist.first(); xat != NULL; xat = xat->next) xat->print();
	}
	if (ringlist.size() != 0)
	{
		printf("%s   Rings : \n",leader.c_str());
		for (ringtype *xring = ringlist.first(); xring != NULL; xring = xring->next) xring->print();
	}
	leader.erase(leader.length()-2,leader.length());
}

/*
// AtomType Ring Functions
*/

// Print
void ringtype::print()
{
	leader += "--";
	printf("%s   Size : %i\n",leader.c_str(),ringsize);
	if (ringatoms.size() != 0)
	{
		printf("%s Contains :\n",leader.c_str());
		for (atomtype *xat = ringatoms.first(); xat != NULL; xat = xat->next) xat->print();
	}
	leader.erase(leader.length()-2,2);
}

/*
// Expand Functions
*/

void ringtype::expand(const char *data, forcefield *ff)
{
	// Separate function (to prevent brain melting) to recursively create a ring definition.
	// At least allows the restriction (and addition) of commands to the ring command.
	dnchar keywd, optlist, def;
	dbg_begin(DM_CALLS,"ringtype::expand");
	msg(DM_TYPING,"expand[ring] : Received string [%s]\n",data);
	// Grab the next command, trip the keyword and option list (if there is one).
	def.set(data);
	do
	{
		// Get next command and repeat
		msg(DM_TYPING,"Command String : [%s]\n",def.get());
		optlist = parser.parse_atstring(def);
		keywd = parser.trim_atkeyword(optlist);
		msg(DM_TYPING,"       Keyword : [%s]\n",keywd.get());
		msg(DM_TYPING,"       Options : [%s]\n",optlist.get());
		if (keywd == "size")	// Ring size specifier
			ringsize = atoi(optlist.get());
		else if (keywd[0] == '-')	// An atom in the ring
		{
			keywd.erase(0,1);	// Remove keyword '-'
			atomtype *newat = ringatoms.add();
			newat->set_elements(keywd.get(),ff);
			newat->expand(optlist.get(),ff);
		}
		else
			msg(DM_NONE,"ringtype::expand - Unrecognised command (%s) in atomtype description.\n",keywd.get());

	} while (!def.empty());
	dbg_end(DM_CALLS,"ringtype::expand");
}

// Master creation routine, returning the head node of an atomtype structure.
void atomtype::expand(const char *data, forcefield *ff)
{
	// Expands the structure with the commands contained in the supplied string.
	// Format is : X(options,...) where X is the element symbol and 'options' is zero or more of:
	//	-Z(options)	: Atom X is bound to atom Z. Z has a description of 'options'
	//	unbound,sp,sp2,
	//	sp3,aromatic	: Atom X has the specified hybridisation / is aromatic / is unbound.
	//	ring()		: Atom X is involved in a cycle of some kind (see above)
	// Options are comma-separated. Defaults are 'don't care' where applicable.
	// The supplied string should contain a keyword followed by (optional) bracketed list of specs.
	// Parent ring structure must be supplied when descending into a ring options structure.
	dbg_begin(DM_CALLS,"atomtype::expand");
	dnchar keywd, optlist, def;
	int n;
	msg(DM_TYPING,"atomtype::expand - Received string [%s]\n",data);
	if (data[0] == '\0')
	{
		dbg_end(DM_CALLS,"atomtype::expand");
		return;
	}
	// Grab the next command, strip the keyword and option list (if there is one).
	def.set(data);
	do
	{
		msg(DM_TYPING,"Command String : [%s]\n",def.get());
		optlist = parser.parse_atstring(def);
		keywd = parser.trim_atkeyword(optlist);
		msg(DM_TYPING,"       Keyword : [%s]\n",keywd.get());
		msg(DM_TYPING,"       Options : [%s]\n",optlist.get());
		if (keywd[0] == '-')	// 'Bound to' specifier
		{
			// Bound atom spec - create new subnode and add it to the bound list.
			// Format is either '-X(options...)' or '-[X1,X2,X3](options...)' (single or list of elements respectively)
			// Remove leading '-', add bound atom and set its element list
			keywd.erasestart(1);
			atomtype *newat = boundlist.add();
			newat->set_elements(keywd.get(),ff);
			newat->expand(optlist.get(),ff);
		}
		else if (keywd == "sp")		// Hybridisation / environment settings (no options)
			env = AE_SP;
		else if (keywd == "sp2")
			env = AE_SP2;
		else if (keywd == "sp3")
			env = AE_SP3;
		else if (keywd == "aromatic")
			env = AE_AROMATIC;
		else if (keywd == "unbound")
			env = AE_UNBOUND;
		else if (keywd == "ring")	// Ring description (pass to ring->expand)
		{
			ringtype *newring = ringlist.add();
			newring->expand(optlist.get(),ff);
		}
		else if (keywd == "nbonds")	// Request exact bond number (nbonds=int)
			nbonds = atoi(optlist.get());
		else if (keywd == "bond")	// Request exact bond type (bond=bond_type)
			bound_bond = BT_from_text(optlist.get());
		else if (keywd == "n")		// Number of times to match (n=int)
			nrepeat = atoi(optlist.get());
		else if (keywd == "os")		// Oxidation state of element (os=int)
			os = atoi(optlist.get());
		else if (keywd == "geom")	// Local geometry about atom (geom=type)
			geom = AG_from_text(optlist.get());
		else
			msg(DM_NONE,"atomtype::expand : Unrecognised command (%s) in atomtype description.\n", keywd.get());
	} while (!def.empty());
	dbg_end(DM_CALLS,"atomtype::expand");
}

/*
// Match Functions
*/

int atomtype::match_in_list(reflist<atom> *alist, list<ring> *ringdata, model *parent)
{
	dbg_begin(DM_CALLS,"atomtype::match_in_list");
	// Search the atomlist supplied for a match to this atomtype.
	// If we find one, remove the corresponding atom from the atomlist.
	int score = 0, bondscore;
	refitem<atom> *boundi = alist->first();
	while (boundi != NULL)
	{
		// See if the atom has already been used elsewhere in the current round of typing
		//if (boundi->item->tempi != 0) {boundi = boundi->next; continue;}
		// Extra check for bond type definition here
		if (bound_bond == BT_UNSPECIFIED)
		{
			msg(DM_TYPING,"match_in_list : ...Atom passed bond type check [no value specified in type]\n");
			bondscore = 1;
		}
		else bound_bond == boundi->data2 ? bondscore = 1 : bondscore = 0;
		// Now do proper atom type check (if we passed the bond check)
		if (bondscore != 0) score = match_atom(boundi->item, ringdata, parent);
		if ((bondscore + score) > 1) break;
		boundi = boundi->next;
	}
	// If boundi is NULL then we finished the loop without finding a match to this atomtype.
	if (boundi != NULL)
	{
		//boundi->item->tempi = 1;
		alist->remove(boundi);
		dbg_end(DM_CALLS,"atomtype::match_in_list");
		return bondscore+score;
	}
	else
	{
		dbg_end(DM_CALLS,"atomtype::match_in_list");
		return 0;
	}
}

int atomtype::match_atom(atom* i, list<ring> *ringdata, model *parent)
{
	// Given the supplied atom pointer and ring data pointer (passed from pattern)
	// see how well the description matches the actual atom, returning as an int. Cycle data is 
	// available in (pattern->)rings. Exit and return 0 as soon as a test fails.
	dbg_begin(DM_CALLS,"atomtype::match_atom");
	int typescore, atomscore, ringscore, n;
	bool found;
	reflist<atom> atomchecklist;
	reflist<ring> ringchecklist;
	refitem<ffatom> *rd;
	// Set the scoring to one (which will be the case if there are no specifications to match)
	typescore = 1;
	// Element check
	if (nallowedel == 0) msg(DM_TYPING,"......Atom passed element check [none specified]\n");
	else
	{
		// Check through list of elements/types that this atom is allowed to be
		found = FALSE;
		for (n=0; n<nallowedel; n++)
		{
			// If its an element, just check the element of the atom.
			if (i->is_element(allowed_el[n]))
			{
				found = TRUE;
				break;
			}
		}
		if (!found) for (rd = allowed_types.first(); rd != NULL; rd = rd->next)
		{
		printf("CHECKING FOR EXACT TYPE (ffid=%i, name=%s)\n",rd->item->get_ffid(),rd->item->get_name());
			// Does this atom match the type descriptions asked for?
			n = rd->item->get_atomtype()->match_atom(i,ringdata,parent);
			if (n > 0)
			{
				found = TRUE;
				break;
			}
		}
		if (found)
		{
			typescore++;
			msg(DM_TYPING,"......Atom passed element check [match]\n");
		}
		else
		{
			msg(DM_TYPING,"......Atom FAILED element check [is %i, type needs %i]\n",i->get_element(),el);
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Atom environment check
	if (env == AE_UNSPECIFIED) msg(DM_TYPING,"......Atom passed atom environment check [none specified]\n");
	else
	{
		if (i->is_env(env))
		{
			typescore++;
			msg(DM_TYPING,"......Atom passed atom environment check [matched %s]\n",text_from_AE(i->get_env()));
		}
		else
		{
			msg(DM_TYPING,"......Atom FAILED atom environment check [is %s, type needs %s]\n",
				text_from_AE(i->get_env()),text_from_AE(env));
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Oxidation state check
	if (os == 99) msg(DM_TYPING,"......Atom passed oxidation state check [none specified]\n");
	else
	{
		if (i->is_os(os))
		{
			typescore++;
			msg(DM_TYPING,"......Atom passed oxidation state check [matched %i]\n",i->get_os());
		}
		else
		{
			msg(DM_TYPING,"......Atom FAILED oxidation state check [is %s, type needs %s]\n",
				i->get_os(),os);
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Number of bound atoms check
	if (nbonds == -1) msg(DM_TYPING,"......Atom passed bond number check [none specified]\n");
	else
	{
		if (i->is_nbonds(nbonds))
		{
			typescore++;
			msg(DM_TYPING,"......Atom passed bond number check [matched %i]\n",i->get_nbonds());
		}
		else
		{
			msg(DM_TYPING,"......Atom FAILED bond number check [is %i, type needs %i]\n",i->get_nbonds(),nbonds);
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Local atom geometry check
	if (geom == AG_NONE) msg(DM_TYPING,"......Atom passed bond number check [none specified]\n");
	else
	{
		if (i->get_geometry(parent) == geom)
		{
			typescore++;
			msg(DM_TYPING,"......Atom passed geometry check [matched %s]\n",text_from_AG(geom));
		}
		else
		{
			msg(DM_TYPING,"......Atom FAILED geometry check [is %s, type needs %s]\n",text_from_AG(i->get_geometry(parent)),
				text_from_AG(geom));
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// List of bound atoms check
	msg(DM_TYPING,"......Begin bound atom check...\n");
	atomtype *bat = boundlist.first();
	if (bat != NULL)
	{
		// Fill the atomchecklist with the atoms bound to our current atom.
		i->add_bound_to_reflist(&atomchecklist);
		//i->tempi = 1;
		while (bat != NULL)
		{
			for (n=0; n<bat->nrepeat; n++)
			{
				// Check the atomlist for a match to the bound atomtype
				atomscore = bat->match_in_list(&atomchecklist,ringdata,parent);
				if (atomscore != 0)
				{
					msg(DM_TYPING,".........Atom passed bound atom check [matched element]\n");
					typescore += atomscore;
				}
				else
				{
					msg(DM_TYPING,".........Atom FAILED bound atom check [element not in list]\n");
					dbg_end(DM_CALLS,"atomtype::match_atom");
					return 0;
				}
			}
			bat = bat->next;
		}
	}
	else
	{
		//typescore ++;
		msg(DM_TYPING,"......Atom passed bound atoms check [no bound atoms defined in type]\n");
	}
	// Ring check
	ringtype *atr = ringlist.first();
	if (atr == NULL)
	{
		msg(DM_TYPING,"......Atom passed basic ring check [no rings defined in atom type]\n");
		//typescore ++;
	}
	else
	{
		// Get list of rings out test atom is involved in
		ringchecklist.clear();
		ring *r = ringdata->first();
		while (r != NULL)
		{
			// Search the list of atoms in this ring for 'i'
			if (r->atoms.search(i) != NULL) ringchecklist.add(r,0,0);
			r = r->next;
		}
		// Loop over ring specifications in atom type
		while (atr != NULL)
		{
			// Loop over rings our atom is involved in, searching for a match.
			refitem<ring> *inring = ringchecklist.first();
			while (inring != NULL)
			{
				// Size check
				if (atr->ringsize == -1)
				{
					msg(DM_TYPING,".........Atom passed ring size check [no size defined]\n");
					ringscore = 1;
				}
				else if (atr->ringsize == inring->item->atoms.size())
				{
					msg(DM_TYPING,".........Atom passed ring size check [matched %i]\n",atr->ringsize);
					ringscore = 1;
				}
				else
				{
					msg(DM_TYPING,".........Atom FAILED ring size check [sizes do not match]\n");
					ringscore = 0;
				}
				// Get the list of other atoms in this ring ready for searching.
				if (ringscore != 0)
				{
					atomchecklist.clear();
					inring->item->add_atoms_to_reflist(&atomchecklist,i);
					// Now go through list of specified atomtypes in this ringtype
					bat = atr->ringatoms.first();
					while (bat != NULL)
					{
						atomscore = bat->match_in_list(&atomchecklist,ringdata,parent);
						if (atomscore != 0)
						{
							msg(DM_TYPING,"............Atom passed ring atom check [matched element]\n");
							ringscore += atomscore;
						}
						else
						{
							msg(DM_TYPING,"............Atom FAILED ring atom check [element]\n");
							// Don't fully return just yet - move onto next ring...
							ringscore = 0;
							break;
						}
						bat = bat->next;
					}
					if (ringscore != 0) break;
				}
				inring = inring->next;
			}
			// Check 'inring' - if NULL then we did not manage to find a match for the ring
			if (inring != NULL)
			{
				msg(DM_TYPING,"......Atom passed full ring check [match]\n");
				typescore += ringscore;
			}
			else
			{
				msg(DM_TYPING,"......Atom FAILED full ring check [no match]\n");
				dbg_end(DM_CALLS,"atomtype::match_atom");
				return 0;
			}
			atr = atr->next;
		}
	}
	// All checks completed, so return the final typing score
	dbg_end(DM_CALLS,"atomtype::match_atom");
	return typescore;
}

