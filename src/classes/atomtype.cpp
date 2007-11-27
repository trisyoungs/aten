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

int printlevel = 0;

// Atom typing commands
const char *ATC_keywords[ATC_NITEMS] = { "sp", "sp2", "sp3", "aromatic", "ring", "noring", "nbonds", "bond", "n", "os", "nh" };
atomtype_command ATC_from_text(const char *s)
	{ return (atomtype_command) enum_search("#",ATC_NITEMS,ATC_keywords,s); }

// Ring typing commands
const char *RTC_keywords[RTC_NITEMS] = { "size", "n", "notself" };
ringtype_command RTC_from_text(const char *s)
	{ return (ringtype_command) enum_search("#",RTC_NITEMS,RTC_keywords,s); }

// Atom environment
const char *AE_strings[AE_NITEMS] = { "Unspecified", "Unbound atom", "Aliphatic sp3", "Resonant sp2", "Triple-bond sp", "Aromatic sp2" };
const char *text_from_AE(atom_env i)
	{ return AE_strings[i]; }

// Geometries about atomic centres
const char *AG_keywords[AG_NITEMS] = { "unspecified", "unbound", "onebond", "linear", "tshape", "trigonal", "tetrahedral", "sqplanar", "tbp", "octahedral" };
atom_geom AG_from_text(const char *s)
	{ return (atom_geom) enum_search("atom geometry",AG_NITEMS,AG_keywords,s); }
const char *text_from_AG(atom_geom i)
	{ return AG_keywords[i]; }

// Constructors
atomtype::atomtype()
{
	// Must set all values to the 'don't care / unspecifed state'
	el = 0;
	env = AE_UNSPECIFIED;
	geom = AG_UNSPECIFIED;
	os = 99;
	nallowedel = 0;
	nbonds = -1;
	allowedel = NULL;
	boundbond = BT_UNSPECIFIED;
	nrepeat = 1;
	acyclic = FALSE;
	nhydrogen = -1;
	prev = NULL;
	next = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_ATOMTYPE] ++;
	#endif
}

ringtype::ringtype()
{
	ringsize = -1;
	nrepeat = 1;
	selfabsent = FALSE;
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
	if (allowedel != NULL) delete[] allowedel;
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
	allowedel = new int[nallowedel];
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
				else allowedtypes.add(ffa);
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
				allowedel[count] = el;
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
	printlevel ++;
	printf("(%3i) Element :",printlevel);
	if (nallowedel == 0) printf(" Any");
	else for (int n=0; n<nallowedel; n++) printf(" %s",elements.name(allowedel[n]));
	printf("\n");
	printf("(%3i)  Repeat : %i\n",printlevel,nrepeat);
	if (boundlist.size() != 0)
	{
		printf("(%3i)   Atoms : \n",printlevel);
		for (atomtype *xat = boundlist.first(); xat != NULL; xat = xat->next) xat->print();
	}
	if (ringlist.size() != 0)
	{
		printf("(%3i)   Rings : \n",printlevel);
		for (ringtype *xring = ringlist.first(); xring != NULL; xring = xring->next) xring->print();
	}
	printlevel --;
}

/*
// AtomType Ring Functions
*/

// Print
void ringtype::print()
{
	printlevel ++;
	printf("(%3i)   Size : %i\n",printlevel,ringsize);
	if (ringatoms.size() != 0)
	{
		printf("(%3i) Contains :\n",printlevel);
		for (atomtype *xat = ringatoms.first(); xat != NULL; xat = xat->next) xat->print();
	}
	printlevel --;
}

/*
// Expand Functions
*/

void ringtype::expand(const char *data, forcefield *ff, ffatom *parent)
{
	// Separate function (to prevent brain melting) to recursively create a ring definition.
	// At least allows the restriction (and addition) of commands to the ring command.
	dbg_begin(DM_CALLS,"ringtype::expand");
	dnchar keywd, optlist, def;
	static char c;
	static int level = 0;
	static bool found;
	static ringtype_command rtc;
	static atomtype *newat;
	level ++;
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
		found = FALSE;
		// Check for atomtype specifier ('-' or '='). Both mean the same here...
		c = keywd[0];
		if ((c == '-') || (c == '='))
		{
			// Remove leading character, add bound atom and set its element list
			keywd.erasestart(1);
			newat = ringatoms.add();
			newat->set_elements(keywd.get(),ff);
			newat->expand(optlist.get(),ff,parent);
			//if (c == '=') boundbond = BT_DOUBLE;
			found = TRUE;
		}
		// Check for keywords (if it wasn't a bound specifier)
		if (!found)
		{
			rtc = RTC_from_text(keywd.get());
			// Set 'found' to TRUE - we will set it to FALSE again if we don't recognise the command
			found = TRUE;
			switch (rtc)
			{
				// Size specifier
				case (RTC_SIZE):
					ringsize = atoi(optlist.get());
					break;
				// Repeat specifier
				case (RTC_REPEAT):
					nrepeat = atoi(optlist.get());
					break;
				// Presence of self specifier
				case (RTC_NOTSELF):
					selfabsent = TRUE;
					break;
				// Unrecognised
				default:
					if (parent != NULL) msg(DM_NONE,"Unrecognised command '%s' found while expanding ring at depth %i [ffid/name %i/%s].\n", keywd.get(), level, parent->get_ffid(), parent->get_name());
					else msg(DM_NONE,"ringtype::expand - Unrecognised command (%s).\n", keywd.get());
					break;
			}
		}
	} while (!def.empty());
	level --;
	dbg_end(DM_CALLS,"ringtype::expand");
}

// Master creation routine, returning the head node of an atomtype structure.
void atomtype::expand(const char *data, forcefield *ff, ffatom *parent)
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
	// Parent pointer is used for error reporting
	dbg_begin(DM_CALLS,"atomtype::expand");
	dnchar keywd, optlist, def;
	static ringtype *newring;
	static bool found;
	static char c;
	static int n, level = 0;
	static atom_geom ag;
	static atomtype_command atc;
	level ++;
	msg(DM_TYPING,"atomtype::expand - Received string [%s]\n",data);
	if (data[0] == '\0')
	{
		level --;
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
		// Check for 'bound to' specifiers first ('-' or '=')
		// Bound atom spec - create new subnode and add it to the bound list.
		// Format is either '-X(options...)' or '-[X1,X2,X3](options...)' (single or list of elements respectively)
		found = FALSE;
		c = keywd[0];
		if ((c == '-') || (c == '='))
		{
			// Remove leading character, add bound atom and set its element list
			keywd.erasestart(1);
			atomtype *newat = boundlist.add();
			newat->set_elements(keywd.get(),ff);
			if (c == '=') boundbond = BT_DOUBLE;
			newat->expand(optlist.get(),ff,parent);
			found = TRUE;
		}
		// Check for keywords (if it wasn't a bound specifier)
		if (!found)
		{
			atc = ATC_from_text(keywd.get());
			// Set 'found' to TRUE - we will set it to FALSE again if we don't recognise the command
			found = TRUE;
			switch (atc)
			{
				// Hybridisation / environment settings (no options)
				case (ATC_SP):
					env = AE_SP;
					break;
				case (ATC_SP2):
					env = AE_SP2;
					break;
				case (ATC_SP3):
					env = AE_SP3;
					break;
				case (ATC_AROMATIC):
					env = AE_AROMATIC;
					break;
				// Ring specification (possible options)
				case (ATC_RING):
					newring = ringlist.add();
					newring->expand(optlist.get(),ff,parent);
					break;
				// Disallow rings
				case (ATC_NORING):
					acyclic = TRUE;
					break;
				// Request exact bond number
				case (ATC_NBONDS):
					nbonds = atoi(optlist.get());
					break;
				// Request exact bond type (bond=bond_type)
				case (ATC_BOND):
					boundbond = BT_from_text(optlist.get());
					break;
				// Number of times to match (n=int)
				case (ATC_REPEAT):
					nrepeat = atoi(optlist.get());
					break;
				// Oxidation state of element (os=int)
				case (ATC_OS):
					os = atoi(optlist.get());
					break;
				// Request no attached hydrogens
				case (ATC_NHYDROGENS):
					nhydrogen = atoi(optlist.get());
					break;
				default:
					found = FALSE;
					break;
			}
		}
		// Check for geometry specifications (if it wasn't a bound specifier or type command)
		if (!found)
		{
			ag = AG_from_text(keywd.get());
			if (ag != AG_NITEMS) geom = ag;
			else
			{
				if (parent != NULL) msg(DM_NONE,"Unrecognised command '%s' found while expanding atom at depth %i [ffid/name %i/%s].\n", keywd.get(), level, parent->get_ffid(), parent->get_name());
				else msg(DM_NONE,"Unrecognised command '%s' found while expanding atom.\n", keywd.get());
				break;
			}
		}
	} while (!def.empty());
	level --;
	dbg_end(DM_CALLS,"atomtype::expand");
}

/*
// Match Functions
*/

int atomtype::match_in_list(reflist<atom> *alist, list<ring> *ringdata, model *parent, atom *topatom)
{
	dbg_begin(DM_CALLS,"atomtype::match_in_list");
	// Search the atomlist supplied for a match to this atomtype.
	// If we find one, remove the corresponding atom from the atomlist.
	int score = 0, bondscore;
	refitem<atom> *boundi;
	for (boundi = alist->first(); boundi != NULL; boundi = boundi->next)
	{
		// Extra check for bond type definition here
		if (boundbond == BT_UNSPECIFIED) bondscore = 1;
		else boundbond == boundi->data2 ? bondscore = 1 : bondscore = 0;
		// Now do proper atom type check (if we passed the bond check)
		if (bondscore != 0) score = match_atom(boundi->item, ringdata, parent, topatom);
		if ((bondscore + score) > 1) break;
	}
	// If boundi is NULL then we finished the loop without finding a match to this atomtype.
	if (boundi != NULL)
	{
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

int atomtype::match_atom(atom* i, list<ring> *ringdata, model *parent, atom *topatom)
{
	// Given the supplied atom pointer and ring data pointer (passed from pattern)
	// see how well the description matches the actual atom, returning as an int. Cycle data is 
	// available in (pattern->)rings. Exit and return 0 as soon as a test fails.
	dbg_begin(DM_CALLS,"atomtype::match_atom");
	static int level = 0;
	int typescore, atomscore, ringscore, n;
	bool found;
	atomtype *bat;
	ringtype *atr;
	ring *r;
	refitem<ring> *refring;
	reflist<atom> atomchecklist;
	reflist<ring> ringchecklist;
	refitem<atom> *ri;
	refitem<ffatom> *rd;
	// Set the scoring to one (which will be the case if there are no specifications to match)
	typescore = 1;
	level ++;
	msg(DM_TYPING,"(%li %2i) Looking to match atom %s: nbonds=%i, env=%s\n",this, level, elements.symbol(i), i->get_nbonds(), text_from_AE(i->get_env()));
	// Element check
	msg(DM_TYPING,"(%li %2i) ... Element  ",this,level);
	if (nallowedel == 0) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		// Check through list of elements/types that this atom is allowed to be
		found = FALSE;
		for (n=0; n<nallowedel; n++)
		{
			// If its an element, just check the element of the atom.
			if (i->is_element(allowedel[n]))
			{
				found = TRUE;
				break;
			}
		}
		if (!found) for (rd = allowedtypes.first(); rd != NULL; rd = rd->next)
		{
			//printf("CHECKING FOR EXACT TYPE (ffid=%i, name=%s)\n",rd->item->get_ffid(),rd->item->get_name());
			// Does this atom match the type descriptions asked for?
			n = rd->item->get_atomtype()->match_atom(i,ringdata,parent,topatom);
			if (n > 0)
			{
				found = TRUE;
				break;
			}
		}
		if (found)
		{
			typescore++;
			msg(DM_TYPING,"[passed]\n");
		}
		else
		{
			msg(DM_TYPING,"[failed - is %i, but type needs %i]\n",i->get_element(),el);
			level --;
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Atom environment check
	msg(DM_TYPING,"(%li %2i) ... Environment  ",this,level);
	if (env == AE_UNSPECIFIED) msg(DM_TYPING," [defaulted]\n");
	else
	{
		if (i->is_env(env))
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%s']\n",text_from_AE(i->get_env()));
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%s', but type needs %s]\n", text_from_AE(i->get_env()), text_from_AE(env));
			level --;
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Oxidation state check
	msg(DM_TYPING,"(%li %2i) ... Oxidation state  ",this,level);
	if (os == 99) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		if (i->is_os(os))
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%i']\n",i->get_os());
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%s', but type needs '%s']\n", i->get_os(), os);
			level --;
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Number of bound atoms check
	msg(DM_TYPING,"(%li %2i) ... Bond number  ",this,level);
	if (nbonds == -1) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		if (i->is_nbonds(nbonds))
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%i']\n",i->get_nbonds());
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%i', but type needs '%i']\n",i->get_nbonds(),nbonds);
			level --;
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Local atom geometry check
	msg(DM_TYPING,"(%li %2i) ... Geometry  ",this,level);
	if (geom == AG_UNSPECIFIED) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		if (i->get_geometry(parent) == geom)
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%s']\n",text_from_AG(geom));
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%s', but type needs '%s']\n",text_from_AG(i->get_geometry(parent)), text_from_AG(geom));
			level --;
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// Construct bound atom list for subsequent checks...
	i->add_bound_to_reflist(&atomchecklist);
	// Hydrogen check
	msg(DM_TYPING,"(%li %2i) ... Attached hydrogens  ",this,level);
	if (nhydrogen == -1) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		// Get number of hydrogen atoms appearing in the atomchecklist...
		n = 0;
		for (ri = atomchecklist.first(); ri != NULL; ri = ri->next) if (ri->item->get_element() == 1) n++;
		if (nhydrogen == n)
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%i']\n",n);
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%i', but type needs '%i']\n",n,nhydrogen);
			level --;
			dbg_end(DM_CALLS,"atomtype::match_atom");
			return 0;
		}
	}
	// List of bound atoms check
	if (boundlist.first() == NULL) msg(DM_TYPING,"(%li %2i) ... Bound atoms  [defaulted]\n",this,level);
	else
	{
		for (bat = boundlist.first(); bat != NULL; bat = bat->next)
		{
			for (n=0; n<bat->nrepeat; n++)
			{
				msg(DM_TYPING,"(%li %2i) ... Bound atom %li (%i/%i):\n",this,level,bat,n+1,bat->nrepeat);
				// Check the atomlist for a match to the bound atomtype
				atomscore = bat->match_in_list(&atomchecklist,ringdata,parent,topatom);
				if (atomscore != 0)
				{
					msg(DM_TYPING,"(%li %2i) ... Bound atom %li (%i/%i) [passed]\n",this,level,bat,n+1,bat->nrepeat);
					typescore += atomscore;
				}
				else
				{
					msg(DM_TYPING,"(%li %2i) ... Bound atom %li (%i/%i) [failed]\n",this,level,bat,n+1,bat->nrepeat);
					level --;
					dbg_end(DM_CALLS,"atomtype::match_atom");
					return 0;
				}
			}
		}
	}
	// Ring check
	if (ringlist.first() == NULL) msg(DM_TYPING,"(%li %2i) ... Rings  [defaulted]\n",this,level);
	else
	{
		// Get list of rings out test atom is involved in
		ringchecklist.clear();
		// Search the list of atoms in this ring for 'i'
		for (r = ringdata->first(); r != NULL; r = r->next)
			if (r->atoms.search(i) != NULL) ringchecklist.add(r,0,0);
		// Loop over ring specifications in atom type
		for (atr = ringlist.first(); atr != NULL; atr = atr->next)
		{
			for (n=0; n<atr->nrepeat; n++)
			{
				msg(DM_TYPING,"(%li %2i) ... Ring (%li) (%i/%i):\n",this,level,atr,n+1,atr->nrepeat);
				// Loop over rings our atom is involved in, searching for a match.
				for (refring = ringchecklist.first(); refring != NULL; refring = refring->next)
				{
					// Initialise score
					ringscore = 0;
					// Check for presence of top atom 
					if (atr->selfabsent)
					{
						 if (refring->item->atoms.search(topatom) != NULL) continue;
						else ringscore ++;
					}
					// Size check
					msg(DM_TYPING,"(%li %2i) ... ... Size  ",this,level);
					if (atr->ringsize == -1)
					{
						
						msg(DM_TYPING,"[defaulted]\n");
						ringscore ++;
					}
					else if (atr->ringsize == refring->item->atoms.size())
					{
						msg(DM_TYPING,"[passed - matched '%i']\n",atr->ringsize);
						ringscore ++;
					}
					else
					{
						msg(DM_TYPING,"[failed]\n");
						continue;
					}
					// Get the list of other atoms in this ring ready for searching.
					msg(DM_TYPING,"(%li %2i) ... ... Atoms:\n", this, level);
					atomchecklist.clear();
					refring->item->add_atoms_to_reflist(&atomchecklist,NULL);
					// Now go through list of specified atomtypes in this ringtype
					for (bat = atr->ringatoms.first(); bat != NULL; bat = bat->next)
					{
						for (n=0; n<bat->nrepeat; n++)
						{
							msg(DM_TYPING,"(%li %2i) ... ... ... Atom (%li) (%i/%i)  ",this,level,bat,n+1,bat->nrepeat);
							atomscore = bat->match_in_list(&atomchecklist, ringdata, parent, topatom);
							if (atomscore != 0)
							{
								msg(DM_TYPING,"[passed]\n");
								ringscore += atomscore;
							}
							else
							{
								msg(DM_TYPING,"[failed]\n");
								// Don't fully return just yet - move onto next ring...
								ringscore = 0;
								break;
							}
						}
						// If ringscore gets reset to 0, we know to break the atoms search
						if (ringscore == 0) break;
					}
					// If we have a match, break out now...
					if (ringscore != 0) break;
				}
				// Check 'refring' - if NULL then we did not manage to find a match for the ring
				if (refring != NULL)
				{
					msg(DM_TYPING,"(%li %2i) ... Ring (%li)  [passed]\n",this,level,atr);
					typescore += ringscore;
					// Remove matched ring from list
					ringchecklist.remove(refring);
				}
				else
				{
					msg(DM_TYPING,"(%li %2i) ... Ring (%li)  [failed]\n",this,level,atr);
					level --;
					dbg_end(DM_CALLS,"atomtype::match_atom");
					return 0;
				}
			} //End of loop over repeats
		}
	}
	// All checks completed, so return the final typing score
	level --;
	dbg_end(DM_CALLS,"atomtype::match_atom");
	return typescore;
}

