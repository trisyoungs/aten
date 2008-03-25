/*
	*** Forcefield atom type
	*** src/classes/Atomtype.cpp
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

#include "classes/atomtype.h"
#include "classes/atom.h"
#include "classes/bond.h"
#include "classes/ring.h"
#include "classes/forcefield.h"
#include "templates/reflist.h"
#include "base/elements.h"
#include "parse/parser.h"

int printlevel = 0;

// Atom typing commands
const char *ATC_keywords[ATC_NITEMS] = { "sp", "sp2", "sp3", "aromatic", "ring", "noring", "nbonds", "bond", "n", "os", "nh" };
AtomtypeCommand ATC_from_text(const char *s)
	{ return (AtomtypeCommand) enumSearch("",ATC_NITEMS,ATC_keywords,s); }

// Ring typing commands
const char *RTC_keywords[RTC_NITEMS] = { "size", "n", "notself" };
RingTypeCommand RTC_from_text(const char *s)
	{ return (RingTypeCommand) enumSearch("",RTC_NITEMS,RTC_keywords,s); }

// Atom environment
const char *AE_strings[AE_NITEMS] = { "Unspecified", "Unbound atom", "Aliphatic sp3", "Resonant sp2", "Triple-bond sp", "Aromatic sp2" };
const char *text_from_AE(AtomEnv i)
	{ return AE_strings[i]; }

// Geometries about atomic centres
const char *AG_keywords[AG_NITEMS] = { "unspecified", "unbound", "onebond", "linear", "tshape", "trigonal", "tetrahedral", "sqplanar", "tbp", "octahedral" };
AtomGeometry AG_from_text(const char *s)
	{ return (AtomGeometry) enumSearch("atom geometry",AG_NITEMS,AG_keywords,s); }
const char *text_from_AG(AtomGeometry i)
	{ return AG_keywords[i]; }

// Constructors
Atomtype::Atomtype()
{
	// Private variables
	env_ = AE_UNSPECIFIED;
	geometry_ = AG_UNSPECIFIED;
	os_ = 99;
	nAllowedElements_ = 0;
	nBonds_ = -1;
	allowedElements_ = NULL;
	boundBond_ = Bond::Unspecified;
	nRepeat_ = 1;
	acyclic_ = FALSE;
	nHydrogen_ = -1;
	characterElement_ = 0;
	// Public variables
	prev = NULL;
	next = NULL;
}

RingType::RingType()
{
	// Private Variables
	nAtoms_ = -1;
	nRepeat_ = 1;
	selfAbsent_ = FALSE;
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructors
Atomtype::~Atomtype()
{
	// Need to destroy all ring structures and bound atom list
	if (allowedElements_ != NULL) delete[] allowedElements_;
}

// Set character element
void Atomtype::setCharacterElement(int el)
{
	characterElement_ = el;
}

// Return character element
int Atomtype::characterElement()
{
	return characterElement_;
}

// Print Atom Type data
void Atomtype::print()
{
	printlevel ++;
	printf("(%3i) Element :", printlevel);
	if (nAllowedElements_ == 0) printf(" Any");
	else for (int n=0; n<nAllowedElements_; n++) printf(" %s",elements.name(allowedElements_[n]));
	printf("\n");
	printf("(%3i)  Repeat : %i\n", printlevel, nRepeat_);
	if (boundList_.nItems() != 0)
	{
		printf("(%3i)   Atoms : \n", printlevel);
		for (Atomtype *xat = boundList_.first(); xat != NULL; xat = xat->next) xat->print();
	}
	if (ringList_.nItems() != 0)
	{
		printf("(%3i)   Rings : \n", printlevel);
		for (RingType *xring = ringList_.first(); xring != NULL; xring = xring->next) xring->print();
	}
	printlevel --;
}

/*
// asInteger routines
*/

// Set element list in Atomtype
void Atomtype::setElements(const char *ellist, Forcefield *ff)
{
	// Add elements from the comma-separated ellist string as possible matches for this Atomtype
	dbgBegin(DM_CALLS,"Atomtype::setElements");
	int n, count, el;
	ForcefieldAtom *ffa;
	Dnchar temp;
	// Find number of elements given in list...
	parser.getArgsDelim(ellist,PO_DEFAULTS);
	// Use 'nargs' to allocate element list
	nAllowedElements_ = parser.nArgs();
	allowedElements_ = new int[nAllowedElements_];
	count = 0;
	// Go through items in 'element' list...
	msg(DM_TYPING,"  %i atom types/elements given for Atomtype : ",nAllowedElements_);
	for (n=0; n<parser.nArgs(); n++)
	{
		// If name begins with a '$' then we expect an Atomtype id/name and not an element
		if (parser.argc(n)[0] == '$')
		{
			// Copy string and remove leading '$'
			temp = parser.argc(n);
			temp.eraseStart(1);
			// Search for the Atomtype pointer with ffid in 'temp' in the forcefield supplied
			if (ff != NULL)
			{
				ffa = ff->findType(temp.asInteger());
				if (ffa == NULL)
				{
					// TODO Does this need a warning? Will we be able to handle recursive typeid checks properly?
					msg(DM_NONE,"Forcefield type ID/name %s has not yet been defined in the forcefield.\n",temp.get());
				}
				else allowedTypes_.add(ffa);
			}
			else printf("Atomtype::setElements <<<< Type ID/Name found in list, but no forcefield passed >>>>\n");
			msg(DM_TYPING,"%s ",parser.argc(n));
		}
		else
		{
			// WATCH Since Atomtype::el became Atomtype::characterElement_, this does not get set. Should it have been set before? WATCH
			el = elements.find(parser.argc(n),ZM_ALPHA);
			if (el == 0)
			{
				nAllowedElements_ --;
				msg(DM_NONE,"Warning : Unrecognised element in list of bound atoms: '%s'\n",parser.argc(n));
				msg(DM_TYPING,"?%s? ",parser.argc(n));
			}
			else
			{
				allowedElements_[count] = el;
				count ++;
				msg(DM_TYPING,"%s ",parser.argc(n));
			}
		}
	}
	msg(DM_TYPING,"\n");
	dbgEnd(DM_CALLS,"Atomtype::setElements");
}

/*
// Atom Type Ring Functions
*/

// Print
void RingType::print()
{
	printlevel ++;
	printf("(%3i)   Size : %i\n", printlevel, nAtoms_);
	if (ringAtoms_.nItems() != 0)
	{
		printf("(%3i) Contains :\n", printlevel);
		for (Atomtype *xat = ringAtoms_.first(); xat != NULL; xat = xat->next) xat->print();
	}
	printlevel --;
}

/*
// Expand Functions
*/

void RingType::expand(const char *data, Forcefield *ff, ForcefieldAtom *parent)
{
	// Separate function (to prevent brain melting) to recursively create a ring definition.
	// At least allows the restriction (and addition) of commands to the ring command.
	dbgBegin(DM_CALLS,"RingType::expand");
	Dnchar keywd, optlist, def;
	static char c;
	static int level = 0;
	static bool found;
	static RingTypeCommand rtc;
	static Atomtype *newat;
	level ++;
	msg(DM_TYPING,"expand[ring] : Received string [%s]\n",data);
	// Grab the next command, trip the keyword and option list (if there is one).
	def.set(data);
	do
	{
		// Get next command and repeat
		msg(DM_TYPING,"Command String : [%s]\n",def.get());
		optlist = parser.parseAtomtypeString(def);
		keywd = parser.trimAtomtypeKeyword(optlist);
		msg(DM_TYPING,"       Keyword : [%s]\n",keywd.get());
		msg(DM_TYPING,"       Options : [%s]\n",optlist.get());
		found = FALSE;
		// Check for Atomtype specifier ('-' or '='). Both mean the same here...
		c = keywd[0];
		if ((c == '-') || (c == '='))
		{
			// Remove leading character, add bound atom and set its element list
			keywd.eraseStart(1);
			newat = ringAtoms_.add();
			newat->setElements(keywd.get(),ff);
			newat->expand(optlist.get(),ff,parent);
			//if (c == '=') boundBond_ = BT_DOUBLE;
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
					nAtoms_ = atoi(optlist.get());
					break;
				// Repeat specifier
				case (RTC_REPEAT):
					nRepeat_ = atoi(optlist.get());
					break;
				// Presence of self specifier
				case (RTC_NOTSELF):
					selfAbsent_ = TRUE;
					break;
				// Unrecognised
				default:
					if (parent != NULL) msg(DM_NONE,"Unrecognised command '%s' found while expanding ring at depth %i [ffid/name %i/%s].\n", keywd.get(), level, parent->typeId(), parent->name());
					else msg(DM_NONE,"RingType::expand - Unrecognised command (%s).\n", keywd.get());
					break;
			}
		}
	} while (!def.empty());
	level --;
	dbgEnd(DM_CALLS,"RingType::expand");
}

// Master creation routine, returning the head node of an Atomtype structure.
void Atomtype::expand(const char *data, Forcefield *ff, ForcefieldAtom *parent)
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
	dbgBegin(DM_CALLS,"Atomtype::expand");
	Dnchar keywd, optlist, def;
	static RingType *newring;
	static bool found;
	static char c;
	static int n, level = 0;
	static AtomGeometry ag;
	static AtomtypeCommand atc;
	level ++;
	msg(DM_TYPING,"Atomtype::expand - Received string [%s]\n",data);
	if (data[0] == '\0')
	{
		level --;
		dbgEnd(DM_CALLS,"Atomtype::expand");
		return;
	}
	// Grab the next command, strip the keyword and option list (if there is one).
	def.set(data);
	do
	{
		msg(DM_TYPING,"Command String : [%s]\n",def.get());
		optlist = parser.parseAtomtypeString(def);
		keywd = parser.trimAtomtypeKeyword(optlist);
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
			keywd.eraseStart(1);
			Atomtype *newat = boundList_.add();
			newat->setElements(keywd.get(),ff);
			if (c == '=') boundBond_ = Bond::Double;
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
					env_ = AE_SP;
					break;
				case (ATC_SP2):
					env_ = AE_SP2;
					break;
				case (ATC_SP3):
					env_ = AE_SP3;
					break;
				case (ATC_AROMATIC):
					env_ = AE_AROMATIC;
					break;
				// Ring specification (possible options)
				case (ATC_RING):
					newring = ringList_.add();
					newring->expand(optlist.get(),ff,parent);
					break;
				// Disallow rings
				case (ATC_NORING):
					acyclic_ = TRUE;
					break;
				// Request exact bond number
				case (ATC_NBONDS):
					nBonds_ = atoi(optlist.get());
					break;
				// Request exact bond type (bond=BondType)
				case (ATC_BOND):
					boundBond_ = Bond::bondType(optlist.get());
					break;
				// Number of times to match (n=int)
				case (ATC_REPEAT):
					nRepeat_ = atoi(optlist.get());
					break;
				// Oxidation state of element (os=int)
				case (ATC_OS):
					os_ = atoi(optlist.get());
					break;
				// Request no attached hydrogens
				case (ATC_NHYDROGENS):
					nHydrogen_ = atoi(optlist.get());
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
			if (ag != AG_NITEMS) geometry_ = ag;
			else
			{
				if (parent != NULL) msg(DM_NONE,"Unrecognised command '%s' found while expanding atom at depth %i [ffid/name %i/%s].\n", keywd.get(), level, parent->typeId(), parent->name());
				else msg(DM_NONE,"Unrecognised command '%s' found while expanding atom.\n", keywd.get());
				break;
			}
		}
	} while (!def.empty());
	level --;
	dbgEnd(DM_CALLS,"Atomtype::expand");
}

/*
// Match Functions
*/

int Atomtype::matchInList(Reflist<Atom,int> *alist, List<Ring> *ringdata, Model *parent, Atom *topatom)
{
	dbgBegin(DM_CALLS,"Atomtype::matchInList");
	// Search the atomlist supplied for a match to this Atomtype.
	// If we find one, remove the corresponding atom from the atomlist.
	int score = 0, bondscore;
	Refitem<Atom,int> *boundi;
	for (boundi = alist->first(); boundi != NULL; boundi = boundi->next)
	{
		// Extra check for bond type definition here
		if (boundBond_ == Bond::Unspecified) bondscore = 1;
		else (boundBond_ == boundi->data ? bondscore = 1 : bondscore = 0);
		// Now do proper atom type check (if we passed the bond check)
		if (bondscore != 0) score = matchAtom(boundi->item, ringdata, parent, topatom);
		if ((bondscore + score) > 1) break;
	}
	// If boundi is NULL then we finished the loop without finding a match to this Atomtype.
	if (boundi != NULL)
	{
		alist->remove(boundi);
		dbgEnd(DM_CALLS,"Atomtype::matchInList");
		return bondscore+score;
	}
	else
	{
		dbgEnd(DM_CALLS,"Atomtype::matchInList");
		return 0;
	}
}

int Atomtype::matchAtom(Atom* i, List<Ring> *ringdata, Model *parent, Atom *topatom)
{
	// Given the supplied atom pointer and ring data pointer (passed from pattern)
	// see how well the description matches the actual atom, returning as an int. Cycle data is 
	// available in (pattern->)rings. Exit and return 0 as soon as a test fails.
	dbgBegin(DM_CALLS,"Atomtype::match_atom");
	static int level = 0;
	int typescore, atomscore, ringscore, n;
	bool found;
	Atomtype *bat;
	RingType *atr;
	Ring *r;
	ForcefieldAtom *ffa;
	Refitem<Ring,int> *refring;
	Reflist<Atom,int> atomchecklist;
	Reflist<Ring,int> ringchecklist;
	Refitem<Atom,int> *ri;
	Refitem<ForcefieldAtom,int> *rd;
	// Set the scoring to one (which will be the case if there are no specifications to match)
	typescore = 1;
	level ++;
	msg(DM_TYPING,"(%li %2i) Looking to match atom %s: nbonds=%i, env=%s\n", this, level, elements.symbol(i), i->nBonds(), text_from_AE(i->env()));
	// Element check
	msg(DM_TYPING,"(%li %2i) ... Element  ",this,level);
	if (nAllowedElements_ == 0) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		// Check through list of elements/types that this atom is allowed to be
		found = FALSE;
		for (n=0; n<nAllowedElements_; n++)
		{
			// If its an element, just check the element of the atom.
			if (i->isElement(allowedElements_[n]))
			{
				found = TRUE;
				break;
			}
		}
		if (!found) for (rd = allowedTypes_.first(); rd != NULL; rd = rd->next)
		{
			ffa = rd->item;
			//printf("CHECKING FOR EXACT TYPE (ffid=%i, name=%s)\n",ffa->get_ffid(),ffa->name());
			// Check element of type first....
			if (i->element() != ffa->atomType()->characterElement()) continue;
			// Does this atom match the type descriptions asked for?
			n = rd->item->atomType()->matchAtom(i,ringdata,parent,topatom);
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
			msg(DM_TYPING,"[failed - is %i, but type needs %i]\n", i->element(), characterElement_);
			level --;
			dbgEnd(DM_CALLS,"Atomtype::match_atom");
			return 0;
		}
	}
	// Atom environment check
	msg(DM_TYPING,"(%li %2i) ... Environment  ",this,level);
	if (env_ == AE_UNSPECIFIED) msg(DM_TYPING," [defaulted]\n");
	else
	{
		if (i->isEnv(env_))
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%s']\n",text_from_AE(i->env()));
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%s', but type needs %s]\n", text_from_AE(i->env()), text_from_AE(env_));
			level --;
			dbgEnd(DM_CALLS,"Atomtype::match_atom");
			return 0;
		}
	}
	// Oxidation state check
	msg(DM_TYPING,"(%li %2i) ... Oxidation state  ",this,level);
	if (os_ == 99) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		if (i->isOs(os_))
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%i']\n",i->os());
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%s', but type needs '%s']\n", i->os(), os_);
			level --;
			dbgEnd(DM_CALLS,"Atomtype::match_atom");
			return 0;
		}
	}
	// Number of bound atoms check
	msg(DM_TYPING,"(%li %2i) ... Bond number  ",this,level);
	if (nBonds_ == -1) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		if (i->isNBonds(nBonds_))
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%i']\n",i->nBonds());
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%i', but type needs '%i']\n",i->nBonds(),nBonds_);
			level --;
			dbgEnd(DM_CALLS,"Atomtype::match_atom");
			return 0;
		}
	}
	// Local atom geometry check
	msg(DM_TYPING,"(%li %2i) ... Geometry  ",this,level);
	if (geometry_ == AG_UNSPECIFIED) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		if (i->geometry(parent) == geometry_)
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%s']\n",text_from_AG(geometry_));
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%s', but type needs '%s']\n",text_from_AG(i->geometry(parent)), text_from_AG(geometry_));
			level --;
			dbgEnd(DM_CALLS,"Atomtype::match_atom");
			return 0;
		}
	}
	// Construct bound atom list for subsequent checks...
	i->addBoundToReflist(&atomchecklist);
	// Hydrogen check
	msg(DM_TYPING,"(%li %2i) ... Attached hydrogens  ",this,level);
	if (nHydrogen_ == -1) msg(DM_TYPING,"[defaulted]\n");
	else
	{
		// Get number of hydrogen atoms appearing in the atomchecklist...
		n = 0;
		for (ri = atomchecklist.first(); ri != NULL; ri = ri->next) if (ri->item->element() == 1) n++;
		if (nHydrogen_ == n)
		{
			typescore++;
			msg(DM_TYPING,"[passed - matched '%i']\n",n);
		}
		else
		{
			msg(DM_TYPING,"[failed - is '%i', but type needs '%i']\n", n, nHydrogen_);
			level --;
			dbgEnd(DM_CALLS,"Atomtype::match_atom");
			return 0;
		}
	}
	// List of bound atoms check
	if (boundList_.first() == NULL) msg(DM_TYPING,"(%li %2i) ... Bound atoms  [defaulted]\n",this,level);
	else
	{
		for (bat = boundList_.first(); bat != NULL; bat = bat->next)
		{
			for (n=0; n<bat->nRepeat_; n++)
			{
				msg(DM_TYPING,"(%li %2i) ... Bound atom %li (%i/%i):\n",this,level,bat,n+1,bat->nRepeat_);
				// Check the atomlist for a match to the bound Atomtype
				atomscore = bat->matchInList(&atomchecklist,ringdata,parent,topatom);
				if (atomscore != 0)
				{
					msg(DM_TYPING,"(%li %2i) ... Bound atom %li (%i/%i) [passed]\n",this,level,bat,n+1,bat->nRepeat_);
					typescore += atomscore;
				}
				else
				{
					msg(DM_TYPING,"(%li %2i) ... Bound atom %li (%i/%i) [failed]\n",this,level,bat,n+1,bat->nRepeat_);
					level --;
					dbgEnd(DM_CALLS,"Atomtype::match_atom");
					return 0;
				}
			}
		}
	}
	// Ring check
	if (ringList_.first() == NULL) msg(DM_TYPING,"(%li %2i) ... Rings  [defaulted]\n",this,level);
	else
	{
		// Get list of rings out test atom is involved in
		ringchecklist.clear();
		// Search the list of atoms in this ring for 'i'
		for (r = ringdata->first(); r != NULL; r = r->next) if (r->containsAtom(i)) ringchecklist.add(r);
		// Loop over ring specifications in atom type
		for (atr = ringList_.first(); atr != NULL; atr = atr->next)
		{
			for (n=0; n<atr->nRepeat_; n++)
			{
				msg(DM_TYPING,"(%li %2i) ... Ring (%li) (%i/%i):\n",this,level,atr,n+1,atr->nRepeat_);
				// Loop over rings our atom is involved in, searching for a match.
				for (refring = ringchecklist.first(); refring != NULL; refring = refring->next)
				{
					// Initialise score
					ringscore = 0;
					// Check for presence of top atom 
					if (atr->selfAbsent_)
					{
						 if (refring->item->containsAtom(topatom)) continue;
						else ringscore ++;
					}
					// Size check
					msg(DM_TYPING,"(%li %2i) ... ... Size  ",this,level);
					if (atr->nAtoms_ == -1)
					{
						
						msg(DM_TYPING,"[defaulted]\n");
						ringscore ++;
					}
					else if (atr->nAtoms_ == refring->item->nAtoms())
					{
						msg(DM_TYPING,"[passed - matched '%i']\n",atr->nAtoms_);
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
					refring->item->addAtomsToReflist(&atomchecklist,NULL);
					// Now go through list of specified Atomtypes in this RingType
					for (bat = atr->ringAtoms_.first(); bat != NULL; bat = bat->next)
					{
						for (n=0; n<bat->nRepeat_; n++)
						{
							msg(DM_TYPING,"(%li %2i) ... ... ... Atom (%li) (%i/%i)  ",this,level,bat,n+1,bat->nRepeat_);
							atomscore = bat->matchInList(&atomchecklist, ringdata, parent, topatom);
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
					dbgEnd(DM_CALLS,"Atomtype::match_atom");
					return 0;
				}
			} //End of loop over repeats
		}
	}
	// All checks completed, so return the final typing score
	level --;
	dbgEnd(DM_CALLS,"Atomtype::match_atom");
	return typescore;
}

