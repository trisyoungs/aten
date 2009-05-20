/*
	*** Forcefield atom type
	*** src/classes/atomtype.cpp
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

#include "classes/atomtype.h"
#include "base/sysfunc.h"
#include "base/atom.h"
#include "base/dnchar.h"
#include "classes/ring.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"
#include "base/lineparser.h"

int printlevel = 0;

// Local parser
LineParser elparser;

// Atom typing commands
const char *AtomtypeCommandKeywords[Atomtype::nAtomtypeCommands] = { "sp", "sp2", "sp3", "aromatic", "ring", "noring", "nbonds", "bond", "n", "os", "nh", "unbound", "onebond", "linear", "tshape", "trigonal", "tetrahedral", "sqplanar", "tbp", "octahedral" };
Atomtype::AtomtypeCommand Atomtype::atomtypeCommand(const char *s)
{
	return (Atomtype::AtomtypeCommand) enumSearch("",Atomtype::nAtomtypeCommands,AtomtypeCommandKeywords,s);
}

// Ring typing commands
const char *RingtypeCommandKeywords[Ringtype::nRingtypeCommands] = { "size", "n", "aliphatic", "nonaromatic", "aromatic", "notself" };
Ringtype::RingtypeCommand Ringtype::ringtypeCommand(const char *s)
{
	return (Ringtype::RingtypeCommand) enumSearch("",Ringtype::nRingtypeCommands,RingtypeCommandKeywords,s);
}

// Constructors
Atomtype::Atomtype()
{
	// Private variables
	environment_ = Atom::NoEnvironment;
	geometry_ = Atom::NoGeometry;
	os_ = 99;
	nAllowedElements_ = 0;
	nBonds_ = -1;
	allowedElements_ = NULL;
	boundBond_ = Bond::Any;
	nRepeat_ = 1;
	acyclic_ = FALSE;
	nHydrogen_ = -1;
	characterElement_ = -1;

	// Public variables
	prev = NULL;
	next = NULL;
}

Ringtype::Ringtype()
{
	// Private Variables
	nAtoms_ = -1;
	nRepeat_ = 1;
	selfAbsent_ = FALSE;
	type_ = Ring::AnyRing;

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
	else for (int n=0; n<nAllowedElements_; n++) printf(" %s",elements().name(allowedElements_[n]));
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
		for (Ringtype *xring = ringList_.first(); xring != NULL; xring = xring->next) xring->print();
	}
	printlevel --;
}

/*
// Set routines
*/

// Set element list in Atomtype
bool Atomtype::setElements(const char *ellist, Forcefield *ff)
{
	// Add elements from the comma-separated ellist string as possible matches for this Atomtype
	msg.enter("Atomtype::setElements");
	int n, count, el;
	ForcefieldAtom *ffa;
	Dnchar temp;
	// Find number of elements given in list...
	elparser.getArgsDelim(ellist, LineParser::Defaults);
	// Use 'nargs' to allocate element list
	nAllowedElements_ = elparser.nArgs();
	allowedElements_ = new int[nAllowedElements_];
	count = 0;
	// Go through items in 'element' list...
	msg.print(Messenger::Typing,"  %i atom types/elements given for Atomtype : ",nAllowedElements_);
	for (n=0; n<elparser.nArgs(); n++)
	{
		// If name begins with a '&' then we expect an Atomtype id/name and not an element
		if (elparser.argc(n)[0] == '&')
		{
			// Copy string and remove leading '$'
			temp = elparser.argc(n);
			temp.eraseStart(1);
			// Search for the Atomtype pointer with ffid in 'temp' in the forcefield supplied
			if (ff != NULL)
			{
				ffa = ff->findType(temp.asInteger());
				// TODO Does this need a warning? Will we be able to handle recursive typeid checks properly?
				if (ffa == NULL) msg.print("Warning: Forcefield type ID/name %s has not yet been defined in the forcefield.\n",temp.get());
				else allowedTypes_.add(ffa);
			}
			else
			{
				printf("Atomtype::setElements <<<< Type ID/Name found in list, but no forcefield passed >>>>\n");
				msg.exit("Atomtype::setElements");
				return FALSE;
			}
			msg.print(Messenger::Typing,"%s ", elparser.argc(n));
		}
		else
		{
			// WATCH Since Atomtype::el became Atomtype::characterElement_, this does not get set. Should it have been set before? WATCH
			el = elements().findAlpha(elparser.argc(n));
			if (el == 0)
			{
				msg.print("Unrecognised element in list of bound atoms: '%s'\n", elparser.argc(n));
				msg.exit("Atomtype::setElements");
				return FALSE;
			}
			else
			{
				allowedElements_[count] = el;
				count ++;
				msg.print(Messenger::Typing,"%s ", elparser.argc(n));
			}
		}
	}
	msg.print(Messenger::Typing,"\n");
	msg.exit("Atomtype::setElements");
	return TRUE;
}

// Set the bound bond type
void Atomtype::setBoundBond(Bond::BondType bt)
{
	boundBond_ = bt;
}

/*
// Atom Type Ring Functions
*/

// Print
void Ringtype::print()
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

bool Ringtype::expand(const char *data, Forcefield *ff, ForcefieldAtom *parent)
{
	// Separate function (to prevent brain melting) to recursively create a ring definition.
	// At least allows the restriction (and addition) of commands to the ring command.
	msg.enter("Ringtype::expand");
	Dnchar keywd, optlist, def;
	char c;
	int level = 0;
	bool found, hasopts;
	RingtypeCommand rtc;
	Atomtype *newat;
	level ++;
	msg.print(Messenger::Typing,"expand[ring] : Received string [%s]\n",data);
	// Grab the next command, trip the keyword and option list (if there is one).
	def.set(data);
	do
	{
		// Get next command and repeat
		msg.print(Messenger::Typing,"Command String : [%s]\n",def.get());
		optlist = elparser.parseAtomtypeString(def);
		keywd = elparser.trimAtomtypeKeyword(optlist);
		hasopts = optlist.isEmpty() ? FALSE : TRUE;
		msg.print(Messenger::Typing,"       Keyword : [%s]\n",keywd.get());
		msg.print(Messenger::Typing,"       Options : [%s]\n",optlist.get());
		found = FALSE;
		// Check for Atomtype specifier ('-' or '='). Both mean the same here...
		c = keywd[0];
		if ((c == '-') || (c == '='))
		{
			// Must have optlist...
			if (keywd[1] == '\0')
			{
				msg.print("Bound specifiers ('-' or '=') must be given an element, type, or list.\n");
				msg.exit("Ringtype::expand");
				return FALSE;
			}
			// Remove leading character, add bound atom and set its element list
			keywd.eraseStart(1);
			newat = ringAtoms_.add();
			newat->setElements(keywd.get(),ff);
			if (!newat->expand(optlist.get(),ff,parent))
			{
				msg.exit("Ringtype::expand");
				return FALSE;
			}
			if (c == '=') msg.print("Note: bound specified '=' means the same as '-' in a ring definition.\n");
			found = TRUE;
		}
		// Check for keywords (if it wasn't a bound specifier)
		if (!found)
		{
			rtc = Ringtype::ringtypeCommand(keywd.get());
			// Set 'found' to TRUE - we will set it to FALSE again if we don't recognise the command
			found = TRUE;
			switch (rtc)
			{
				// Size specifier
				case (Ringtype::SizeCommand):
					// Must have optlist...
					if (!hasopts)
					{
						msg.print("Ring size must be provided in atomtype description (e.g. size=6).\n");
						msg.exit("Ringtype::expand");
						return FALSE;
					}
					nAtoms_ = atoi(optlist.get());
					break;
				// Repeat specifier
				case (Ringtype::RepeatCommand):
					// Must have optlist...
					if (!hasopts)
					{
						msg.print("Repeat number must be provided in atomtype description (e.g. n=2).\n");
						msg.exit("Ringtype::expand");
						return FALSE;
					}
					nRepeat_ = atoi(optlist.get());
					break;
				// Presence of self specifier
				case (Ringtype::NotSelfCommand):
					selfAbsent_ = TRUE;
					break;
				// Ring type (aliphatic)
				case (Ringtype::AliphaticCommand):
					type_ = Ring::AliphaticRing;
					break;
				// Ring type (non-aromatic)
				case (Ringtype::NonAromaticCommand):
					type_ = Ring::NonAromaticRing;
					break;
				// Ring type (aromatic)
				case (Ringtype::AromaticCommand):
					type_ = Ring::AromaticRing;
					break;
				// Unrecognised
				default:
					if (parent != NULL) msg.print("Unrecognised command '%s' found while expanding ring at depth %i [ffid/name %i/%s].\n", keywd.get(), level, parent->typeId(), parent->name());
					else
					{
						msg.print("Ringtype::expand - Unrecognised command (%s).\n", keywd.get());
						msg.exit("Ringtype::expand");
						return FALSE;
					}
					break;
			}
		}
	} while (!def.isEmpty());
	level --;
	msg.exit("Ringtype::expand");
	return TRUE;
}

// Master creation routine, returning the head node of an Atomtype structure.
bool Atomtype::expand(const char *data, Forcefield *ff, ForcefieldAtom *parent)
{
	// Expands the structure with the commands contained in the supplied string.
	// Format is : X(options,...) where X is the element symbol and 'options' is zero or more of:
	//	-Z(options)	: Atom X is bound to atom Z. Z has a description of 'options'
	//	unbound,sp,sp2,
	//	sp3,aromatic	: Atom X has the specified hybridisation / is aromatic / is unbound.
	//	ring()		: Atom X is involved in a cycle of some kind (see above)
	// Options are comma- or space-separated. Defaults are 'don't care' where applicable.
	// The supplied string should contain a keyword followed by (optional) bracketed list of specs.
	// Parent ring structure must be supplied when descending into a ring options structure.
	// Parent pointer is used for error reporting
	msg.enter("Atomtype::expand");
	Dnchar keywd, optlist, def;
	Ringtype *newring;
	bool found, hasopts;
	char c;
	int n, level = 0;
	Atom::AtomGeometry ag;
	AtomtypeCommand atc;
	level ++;
	msg.print(Messenger::Typing,"Atomtype::expand - Received string [%s]\n",data);
	if (data[0] == '\0')
	{
		level --;
		msg.exit("Atomtype::expand");
		return TRUE;
	}
	// Grab the next command, strip the keyword and option list (if there is one).
	def.set(data);
	do
	{
		msg.print(Messenger::Typing,"Command String : [%s]\n",def.get());
		optlist = elparser.parseAtomtypeString(def);
		keywd = elparser.trimAtomtypeKeyword(optlist);
		hasopts = optlist.isEmpty() ? FALSE : TRUE;
		msg.print(Messenger::Typing,"       Keyword : [%s]\n",keywd.get());
		msg.print(Messenger::Typing,"       Options : [%s]\n",optlist.get());
		// Check for 'bound to' specifiers first ('-' or '=')
		// Bound atom spec - create new subnode and add it to the bound list.
		// Format is either '-X(options...)' or '-[X1,X2,X3](options...)' (single or list of elements respectively)
		found = FALSE;
		c = keywd[0];
		if ((c == '-') || (c == '='))
		{
			// Must have more data...
			if (keywd[1] == '\0')
			{
				msg.print("Bound specifiers ('-' or '=') must be given an element, type, or list.\n");
				msg.exit("Atomtype::expand");
				return FALSE;
			}
			// Remove leading character, add bound atom and set its element list
			keywd.eraseStart(1);
			Atomtype *newat = boundList_.add();
			newat->setElements(keywd.get(),ff);
			if (c == '=') newat->setBoundBond(Bond::Double);
			if (!newat->expand(optlist.get(),ff,parent))
			{
				msg.exit("Atomtype::expand");
				return FALSE;
			}
			found = TRUE;
		}
		// Check for keywords (if it wasn't a bound specifier)
		if (!found)
		{
			atc = Atomtype::atomtypeCommand(keywd.get());
			// Set 'found' to TRUE - we will set it to FALSE again if we don't recognise the command
			found = TRUE;
			switch (atc)
			{
				// Hybridisation / environment settings (no options)
				case (Atomtype::SpCommand):
					environment_ = Atom::SpEnvironment;
					break;
				case (Atomtype::Sp2Command):
					environment_ = Atom::Sp2Environment;
					break;
				case (Atomtype::Sp3Command):
					environment_ = Atom::Sp3Environment;
					break;
				// Ring specification (possible options)
				case (Atomtype::RingCommand):
					newring = ringList_.add();
					if ((hasopts) && (!newring->expand(optlist.get(),ff,parent)))
					{
						msg.exit("Atomtype::expand");
						return FALSE;
					}	
					break;
				// Disallow rings
				case (Atomtype::NoRingCommand):
					acyclic_ = TRUE;
					break;
				// Request exact bond number
				case (Atomtype::NBondsCommand):
					// Must have optlist...
					if (!hasopts)
					{
						msg.print("Number of bonds must be provided in atomtype description (e.g. nbonds=2).\n");
						msg.exit("Atomtype::expand");
						return FALSE;
					}
					nBonds_ = atoi(optlist.get());
					break;
				// Request exact bond type (bond=BondType)
				case (Atomtype::BondCommand):
					// Must have optlist...
					if (!hasopts)
					{
						msg.print("Bond type must be provided in atomtype description (e.g. bond=single).\n");
						msg.exit("Atomtype::expand");
						return FALSE;
					}
					boundBond_ = Bond::bondType(optlist.get());
					break;
				// Number of times to match (n=int)
				case (Atomtype::RepeatCommand):
					// Must have optlist...
					if (!hasopts)
					{
						msg.print("Repeat number must be provided in atomtype description (e.g. n=5).\n");
						msg.exit("Atomtype::expand");
						return FALSE;
					}
					nRepeat_ = atoi(optlist.get());
					break;
				// Oxidation state of element (os=int)
				case (Atomtype::OxidationStateCommand):
					// Must have optlist...
					if (!hasopts)
					{
						msg.print("Oxidation state must be provided in atomtype description (e.g. os=1).\n");
						msg.exit("Atomtype::expand");
						return FALSE;
					}
					os_ = atoi(optlist.get());
					break;
				// Request no attached hydrogens
				case (Atomtype::NHydrogensCommand):
					// Must have optlist...
					if (!hasopts)
					{
						msg.print("Number of hydrogens must be provided in atomtype description (e.g. nh=3).\n");
						msg.exit("Atomtype::expand");
						return FALSE;
					}
					nHydrogen_ = atoi(optlist.get());
					break;
				// Atom geometries
				case (UnboundCommand):
				case (OneBondCommand):
				case (LinearCommand):
				case (TShapeCommand):
				case (TrigPlanarCommand):
				case (TetrahedralCommand):
				case (SquarePlanarCommand):
				case (TrigBipyramidCommand):
				case (OctahedralCommand):
					// Convert atomtype geometry keyword to Atom::Geometry enum
					ag = Atom::atomGeometry(keywd.get());
					if (ag != Atom::nAtomGeometries) geometry_ = ag;
					else printf("Internal Error: Failed to reconvert atom geometry in atomtype description.\n");
					break;
				default:
					found = FALSE;
					break;
			}
		}
		// Check for geometry specifications (if it wasn't a bound specifier or type command)
		if (!found)
		{
			if (parent != NULL) msg.print("Unrecognised command '%s' found while expanding atom at depth %i [ffid/name %i/%s].\n", keywd.get(), level, parent->typeId(), parent->name());
			else
			{
				msg.print("Unrecognised command '%s' found while expanding atom.\n", keywd.get());
				msg.exit("Atomtype::expand");
				return FALSE;
			}
			break;
		}
	} while (!def.isEmpty());
	level --;
	msg.exit("Atomtype::expand");
	return TRUE;
}

/*
// Match Functions
*/

int Atomtype::matchInList(Reflist<Atom,int> *alist, List<Ring> *ringdata, Model *parent, Atom *topatom)
{
	msg.enter("Atomtype::matchInList");
	// Search the atomlist supplied for a match to this Atomtype.
	// If we find one, remove the corresponding atom from the atomlist.
	int score, bondscore;
	Refitem<Atom,int> *boundi;
	for (boundi = alist->first(); boundi != NULL; boundi = boundi->next)
	{
		// Extra check for bond type definition here
		if (boundBond_ == Bond::Any) bondscore = 0;
		else if (boundBond_ == boundi->data) bondscore = 1;
		else continue;
		// Now do proper atom type check (if we passed the bond check)
		score = matchAtom(boundi->item, ringdata, parent, topatom);
		if (score > -1) break;
	}
	// If boundi is NULL then we finished the loop without finding a match to this Atomtype.
	if (boundi != NULL)
	{
		alist->remove(boundi);
		msg.exit("Atomtype::matchInList");
		return bondscore+score;
	}
	msg.exit("Atomtype::matchInList");
	return -1;
}

int Atomtype::matchAtom(Atom* i, List<Ring> *ringdata, Model *parent, Atom *topatom)
{
	// Given the supplied atom pointer and ring data pointer (passed from pattern)
	// see how well the description matches the actual atom, returning as an int. Cycle data is 
	// available in (pattern->)rings. Exit and return -1 as soon as a test fails.
	msg.enter("Atomtype::matchAtom");
	static int level = 0;
	int typescore, atomscore, ringscore, n;
	bool found;
	Atomtype *bat;
	Ringtype *atr;
	Ring *r;
	ForcefieldAtom *ffa;
	Refitem<Ring,int> *refring;
	Reflist<Atom,int> atomchecklist;
	Reflist<Ring,int> ringchecklist;
	Refitem<Atom,int> *ri;
	Refitem<ForcefieldAtom,int> *rd;
	// If a character element is specified, check that our target atom is the correct element. Otherwise, initialise typescore to zero
	if (characterElement_ != -1)
	{
		if (i->element() != characterElement_)
		{
			msg.exit("Atomtype::matchAtom");
			return -1;
		}
		else typescore = 1;
	}
	else typescore = 0;
	level ++;
	msg.print(Messenger::Typing,"(%li %2i) Looking to match atom %s: nbonds=%i, env=%s\n", this, level, elements().symbol(i), i->nBonds(), Atom::atomEnvironment(i->environment()));
	// Element check
	msg.print(Messenger::Typing,"(%li %2i) ... Element  ",this,level);
	if (nAllowedElements_ == 0) msg.print(Messenger::Typing,"[defaulted]\n");
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
				typescore++;
				break;
			}
		}
		if (!found) for (rd = allowedTypes_.first(); rd != NULL; rd = rd->next)
		{
			ffa = rd->item;
			//printf("CHECKING FOR EXACT TYPE (ffid=%i, name=%s)\n",ffa->get_ffid(),ffa->name());
			// Check element of type first....
			if (i->element() != ffa->atomtype()->characterElement()) continue;
			// Does this atom match the type descriptions asked for?
			n = rd->item->atomtype()->matchAtom(i,ringdata,parent,topatom);
			if (n > -1)
			{
				found = TRUE;
				typescore += n;
				break;
			}
		}
		if (found) msg.print(Messenger::Typing,"[passed]\n");
		else
		{
			msg.print(Messenger::Typing,"[failed - is %i, but type needs %i]\n", i->element(), characterElement_);
			level --;
			msg.exit("Atomtype::matchAtom");
			return -1;
		}
	}
	// Atom environment check
	msg.print(Messenger::Typing,"(%li %2i) ... Environment  ",this,level);
	if (environment_ == Atom::NoEnvironment) msg.print(Messenger::Typing," [defaulted]\n");
	else
	{
		if (i->isEnvironment(environment_))
		{
			typescore++;
			msg.print(Messenger::Typing,"[passed - matched '%s']\n", Atom::atomEnvironment(i->environment()));
		}
		else
		{
			msg.print(Messenger::Typing,"[failed - is '%s', but type needs %s]\n", Atom::atomEnvironment(i->environment()), Atom::atomEnvironment(environment_));
			level --;
			msg.exit("Atomtype::matchAtom");
			return -1;
		}
	}
	// Oxidation state check
	msg.print(Messenger::Typing,"(%li %2i) ... Oxidation state  ",this,level);
	if (os_ == 99) msg.print(Messenger::Typing,"[defaulted]\n");
	else
	{
		if (i->isOs(os_))
		{
			typescore++;
			msg.print(Messenger::Typing,"[passed - matched '%i']\n",i->os());
		}
		else
		{
			msg.print(Messenger::Typing,"[failed - is '%i', but type needs '%i']\n", i->os(), os_);
			level --;
			msg.exit("Atomtype::matchAtom");
			return -1;
		}
	}
	// Number of bound atoms check
	msg.print(Messenger::Typing,"(%li %2i) ... Bond number  ",this,level);
	if (nBonds_ == -1) msg.print(Messenger::Typing,"[defaulted]\n");
	else
	{
		if (i->isNBonds(nBonds_))
		{
			typescore++;
			msg.print(Messenger::Typing,"[passed - matched '%i']\n",i->nBonds());
		}
		else
		{
			msg.print(Messenger::Typing,"[failed - is '%i', but type needs '%i']\n",i->nBonds(),nBonds_);
			level --;
			msg.exit("Atomtype::matchAtom");
			return -1;
		}
	}
	// Local atom geometry check
	msg.print(Messenger::Typing,"(%li %2i) ... Geometry  ",this,level);
	if (geometry_ == Atom::NoGeometry) msg.print(Messenger::Typing,"[defaulted]\n");
	else
	{
		if (i->geometry(parent) == geometry_)
		{
			typescore++;
			msg.print(Messenger::Typing,"[passed - matched '%s']\n", Atom::atomGeometry(geometry_));
		}
		else
		{
			msg.print(Messenger::Typing,"[failed - is '%s', but type needs '%s']\n", Atom::atomGeometry(i->geometry(parent)), Atom::atomGeometry(geometry_));
			level --;
			msg.exit("Atomtype::matchAtom");
			return -1;
		}
	}
	// Construct bound atom list for subsequent checks...
	i->addBoundToReflist(&atomchecklist);
	// Hydrogen check
	msg.print(Messenger::Typing,"(%li %2i) ... Attached hydrogens  ",this,level);
	if (nHydrogen_ == -1) msg.print(Messenger::Typing,"[defaulted]\n");
	else
	{
		// Get number of hydrogen atoms appearing in the atomchecklist...
		n = 0;
		for (ri = atomchecklist.first(); ri != NULL; ri = ri->next) if (ri->item->element() == 1) n++;
		if (nHydrogen_ == n)
		{
			typescore++;
			msg.print(Messenger::Typing,"[passed - matched '%i']\n",n);
		}
		else
		{
			msg.print(Messenger::Typing,"[failed - is '%i', but type needs '%i']\n", n, nHydrogen_);
			level --;
			msg.exit("Atomtype::matchAtom");
			return -1;
		}
	}
	// List of bound atoms check
	if (boundList_.first() == NULL) msg.print(Messenger::Typing,"(%li %2i) ... Bound atoms  [defaulted]\n",this,level);
	else
	{
		for (bat = boundList_.first(); bat != NULL; bat = bat->next)
		{
			for (n=0; n<bat->nRepeat_; n++)
			{
				msg.print(Messenger::Typing,"(%li %2i) ... Bound atom %li (n=%i/%i): ",this,level,bat,n+1,bat->nRepeat_);
				// Check the atomlist for a match to the bound Atomtype
				atomscore = bat->matchInList(&atomchecklist,ringdata,parent,topatom);
				if (atomscore != -1)
				{
					msg.print(Messenger::Typing,"[passed]\n");
					typescore += atomscore;
				}
				else
				{
					msg.print(Messenger::Typing,"[failed]\n");
					level --;
					msg.exit("Atomtype::matchAtom");
					return -1;
				}
			}
		}
	}
	// Ring check
	if (ringList_.first() == NULL)
	{
		if (acyclic_)
		{
			// Get list of rings our test atom is involved in
			ringchecklist.clear();
			// Search the list of atoms in this ring for 'i'
			for (r = ringdata->first(); r != NULL; r = r->next) if (r->containsAtom(i)) ringchecklist.add(r);
			if (acyclic_ && (ringchecklist.nItems() != 0))
			{
				msg.print(Messenger::Typing,"(%li %2i) [failed - acyclic specified but is in %i rings]\n",this,level,ringchecklist.nItems());
				level --;
				msg.exit("Atomtype::matchAtom");
				return -1;
			}
		}
		msg.print(Messenger::Typing,"(%li %2i) ... Rings  [defaulted]\n",this,level);
	}
	else
	{
		// Get list of rings our test atom is involved in
		ringchecklist.clear();
		// Search the list of atoms in this ring for 'i'
		for (r = ringdata->first(); r != NULL; r = r->next) if (r->containsAtom(i)) ringchecklist.add(r);
		// Loop over ring specifications in atom type
		for (atr = ringList_.first(); atr != NULL; atr = atr->next)
		{
			for (n=0; n<atr->nRepeat_; n++)
			{
				msg.print(Messenger::Typing,"(%li %2i) ... Ring (%li) (%i/%i):\n",this,level,atr,n+1,atr->nRepeat_);
				// Loop over rings our atom is involved in, searching for a match.
				for (refring = ringchecklist.first(); refring != NULL; refring = refring->next)
				{
					// Initialise score
					ringscore = 0;
					// Check for specific type of ring first...
					msg.print(Messenger::Typing,"(%li %2i) ... ... Type  ",this,level);
					if (atr->type_ == Ring::AnyRing) msg.print(Messenger::Typing,"[defaulted]");
					else if (atr->type_ == refring->item->type())
					{
						msg.print(Messenger::Typing,"[passed - matched '%s']\n",Ring::ringType(atr->type_));
						ringscore ++;
					}
					else
					{
						msg.print(Messenger::Typing,"[failed]\n");
						continue;
					}
					// Check for presence of top atom 
					if ((atr->selfAbsent_) && (refring->item->containsAtom(topatom))) continue;
					else ringscore ++;
					// Size check
					msg.print(Messenger::Typing,"(%li %2i) ... ... Size  ",this,level);
					if (atr->nAtoms_ == -1) msg.print(Messenger::Typing,"[defaulted]\n");
					else if (atr->nAtoms_ == refring->item->nAtoms())
					{
						msg.print(Messenger::Typing,"[passed - matched '%i']\n",atr->nAtoms_);
						ringscore ++;
					}
					else
					{
						msg.print(Messenger::Typing,"[failed]\n");
						continue;
					}
					// Get the list of other atoms in this ring ready for searching.
					msg.print(Messenger::Typing,"(%li %2i) ... ... Atoms:\n", this, level);
					atomchecklist.clear();
					refring->item->addAtomsToReflist(&atomchecklist,NULL);
					// Now go through list of specified Atomtypes in this Ringtype
					for (bat = atr->ringAtoms_.first(); bat != NULL; bat = bat->next)
					{
						for (n=0; n<bat->nRepeat_; n++)
						{
							msg.print(Messenger::Typing,"(%li %2i) ... ... ... Atom (%li) (%i/%i)  ",this,level,bat,n+1,bat->nRepeat_);
							atomscore = bat->matchInList(&atomchecklist, ringdata, parent, topatom);
							if (atomscore > -1)
							{
								msg.print(Messenger::Typing,"[passed]\n");
								ringscore += atomscore;
							}
							else
							{
								msg.print(Messenger::Typing,"[failed]\n");
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
					msg.print(Messenger::Typing,"(%li %2i) ... Ring (%li)  [passed]\n",this,level,atr);
					typescore += ringscore;
					// Remove matched ring from list
					ringchecklist.remove(refring);
				}
				else
				{
					msg.print(Messenger::Typing,"(%li %2i) ... Ring (%li)  [failed]\n",this,level,atr);
					level --;
					msg.exit("Atomtype::matchAtom");
					return -1;
				}
			} //End of loop over repeats
		}
	}
	// All checks completed, so return the final typing score
	level --;
	msg.exit("Atomtype::matchAtom");
	return typescore;
}

