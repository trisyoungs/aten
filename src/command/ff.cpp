/*
	*** Forcefield command functions
	*** src/command/ff.cpp
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

#include "command/commandlist.h"
#include "base/master.h"
#include "base/elements.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"
#include "model/model.h"

// Add a new angle definition to the current forcefield
int CommandData::function_CA_ANGLEDEF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	int n;
	// Get functional form of bond potential
	AngleFunctions::AngleFunction anglestyle = AngleFunctions::angleFunction(c->argc(0));
	if (anglestyle == AngleFunctions::nAngleFunctions) return CR_FAIL;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<4; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			msg(Debug::None,"\t... Warning - bond atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	// Create new ff_bond structure
	ForcefieldBound *ffb = obj.ff->addAngle(anglestyle);
	for (n=1; n<4; n++) ffb->setTypeName(n-1,c->argc(n));
	for (n=4; n<MAXFFPARAMDATA+4; n++) if (c->hasArg(n)) ffb->params().data[n-4] = c->argd(n);
	msg(Debug::Verbose,"BOND %i : %s  %s  %8.4f %8.4f\n", n, ffb->typeName(0), ffb->typeName(1) , ffb->params().data[0], ffb->params().data[1], ffb->params().data[2], ffb->params().data[3], ffb->params().data[4], ffb->params().data[5]);
	return CR_SUCCESS;
}

// Add a new bond definition to the current forcefield
int CommandData::function_CA_BONDDEF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	int n;
	// Get functional form of bond potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(c->argc(0));
	if (bondstyle == BondFunctions::nBondFunctions) return CR_FAIL;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<3; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			msg(Debug::None,"\t... Warning - bond atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	// Create new ff_bond structure
	ForcefieldBound *ffb = obj.ff->addBond(bondstyle);
	for (n=1; n<3; n++) ffb->setTypeName(n-1, c->argc(n));
	for (n=3; n<MAXFFPARAMDATA+3; n++) if (c->hasArg(n)) ffb->params().data[n-3] = c->argd(n);
	msg(Debug::Verbose,"BOND %i : %s  %s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, ffb->typeName(0), ffb->typeName(1) , ffb->params().data[0], ffb->params().data[1], ffb->params().data[2], ffb->params().data[3], ffb->params().data[4], ffb->params().data[5]); 
	return CR_SUCCESS;
}

// Clear manual type mapping list ('clearmap')
int CommandData::function_CA_CLEARMAP(Command *&c, Bundle &obj)
{
	master.typeMap.clear();
	return CR_SUCCESS;
}

// Set default forcefield ('defaultff <ff>')
int CommandData::function_CA_DEFAULTFF(Command *&c, Bundle &obj)
{
	// If an argument was supplied, select forcefield by name. Otherwise use current
	master.setDefaultForcefield(master.findForcefield(c->argc(0)));
	return CR_SUCCESS;
}

// Set equivalent 
int CommandData::function_CA_EQUIVALENT(Command *&c, Bundle &obj)
{
}

// Associate current ff to current model ('ffmodel [name]')
int CommandData::function_CA_FFMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// If an argument was supplied, select forcefield by name. Otherwise use current
	if (c->hasArg(0)) obj.m->setForcefield(master.findForcefield(c->argc(0)));
	else obj.m->setForcefield(obj.ff);
	return CR_SUCCESS;
}

// Set current forcefield for named pattern ('ffpattern')
int CommandData::function_CA_FFPATTERN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL+BP_FF)) return CR_FAIL;
	obj.p->setForcefield(obj.ff);
	return CR_SUCCESS;
}

// Set current forcefield for pattern id given ('ffpatternid <id>')
int CommandData::function_CA_FFPATTERNID(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL+BP_FF)) return CR_FAIL;
	int nodeid = c->argi(0) - 1;
	if ((nodeid < 0) || (nodeid > obj.m->nPatterns()))
	{
		msg(Debug::None,"Pattern ID %i is out of range for model (which has %i patterns).\n", nodeid, obj.m->nPatterns());
		return CR_FAIL;
	}
	else obj.m->pattern(nodeid)->setForcefield(obj.ff);
	return CR_SUCCESS;
}

// Finalise current forcefield
int CommandData::function_CA_FINALISEFF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	// Print some information about the terms read in from the forcefield
	msg(Debug::None,"Read in %i type descriptions\n",obj.ff->nTypes());
	msg(Debug::None,"Read in %i bond definitions\n",obj.ff->nBonds());
	msg(Debug::None,"Read in %i angle definitions\n",obj.ff->nAngles());
	msg(Debug::None,"Read in %i torsion definitions\n",obj.ff->nTorsions());
	// Convert energetic units in the forcefield to the internal units of the program
	obj.ff->convertParameters();
	return CR_SUCCESS;
}

// Set energetic parameters to convert in generator data
int CommandData::function_CA_GENCONVERT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	for (int n=0; n<c->nArgs(); n++) obj.ff->setEnergyGenerator(c->argi(n));
	return CR_SUCCESS;
}

// Set generator data for atom type
int CommandData::function_CA_GENERATOR(Command *&c, Bundle &obj)
{
	int success, n;
	// Convert type name to internal index and read in generator data...
	// Typename (arg 1) is unused, but is present in the file to aid readability
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		msg(Debug::None,"Unrecognised forcefield typeId %i in generator list.\n",c->argi(0));
		return CR_FAIL;
	}
	// Create generator array on atom
	ffa->initialiseGenerator();
	for (n=1; n<c->nArgs(); n++) ffa->setGenerator(n-1, c->argd(n));
	return CR_SUCCESS;
}

// Load forcefield ('loadff <filename> [nickname]')
int CommandData::function_CA_LOADFF(Command *&c, Bundle &obj)
{
	// Try some different locations to find the supplied forcefield.
	static char s[512];
	Forcefield *ff = NULL;
	// First try - actual / absolute path
	msg(Debug::Verbose,"Looking for forcefield in absolute path (%s)...\n",c->argc(0));
	if (fileExists(c->argc(0))) ff = master.loadForcefield(c->argc(0));
	else
	{
		// Second try - master.dataDir/ff
		sprintf(s,"%s/%s",master.dataDir.get(), c->argc(0));
		msg(Debug::Verbose,"Looking for forcefield in installed location (%s)...\n",s);
		if (fileExists(s)) ff = master.loadForcefield(s);
		else
		{
			// Last try - user home datadir/ff
			sprintf(s,"%s/.aten/ff/%s",master.homeDir.get(), c->argc(0));
			msg(Debug::Verbose,"Looking for forcefield in user's data directory (%s)...\n",s);
			if (fileExists(s)) ff = master.loadForcefield(s);
			else msg(Debug::None,"Can't find forcefield file '%s' in any location.\n",c->argc(0));
		}
	}
	if (ff != NULL)
	{
		master.setCurrentForcefield(ff);
		if (c->hasArg(1)) ff->setName(c->argc(1));
		msg(Debug::None,"Forcefield '%s' loaded, name '%s'\n", c->argc(0), ff->name());
		return CR_SUCCESS;
	}
	else return CR_FAIL;
}

// Select current forcefield ('getff <name>')
int CommandData::function_CA_GETFF(Command *&c, Bundle &obj)
{
	Forcefield *ff = (c->argt(0) == Variable::IntegerVariable ? master.forcefield(c->argi(0)) : master.findForcefield(c->argc(0)));
	if (ff != NULL)	master.setCurrentForcefield(ff);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Add manual type mappings ('map <name=element,...>')
int CommandData::function_CA_MAP(Command *&c, Bundle &obj)
{
	// Get the argument and parse it internally
	parser.getArgsDelim(c->argc(0), Parser::Defaults);
	int n, el;
	Namemap<int> *nm;
	for (n=0; n<parser.nArgs(); n++)
	{
		el = elements.find(afterChar(parser.argc(n), '='));
		if (el == 0) msg(Debug::None,"Unrecognised element '%s' in type map.\n",afterChar(parser.argc(n),'='));
		else
		{
			nm = master.typeMap.add();
			nm->set(beforeChar(parser.argc(n),'='), el);
		}
	}
	return CR_SUCCESS;
}

// Set rules to use in parameter generation
int CommandData::function_CA_RULES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	Forcefield::ForcefieldRules rules = Forcefield::forcefieldRules(c->argc(0));
	if (rules == Forcefield::nForcefieldRules) return CR_FAIL;
	msg(Debug::None,"\t: Rule-set to use is '%s'\n", rules);
	return CR_SUCCESS;
}

// Add a new torsion definition to the current forcefield
int CommandData::function_CA_TORSIONDEF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	int n;
	// Get functional form of bond potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(c->argc(0));
	if (torsionstyle == TorsionFunctions::nTorsionFunctions) return CR_FAIL;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<4; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			msg(Debug::None,"\t... Warning - bond atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	// Create new ff_bond structure
	ForcefieldBound *ffb = obj.ff->addTorsion(torsionstyle);
	for (n=1; n<5; n++) ffb->setTypeName(n-1,c->argc(n));
	for (n=5; n<MAXFFPARAMDATA+5; n++) if (c->hasArg(n)) ffb->params().data[n-5] = c->argd(n);
	msg(Debug::Verbose,"TORSION %i : %s  %s  %8.4f %8.4f\n", n, ffb->typeName(0), ffb->typeName(1) , ffb->params().data[0], ffb->params().data[1], ffb->params().data[2], ffb->params().data[3], ffb->params().data[4], ffb->params().data[5]);
	return CR_SUCCESS;
}

// Add a new type definition to the current forcefield
int CommandData::function_CA_TYPEDEF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	// Search for this ID to make sure it hasn't already been used
	int newffid = c->argi(0);
	ForcefieldAtom *idsearch = obj.ff->findType(newffid);
	if (idsearch != NULL)
	{
		msg(Debug::None,"Duplicate forcefield type ID '%i' - already used by type '%s'.\n", newffid, idsearch->name());
		return CR_FAIL;
	}
	ForcefieldAtom *ffa = obj.ff->addType();
	ffa->setTypeId(newffid);
	ffa->setName(c->argc(1));
	ffa->setEquivalent(c->argc(1));
	ffa->atomtype()->setCharacterElement(elements.find(c->argc(2), Prefs::AlphaZmap));
	ffa->setAtomtype(c->argc(3), obj.ff, ffa);
	if (c->hasArg(4)) ffa->setDescription(c->argc(4));
	return CR_SUCCESS;
}

// Perform typing on current model
int CommandData::function_CA_TYPEMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	return (obj.m->typeAll() ? CR_SUCCESS : CR_FAIL);
}

// Test specified type ID of current forcefield
int CommandData::function_CA_TYPETEST(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL+BP_FF)) return CR_FAIL;
	// Find the specified type...
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		msg(Debug::None,"Type ID %i does not exist in the forcefield '%s'.\n",c->argi(0), obj.ff->name());
		return CR_FAIL;
	}
	else
	{
		if (obj.m->autocreatePatterns())
		{
			// Prepare for typing
			obj.m->describeAtoms();
			// Get atom, element, and the atom's pattern
			Atom *i = obj.m->atomArray()[c->argi(1)-1];
			int el = i->element();
			Pattern *p = obj.m->pattern(i);
			int score = ffa->atomtype()->matchAtom(i,p->ringList(),obj.m,i);
			if (score != 0) msg(Debug::None,"Atom %i matched type %i (%s) with score %i.\n", i->id()+1, ffa->typeId(), ffa->name(), score);
			else msg(Debug::None,"Atom %i did not match type %i (%s).\n", i->id()+1, ffa->typeId(), ffa->name());
		}
		else return CR_FAIL;
	}
	return CR_SUCCESS;
}

// Set units used in the forcefield
int CommandData::function_CA_UNITS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	Prefs::EnergyUnit newunit = Prefs::energyUnit(c->argc(0));
	if (newunit == Prefs::nEnergyUnits) return CR_FAIL;
	obj.ff->setEnergyUnit(newunit);
	msg(Debug::None,"Forcefield energy unit set to %s\n", Prefs::energyUnit(newunit));
	return CR_SUCCESS;
}

// Add a new VDW definition to the current forcefield
int CommandData::function_CA_VDWDEF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_FF)) return CR_FAIL;
	// Get functional form of vdw
	VdwFunctions::VdwFunction vdwstyle = VdwFunctions::vdwFunction(c->argc(0));
	if (vdwstyle == VdwFunctions::nVdwFunctions) return CR_FAIL;
	// Find typeId referenced by command
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(1));
	if (ffa == NULL)
	{
		msg(Debug::None,"TypeId %i has not been defined - can't define VDW data.\n",c->argi(1));
		return CR_FAIL;
	}
	ffa->setCharge(parser.argd(3));
	for (int i=4; i<MAXFFPARAMDATA+4; i++) if (c->hasArg(i)) ffa->params().data[i-4] = c->argd(i);
	ffa->setVdwForm(vdwstyle);
	msg(Debug::Verbose,"VDW Data %i : %s %8.4f %8.4f %8.4f %8.4f\n", ffa->typeId(), ffa->name(), ffa->params().data[0], ffa->params().data[1], ffa->params().data[2], ffa->charge());
	return CR_SUCCESS;
}
