/*
	*** Forcefield command functions
	*** src/command/forcefield.cpp
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

#include "main/aten.h"
#include "command/commandlist.h"
#include "model/model.h"
#include "base/elements.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"
#include "base/pattern.h"
#include "base/sysfunc.h"


// Add a new angle definition to the current forcefield
int Command::function_CA_ANGLEDEF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	int n;
	// Get functional form of bond potential
	AngleFunctions::AngleFunction anglestyle = AngleFunctions::angleFunction(c->argc(0));
	if (anglestyle == AngleFunctions::nAngleFunctions) return Command::Fail;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<4; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			msg.print("\t... Warning - bond atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	// Create new ff_bond structure
	ForcefieldBound *ffb = obj.ff->addAngle(anglestyle);
	for (n=1; n<4; n++) ffb->setTypeName(n-1,c->argc(n));
	for (n=4; n<MAXFFPARAMDATA+4; n++) if (c->hasArg(n)) ffb->setParameter(n-4, c->argd(n));
	msg.print(Messenger::Verbose,"Angle %i : %s-%s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", obj.ff->nAngles(), ffb->typeName(0), ffb->typeName(1) , ffb->typeName(2), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5));
	return Command::Success;
}

// Add a new bond definition to the current forcefield
int Command::function_CA_BONDDEF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	int n;
	// Get functional form of bond potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(c->argc(0));
	if (bondstyle == BondFunctions::nBondFunctions) return Command::Fail;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<3; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			msg.print("\t... Warning - bond atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	// Create new ff_bond structure
	ForcefieldBound *ffb = obj.ff->addBond(bondstyle);
	for (n=1; n<3; n++) ffb->setTypeName(n-1, c->argc(n));
	for (n=3; n<MAXFFPARAMDATA+3; n++) if (c->hasArg(n)) ffb->setParameter(n-3, c->argd(n));
	msg.print(Messenger::Verbose,"Bond %i : %s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", obj.ff->nBonds(), ffb->typeName(0), ffb->typeName(1) , ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5)); 
	return Command::Success;
}

// Clear manual type mapping list ('clearmap')
int Command::function_CA_CLEARMAP(CommandNode *&c, Bundle &obj)
{
	aten.typeMap.clear();
	return Command::Success;
}

// Create energy expression for current model ('createexpression'}
int Command::function_CA_CREATEEXPRESSION(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (!obj.m->autocreatePatterns()) return Command::Fail;
	if (!obj.m->createExpression()) return Command::Fail;
	return Command::Success;
}

// Set default forcefield ('defaultff <ff>')
int Command::function_CA_DEFAULTFF(CommandNode *&c, Bundle &obj)
{
	// If an argument was supplied, select forcefield by name. Otherwise use current
	aten.setDefaultForcefield(aten.findForcefield(c->argc(0)));
	return Command::Success;
}

// Set equivalent 
int Command::function_CA_EQUIVALENT(CommandNode *&c, Bundle &obj)
{
	return Command::Fail;
}

// Associate current ff to current model ('ffmodel [name]')
int Command::function_CA_FFMODEL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// If an argument was supplied, select forcefield by name. Otherwise use current
	if (c->hasArg(0)) obj.m->setForcefield(aten.findForcefield(c->argc(0)));
	else obj.m->setForcefield(obj.ff);
	return Command::Success;
}

// Set current forcefield for named pattern ('ffpattern')
int Command::function_CA_FFPATTERN(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return Command::Fail;
	obj.p->setForcefield(obj.ff);
	return Command::Success;
}

// Set current forcefield for pattern id given ('ffpatternid <id>')
int Command::function_CA_FFPATTERNID(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return Command::Fail;
	int nodeid = c->argi(0) - 1;
	if ((nodeid < 0) || (nodeid > obj.m->nPatterns()))
	{
		msg.print("Pattern ID %i is out of range for model (which has %i patterns).\n", nodeid, obj.m->nPatterns());
		return Command::Fail;
	}
	else obj.m->pattern(nodeid)->setForcefield(obj.ff);
	return Command::Success;
}

// Finalise current forcefield
int Command::function_CA_FINALISEFF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	// Print some information about the terms read in from the forcefield
	msg.print("Read in %i type descriptions\n", obj.ff->nTypes() - 1);
	msg.print("Read in %i bond definitions\n", obj.ff->nBonds());
	msg.print("Read in %i angle definitions\n", obj.ff->nAngles());
	msg.print("Read in %i torsion definitions\n", obj.ff->nTorsions());
	// Convert energetic units in the forcefield to the internal units of the program
	obj.ff->convertParameters();
	return Command::Success;
}

// Set energetic parameters to convert in generator data
int Command::function_CA_GENCONVERT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	for (int n=0; n<c->nArgs(); n++) obj.ff->setEnergyGenerator(c->argi(n));
	return Command::Success;
}

// Set generator data for atom type
int Command::function_CA_GENERATOR(CommandNode *&c, Bundle &obj)
{
	int n;
	// Convert type name to internal index and read in generator data...
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		msg.print("Unrecognised forcefield typeId %i in generator list.\n",c->argi(0));
		return Command::Fail;
	}
	// Create generator array on atom
	ffa->initialiseGenerator();
	for (n=1; n<c->nArgs(); n++) ffa->setGenerator(n-1, c->argd(n));
	return Command::Success;
}

// Select current forcefield ('getff <name>')
int Command::function_CA_GETFF(CommandNode *&c, Bundle &obj)
{
	Forcefield *ff = (c->argt(0) == VTypes::IntegerData ? aten.forcefield(c->argi(0)) : aten.findForcefield(c->argc(0)));
	if (ff != NULL)	aten.setCurrentForcefield(ff);
	else return Command::Fail;
	return Command::Success;
}

// Load forcefield ('loadff <filename> [nickname]')
int Command::function_CA_LOADFF(CommandNode *&c, Bundle &obj)
{
	Forcefield *ff = aten.loadForcefield(c->argc(0));
	if (ff == NULL)
	{
		msg.print("Can't find forcefield file '%s' in any location.\n", c->argc(0));
		return Command::Fail;
	}
	else
	{
		if (c->hasArg(1)) ff->setName(c->argc(1));
		msg.print("Forcefield '%s' loaded, name '%s'\n", c->argc(0), ff->name());
	}
	return Command::Success;
}

// Add manual type mappings ('map <name=element,...>')
int Command::function_CA_MAP(CommandNode *&c, Bundle &obj)
{
	// Get the argument and parse it internally
	parser.getArgsDelim(c->argc(0), Parser::Defaults);
	int n, el;
	Namemap<int> *nm;
	for (n=0; n<parser.nArgs(); n++)
	{
		el = elements.find(afterChar(parser.argc(n), '='));
		if (el == 0) msg.print("Unrecognised element '%s' in type map.\n",afterChar(parser.argc(n),'='));
		else
		{
			nm = aten.typeMap.add();
			nm->set(beforeChar(parser.argc(n),'='), el);
		}
	}
	return Command::Success;
}

// Create new, empty forcefield ('newff <name>')
int Command::function_CA_NEWFF(CommandNode *&c, Bundle &obj)
{
	obj.ff = aten.addForcefield();
	obj.ff->setName(c->argc(0));
	return Command::Success;
}

// Print expression setup ('printexpression')
int Command::function_CA_PRINTSETUP(CommandNode *&c, Bundle &obj)
{
	msg.print("Current Energy Setup:\n");
	msg.print("Intramolecular Terms : %s\n", (prefs.calculateIntra() ? "On" : "Off"));
	msg.print("       van der Waals : %s\n", (prefs.calculateVdw() ? "On" : "Off"));
	msg.print("      Electrostatics : %s (%s)\n", (prefs.calculateElec() ? "On" : "Off"), Electrostatics::elecMethod(prefs.electrostaticsMethod()));
	msg.print("             Cutoffs : %13.6e (VDW)  %13.6e (elec)\n", prefs.vdwCutoff(), prefs.elecCutoff());
	return Command::Success;
}

// Set rules to use in parameter generation
int Command::function_CA_RULES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	Rules::ForcefieldRules rules = Rules::forcefieldRules(c->argc(0));
	if (rules == Rules::nForcefieldRules) return Command::Fail;
	msg.print("\t: Rule-set to use is '%s'\n", rules);
	return Command::Success;
}

// Save expression ('saveexpression <format> <file>')
int Command::function_CA_SAVEEXPRESSION(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Find filter with a nickname matching that given in argc(0)
	Filter *f = aten.findFilter(Filter::ExpressionExport, c->argc(0));
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg.print("script : No expression export filter was found that matches the extension '%s'.\nNot saved.\n",c->argc(0));
		return Command::Fail;
	}
	f->execute(c->argc(1));
	return Command::Success;
}

// Add a new torsion definition to the current forcefield
int Command::function_CA_TORSIONDEF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	int n;
	// Get functional form of bond potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(c->argc(0));
	if (torsionstyle == TorsionFunctions::nTorsionFunctions) return Command::Fail;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<4; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			msg.print("\t... Warning - bond atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	// Create new ff_bond structure
	ForcefieldBound *ffb = obj.ff->addTorsion(torsionstyle);
	for (n=1; n<5; n++) ffb->setTypeName(n-1,c->argc(n));
	for (n=5; n<MAXFFPARAMDATA+3; n++) if (c->hasArg(n)) ffb->setParameter(n-5, c->argd(n));
	msg.print(Messenger::Verbose,"TORSION %i : %s-%s-%s-%s  %8.4f %8.4f %8.4f %8.4f, escale=%8.4f vscale=%8.4f\n", obj.ff->nTorsions(), ffb->typeName(0), ffb->typeName(1), ffb->typeName(2), ffb->typeName(3), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5));
	return Command::Success;
}

// Add a new type definition to the current forcefield
int Command::function_CA_TYPEDEF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	// Search for this ID to make sure it hasn't already been used
	int newffid = c->argi(0);
	ForcefieldAtom *idsearch = obj.ff->findType(newffid);
	if (idsearch != NULL)
	{
		msg.print("Duplicate forcefield type ID '%i' - already used by type '%s'.\n", newffid, idsearch->name());
		return Command::Fail;
	}
	ForcefieldAtom *ffa = obj.ff->addType();
	ffa->setTypeId(newffid);
	ffa->setName(c->argc(1));
	ffa->setEquivalent(c->argc(1));
	ffa->atomtype()->setCharacterElement(elements.find(c->argc(2), ElementMap::AlphaZmap));
	ffa->setAtomtype(c->argc(3), obj.ff, ffa);
	if (c->hasArg(4)) ffa->setDescription(c->argc(4));
	return Command::Success;
}

// Perform typing on current model
int Command::function_CA_TYPEMODEL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	return (obj.m->typeAll() ? Command::Success : Command::Fail);
}

// Test specified type ID of current forcefield
int Command::function_CA_TYPETEST(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return Command::Fail;
	// Find the specified type...
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		msg.print("Type ID %i does not exist in the forcefield '%s'.\n",c->argi(0), obj.ff->name());
		return Command::Fail;
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
			if (score != 0) msg.print("Atom %i matched type %i (%s) with score %i.\n", i->id()+1, ffa->typeId(), ffa->name(), score);
			else msg.print("Atom %i did not match type %i (%s).\n", i->id()+1, ffa->typeId(), ffa->name());
		}
		else return Command::Fail;
	}
	return Command::Success;
}

// Set units used in the forcefield
int Command::function_CA_UNITS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	Prefs::EnergyUnit newunit = Prefs::energyUnit(c->argc(0));
	if (newunit == Prefs::nEnergyUnits) return Command::Fail;
	obj.ff->setEnergyUnit(newunit);
	msg.print("Forcefield energy unit set to %s\n", Prefs::energyUnit(newunit));
	return Command::Success;
}

// Add a new VDW definition to the current forcefield
int Command::function_CA_VDWDEF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return Command::Fail;
	// Get functional form of vdw
	VdwFunctions::VdwFunction vdwstyle = VdwFunctions::vdwFunction(c->argc(0));
	if (vdwstyle == VdwFunctions::nVdwFunctions) return Command::Fail;
	// Find typeId referenced by command
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(1));
	if (ffa == NULL)
	{
		msg.print("TypeId %i has not been defined - can't define VDW data.\n",c->argi(1));
		return Command::Fail;
	}
	ffa->setCharge(parser.argd(2));
	for (int i=3; i<MAXFFPARAMDATA+3; i++) if (c->hasArg(i)) ffa->setParameter(i-3, c->argd(i));
	ffa->setVdwForm(vdwstyle);
	msg.print(Messenger::Verbose,"VDW Data %i : %s %8.4f %8.4f %8.4f %8.4f\n", ffa->typeId(), ffa->name(), ffa->parameter(0), ffa->parameter(1), ffa->parameter(2), ffa->charge());
	return Command::Success;
}
