/*
	*** Forcefield Commands
	*** src/nucommand/forcefield.cpp
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

#include "main/aten.h"
#include "nucommand/commands.h"
#include "parser/commandnode.h"
#include "model/model.h"
#include "base/elements.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"
#include "base/pattern.h"
#include "base/sysfunc.h"


// Add a new angle definition to the current forcefield
bool NuCommand::function_AngleDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	int n;
	// Get functional form of bond potential
	AngleFunctions::AngleFunction anglestyle = AngleFunctions::angleFunction(c->argc(0));
	if (anglestyle == AngleFunctions::nAngleFunctions) return FALSE;
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
	msg.print(Messenger::Verbose,"Angle %i : %s-%s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", obj.ff->nAngles(), ffb->typeName(0), ffb->typeName(1) , ffb->typeName(2), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5));	rv.reset();
	return TRUE;
}

// Add a new bond definition to the current forcefield
bool NuCommand::function_BondDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	int n;
	// Get functional form of bond potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(c->argc(0));
	if (bondstyle == BondFunctions::nBondFunctions) return FALSE;
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
	rv.reset();
	return TRUE;
}

// Clear manual type mapping list ('clearmap')
bool NuCommand::function_ClearMap(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	aten.typeMap.clear();
	rv.reset();
	return TRUE;
}

// Create energy expression for current model ('createexpression'}
bool NuCommand::function_CreateExpression(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (!obj.m->autocreatePatterns()) return FALSE;
	if (!obj.m->createExpression()) return FALSE;
	rv.reset();
	return TRUE;
}

// Set default forcefield ('defaultff <ff>')
bool NuCommand::function_DefaultFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// If an argument was supplied, select forcefield by name. Otherwise use current
	aten.setDefaultForcefield(aten.findForcefield(c->argc(0)));
	rv.reset();
	return TRUE;
}

// Set equivalent 
bool NuCommand::function_Equivalent(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	rv.reset();
	return FALSE;
}

// Associate current ff to current model ('ffmodel [name]')
bool NuCommand::function_FFModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// If an argument was supplied, select forcefield by name. Otherwise use current
	if (c->hasArg(0)) obj.m->setForcefield(aten.findForcefield(c->argc(0)));
	else obj.m->setForcefield(obj.ff);
	rv.reset();
	return TRUE;
}

// Set current forcefield for named pattern ('ffpattern')
bool NuCommand::function_FFPattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return FALSE;
	// If no argument was given, set the current pattern
	if (!c->hasArg(0)) obj.p->setForcefield(obj.ff);
	else
	{
		Pattern *p = NULL;
		switch (c->argType(0))
		{
			case (NuVTypes::IntegerData):
				p = obj.m->pattern(c->argi(0)-1);
				break;
			case (NuVTypes::CharacterData):
				p = obj.m->findPattern(c->argc(0));
				break;
			case (NuVTypes::PatternData):
				p = (Pattern*) c->argp(0, NuVTypes::ModelData);
				break;
		}
		if (p == NULL)
		{
			msg.print("Invalid pattern specified - current model unchanged.\n");
			return FALSE;
		}
		p->setForcefield(obj.ff);
	}
	rv.reset();
	return TRUE;
}

// Finalise current forcefield
bool NuCommand::function_FinaliseFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	// Print some information about the terms read in from the forcefield
	msg.print("Read in %i type descriptions\n", obj.ff->nTypes() - 1);
	msg.print("Read in %i bond definitions\n", obj.ff->nBonds());
	msg.print("Read in %i angle definitions\n", obj.ff->nAngles());
	msg.print("Read in %i torsion definitions\n", obj.ff->nTorsions());
	// Convert energetic units in the forcefield to the internal units of the program
	obj.ff->convertParameters();
	rv.reset();
	return TRUE;
}

// Set energetic parameters to convert in generator data
bool NuCommand::function_GenConvert(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	for (int n=0; n<c->nArgs(); n++) obj.ff->setEnergyGenerator(c->argi(n));
	rv.reset();
	return TRUE;
}

// Set generator data for atom type
bool NuCommand::function_Generator(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	int n;
	// Convert type name to internal index and read in generator data...
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		msg.print("Unrecognised forcefield typeId %i in generator list.\n",c->argi(0));
		return FALSE;
	}
	// Create generator array on atom
	ffa->initialiseGenerator();
	for (n=1; n<c->nArgs(); n++) ffa->setGenerator(n-1, c->argd(n));
	rv.reset();
	return TRUE;
}

// Select current forcefield ('getff <name>')
bool NuCommand::function_GetFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Forcefield *ff = (c->argType(0) == NuVTypes::IntegerData ? aten.forcefield(c->argi(0)) : aten.findForcefield(c->argc(0)));
	if (ff == NULL)	return FALSE;
	aten.setCurrentForcefield(ff);
	rv.set(NuVTypes::ForcefieldData, ff);
	return TRUE;
}

// Load forcefield ('loadff <filename> [nickname]')
bool NuCommand::function_LoadFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Forcefield *ff = aten.loadForcefield(c->argc(0));
	if (ff == NULL) return FALSE;
	else
	{
		if (c->hasArg(1)) ff->setName(c->argc(1));
		msg.print("Forcefield '%s' loaded, name '%s'\n", c->argc(0), ff->name());
	}
	rv.set(NuVTypes::ForcefieldData, ff);
	return TRUE;
}

// Add manual type mappings ('map <name=element,...>')
bool NuCommand::function_Map(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Get each argument and parse it internally
	int el;
	Namemap<int> *nm;
	LineParser parser;
	for (int m=0; m<c->nArgs(); m++)
	{
		parser.getArgsDelim(c->argc(m), LineParser::Defaults);
		for (int n=0; n<parser.nArgs(); n++)
		{
			el = elements().findAlpha(afterChar(parser.argc(n), '='));
			if (el == 0) msg.print("Unrecognised element '%s' in type map.\n",afterChar(parser.argc(n),'='));
			else
			{
				nm = aten.typeMap.add();
				nm->set(beforeChar(parser.argc(n),'='), el);
			}
		}
	}
	rv.reset();
	return TRUE;
}

// Create new, empty forcefield ('newff <name>')
bool NuCommand::function_NewFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	obj.ff = aten.addForcefield();
	obj.ff->setName(c->argc(0));
	rv.set(NuVTypes::ForcefieldData, obj.ff);
	return TRUE;
}

// Print expression setup ('printsetup')
bool NuCommand::function_PrintSetup(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	msg.print("Current Energy Setup:\n");
	msg.print("Intramolecular Terms : %s\n", (prefs.calculateIntra() ? "On" : "Off"));
	msg.print("       van der Waals : %s\n", (prefs.calculateVdw() ? "On" : "Off"));
	msg.print("      Electrostatics : %s (%s)\n", (prefs.calculateElec() ? "On" : "Off"), Electrostatics::elecMethod(prefs.electrostaticsMethod()));
	msg.print("             Cutoffs : %13.6e (VDW)  %13.6e (elec)\n", prefs.vdwCutoff(), prefs.elecCutoff());
	rv.reset();
	return TRUE;
}

// Set rules to use in parameter generation
bool NuCommand::function_Rules(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	Rules::ForcefieldRules rules = Rules::forcefieldRules(c->argc(0));
	if (rules == Rules::nForcefieldRules) return FALSE;
	msg.print("\t: Rule-set to use is '%s'\n", rules);
	return TRUE;
}

// Save expression ('saveexpression <format> <file>')
bool NuCommand::function_SaveExpression(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Find filter with a nickname matching that given in argc(0)
	Tree *filter = aten.findFilter(Tree::ExpressionExport, c->argc(0));
	// Check that a suitable format was found
	if (filter == NULL)
	{
		msg.print("script : No expression export filter was found that matches the extension '%s'.\nNot saved.\n",c->argc(0));
		return FALSE;
	}
	filter->executeWrite(c->argc(1));
	rv.reset();
	return TRUE;
}

// Add a new torsion definition to the current forcefield
bool NuCommand::function_TorsionDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	int n;
	// Get functional form of bond potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(c->argc(0));
	if (torsionstyle == TorsionFunctions::nTorsionFunctions) return FALSE;
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
	rv.reset();
	return TRUE;
}

// Add a new type definition to the current forcefield
bool NuCommand::function_TypeDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	// Search for this ID to make sure it hasn't already been used
	int newffid = c->argi(0);
	ForcefieldAtom *idsearch = obj.ff->findType(newffid);
	if (idsearch != NULL)
	{
		msg.print("Duplicate forcefield type ID '%i' - already used by type '%s'.\n", newffid, idsearch->name());
		return FALSE;
	}
	ForcefieldAtom *ffa = obj.ff->addType();
	ffa->setTypeId(newffid);
	ffa->setName(c->argc(1));
	ffa->setEquivalent(c->argc(1));
	ffa->atomtype()->setCharacterElement(elements().findAlpha(c->argc(2)));
	ffa->setAtomtype(c->argc(3), obj.ff, ffa);
	if (c->hasArg(4)) ffa->setDescription(c->argc(4));
	rv.reset();
	return TRUE;
}

// Perform typing on current model
bool NuCommand::function_TypeModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	rv.reset();
	return (obj.m->typeAll() ? TRUE : FALSE);
}

// Test specified type ID of current forcefield
bool NuCommand::function_TypeTest(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return FALSE;
	// Find the specified type...
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		msg.print("Type ID %i does not exist in the forcefield '%s'.\n",c->argi(0), obj.ff->name());
		return FALSE;
	}
	else
	{
		if (obj.m->autocreatePatterns())
		{
			// Prepare for typing
			obj.m->describeAtoms();
			// Get atom, element, and the atom's pattern
			Atom *i = obj.m->atomArray()[c->argi(1)-1];
			Pattern *p = obj.m->pattern(i);
			int score = ffa->atomtype()->matchAtom(i,p->ringList(),obj.m,i);
			if (score > 0) msg.print("Atom %i matched type %i (%s) with score %i.\n", i->id()+1, ffa->typeId(), ffa->name(), score);
			else msg.print("Atom %i did not match type %i (%s).\n", i->id()+1, ffa->typeId(), ffa->name());
		}
		else return FALSE;
	}
	rv.reset();
	return TRUE;
}

// Set units used in the forcefield
bool NuCommand::function_Units(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	Prefs::EnergyUnit newunit = Prefs::energyUnit(c->argc(0));
	if (newunit == Prefs::nEnergyUnits) return FALSE;
	obj.ff->setEnergyUnit(newunit);
	msg.print("Forcefield energy unit set to %s\n", Prefs::energyUnit(newunit));
	rv.reset();
	return TRUE;
}

// Add a new intermolecular definition to the current forcefield
bool NuCommand::function_InterDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	// Get functional form of vdw
	VdwFunctions::VdwFunction vdwstyle = VdwFunctions::vdwFunction(c->argc(0));
	if (vdwstyle == VdwFunctions::nVdwFunctions) return FALSE;
	// Find typeId referenced by command
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(1));
	if (ffa == NULL)
	{
		msg.print("TypeId %i has not been defined - can't define VDW data.\n",c->argi(1));
		return FALSE;
	}
	ffa->setCharge(c->argd(2));
	for (int i=3; i<MAXFFPARAMDATA+3; i++) if (c->hasArg(i)) ffa->setParameter(i-3, c->argd(i));
	ffa->setVdwForm(vdwstyle);
	msg.print(Messenger::Verbose,"VDW Data %i : %s %8.4f %8.4f %8.4f %8.4f\n", ffa->typeId(), ffa->name(), ffa->parameter(0), ffa->parameter(1), ffa->parameter(2), ffa->charge());
	rv.reset();
	return TRUE;
}
