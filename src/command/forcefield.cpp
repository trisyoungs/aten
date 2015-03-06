/*
	*** Forcefield Commands
	*** src/command/forcefield.cpp
	Copyright T. Youngs 2007-2015

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

#include "command/commands.h"
#include "main/aten.h"
#include "ff/forcefield.h"
#include "base/forcefieldatom.h"
#include "base/forcefieldbound.h"
#include "base/pattern.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Add a new angle definition to the current forcefield
bool Commands::function_AngleDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	int n;
	// Get functional form of bond potential
	AngleFunctions::AngleFunction anglestyle = AngleFunctions::angleFunction(c->argc(0), TRUE);
	if (anglestyle == AngleFunctions::nAngleFunctions) return FALSE;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<4; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			Messenger::print("\t... Warning - angle atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	// Create new ff_bond structure
	ForcefieldBound* ffb = obj.ff->addAngle(anglestyle);
	for (n=1; n<4; n++) ffb->setTypeName(n-1,c->argc(n));
	for (n=4; n<MAXFFPARAMDATA+4; n++) if (c->hasArg(n)) ffb->setParameter(n-4, c->argd(n));
	Messenger::print(Messenger::Verbose, "Angle %i : %s-%s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", obj.ff->nAngles(), ffb->typeName(0), ffb->typeName(1) , ffb->typeName(2), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5));
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return TRUE;
}

// Cet current autoconversion unit
bool Commands::function_AutoConversionUnit(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	if (!c->parent()->isFilter())
	{
		Messenger::print("The 'autoconversionunit' command can only be used from within a Filter.\n");
		return FALSE;
	}
	if (c->hasArg(0))
	{
		Prefs::EnergyUnit eu = Prefs::energyUnit(c->argc(0), TRUE);
		if (eu == Prefs::nEnergyUnits) return FALSE;
		else prefs.setAutoConversionUnit(eu);
	}
	else prefs.setAutoConversionUnit(Prefs::nEnergyUnits);
	return TRUE;
}

// Add a new bond definition to the current forcefield
bool Commands::function_BondDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	int n;
	// Get functional form of bond potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(c->argc(0),TRUE);
	if (bondstyle == BondFunctions::nBondFunctions) return FALSE;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<3; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			Messenger::print("\t... Warning - bond atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	// Create new ff_bond structure
	ForcefieldBound* ffb = obj.ff->addBond(bondstyle);
	for (n=1; n<3; n++) ffb->setTypeName(n-1, c->argc(n));
	for (n=3; n<MAXFFPARAMDATA+3; n++) if (c->hasArg(n)) ffb->setParameter(n-3, c->argd(n));
	Messenger::print(Messenger::Verbose, "Bond %i : %s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", obj.ff->nBonds(), ffb->typeName(0), ffb->typeName(1) , ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5)); 
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return TRUE;
}

// Clear manual export type mapping list ('clearexportmap')
bool Commands::function_ClearExportMap(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	aten_.typeExportMap.clear();
	rv.reset();
	return TRUE;
}

// Clear energy expression for current model ('clearexpression'}
bool Commands::function_ClearExpression(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->clearExpression();
	rv.reset();
	return TRUE;
}

// Clear manual type mapping list ('clearmap')
bool Commands::function_ClearMap(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	aten_.typeImportMap.clear();
	rv.reset();
	return TRUE;
}

// Clear atom types from current model
bool Commands::function_ClearTypes(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->removeTyping();
	rv.reset();
	return TRUE;
}

// Create energy expression for current model ('createexpression(bool vdwOnly, bool allowdummy, bool assigncharges)'}
bool Commands::function_CreateExpression(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (!obj.m->createPatterns()) return FALSE;
	Choice vdwOnly, allowDummy, assignCharges;
	if (c->hasArg(0)) vdwOnly = c->argb(0) ? Choice::Yes : Choice::No;
	if (c->hasArg(1)) allowDummy = c->argb(1) ? Choice::Yes : Choice::No;
	if (c->hasArg(2)) assignCharges = c->argb(2) ? Choice::Yes : Choice::No;
	bool result = obj.m->createExpression(vdwOnly, allowDummy, assignCharges, aten_.currentForcefield(), aten_.combinationRules());
	rv.set(result);
	return TRUE;
}

// Set current forcefield
bool Commands::function_CurrentFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Forcefield* ff = NULL;
	if (c->hasArg(0))
	{
		switch (c->argType(0))
		{
			case (VTypes::IntegerData):
				ff = aten_.forcefield(c->argi(0)-1);
				break;
			case (VTypes::StringData):
				ff = aten_.findForcefield(c->argc(0));
				break;
			case (VTypes::ForcefieldData):
				ff = (Forcefield*) c->argp(0, VTypes::ForcefieldData);
				break;
			default:
				Messenger::print("Can't convert a variable of type '%s' into a Forcefield.\n", VTypes::dataType(c->argType(0)));
				break;
		}
		if (ff == NULL)	return FALSE;
		aten_.setCurrentForcefield(ff);
		rv.set(VTypes::ForcefieldData, ff);
	}
	else rv.set(VTypes::ForcefieldData, aten_.currentForcefield());
	return TRUE;
}

// Delete forcefield
bool Commands::function_DeleteFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Forcefield* ff = NULL;
	rv.reset();
	if (c->hasArg(0))
	{
		switch (c->argType(0))
		{
			case (VTypes::IntegerData):
				ff = aten_.forcefield(c->argi(0)-1);
				break;
			case (VTypes::StringData):
				ff = aten_.findForcefield(c->argc(0));
				break;
			case (VTypes::ForcefieldData):
				ff = (Forcefield*) c->argp(0, VTypes::ForcefieldData);
				break;
			default:
				Messenger::print("Can't convert a variable of type '%s' into a Forcefield.\n", VTypes::dataType(c->argType(0)));
				break;
		}
		if (ff == NULL)	return FALSE;
		aten_.removeForcefield(ff);
	}
	return TRUE;
}

// Set energetic parameters to convert in generator data
bool Commands::function_EnergyConvert(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	for (int n=0; n<c->nArgs(); n++) obj.ff->addEnergyData(c->argc(n));
	rv.reset();
	return TRUE;
}

// Set equivalent
bool Commands::function_Equivalent(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	ForcefieldAtom* ffa;
	LineParser parser;
	// Loop over command arguments
	for (int n=1; n<c->nArgs(); ++n)
	{
		// Split command into comma-delimited sections
		parser.getArgsDelim(0, c->argc(n));
		for (int i=0; i<parser.nArgs(); ++i)
		{
			for (ffa = obj.ff->types(); ffa != NULL; ffa = ffa->next)
				if (obj.ff->matchType(ffa->name(),parser.argc(i)) < 10) ffa->setEquivalent(c->argc(0));
		}
	}
	rv.reset();
	return TRUE;
}

// Add manual export type mappings ('exportmap <typename=name,...>')
bool Commands::function_ExportMap(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Get each argument and parse it internally
	LineParser parser;
	for (int m=0; m<c->nArgs(); m++)
	{
		parser.getArgsDelim(0, c->argc(m));
		for (int n=0; n<parser.nArgs(); n++)
		{
			if (strchr(parser.argc(n),'=') == NULL)
			{
				Messenger::print("Mangled exportmap value found (i.e. it contains no '='): '%s'.\n", parser.argc(n));
				continue;
			}
			aten_.typeExportMap.add(beforeChar(parser.argc(n),'='), afterChar(parser.argc(n),'='));
		}
	}
	rv.reset();
	return TRUE;
}

// Associate current ff to current model ('ffmodel [name]')
bool Commands::function_FFModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// If an argument was supplied, select forcefield by name. Otherwise use current
	if (c->hasArg(0)) obj.m->setForcefield(aten_.findForcefield(c->argc(0)));
	else obj.m->setForcefield(obj.ff);
	rv.reset();
	return TRUE;
}

// Set current forcefield for named pattern ('ffpattern')
bool Commands::function_FFPattern(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return FALSE;
	// If no argument was given, set the current pattern
	if (!c->hasArg(0)) obj.p->setForcefield(obj.ff);
	else
	{
		Pattern* p = NULL;
		switch (c->argType(0))
		{
			case (VTypes::IntegerData):
				p = obj.m->pattern(c->argi(0)-1);
				break;
			case (VTypes::StringData):
				p = obj.m->findPattern(c->argc(0));
				break;
			case (VTypes::PatternData):
				p = (Pattern*) c->argp(0, VTypes::ModelData);
				break;
			default:
				Messenger::print("Can't convert a variable of type '%s' into a Forcefield.\n", VTypes::dataType(c->argType(0)));
				break;
		}
		if (p == NULL)
		{
			Messenger::print("Invalid pattern specified - current model unchanged.\n");
			return FALSE;
		}
		p->setForcefield(obj.ff);
	}
	rv.reset();
	return TRUE;
}

// Finalise current forcefield
bool Commands::function_FinaliseFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	// Print some information about the terms read in from the forcefield
	Messenger::print("Forcefield now contains:\n");
	Messenger::print("\t%i type descriptions\n", obj.ff->nTypes() - 1);
	Messenger::print("\t%i bond definitions\n", obj.ff->nBonds());
	Messenger::print("\t%i angle definitions\n", obj.ff->nAngles());
	Messenger::print("\t%i torsion definitions\n", obj.ff->nTorsions());
	// Check that some forcefield types were defined...
	if (obj.ff->nTypes() <= 1) Messenger::print("Warning - no types are defined in this forcefield.\n");
	// Link forcefield type references (&N) to their actual forcefield types
	for (ForcefieldAtom* ffa = obj.ff->types(); ffa != NULL; ffa = ffa->next) ffa->neta()->linkReferenceTypes();
	// Convert energetic units in the forcefield to the internal units of the program
	obj.ff->convertParameters();
	rv.reset();
	return TRUE;
}

// Fix atom types
bool Commands::function_FixType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	ForcefieldAtom* ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		if (c->argType(0) == VTypes::IntegerData) Messenger::print("Forcefield type ID %i not defined in forcefield '%s'.\n", c->argi(0), obj.ff->name());
		else if (c->argType(0) == VTypes::StringData) Messenger::print("Forcefield type '%s' not defined in forcefield '%s'.\n", c->argc(0), obj.ff->name());
		rv.reset();
		return FALSE;
	}
	if (c->hasArg(1))
	{
		Atom* i = c->argType(1) == VTypes::IntegerData ? obj.m->atom(c->argi(1)-1) : (Atom*) c->argp(1, VTypes::AtomData);
		if (i == NULL) return FALSE;
		obj.m->setAtomType(i, ffa, TRUE);
		Messenger::print("Atom type for atom id %i fixed to %i (%s/%s).\n", i->id()+1, c->argi(0), ffa->name(), ffa->equivalent());
	}
	else for (Refitem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next)
	{
		obj.m->setAtomType(ri->item, ffa, TRUE);
		Messenger::print("Atom type for atom id %i fixed to %i (%s/%s).\n", ri->item->id()+1, c->argi(0), ffa->name(), ffa->equivalent());
	}
	obj.m->changeLog.add(Log::Structure);
	rv.reset();
	return TRUE;
}

// Free atom types
bool Commands::function_FreeType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (c->hasArg(1))
	{
		Atom* i = c->argType(1) == VTypes::IntegerData ? obj.m->atom(c->argi(1)-1) : (Atom*) c->argp(1, VTypes::AtomData);
		if (i == NULL) return FALSE;
		obj.m->setAtomType(i, i->type(), FALSE);
	}
	else for (Refitem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next) obj.m->setAtomType(ri->item, ri->item->type(), TRUE);
	obj.m->changeLog.add(Log::Structure);
	rv.reset();
	return TRUE;
}

// Generate (or return existing) bound parameters for specified angle interaction
bool Commands::function_GenerateAngle(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer+Bundle::ModelPointer)) return FALSE;
	// Find named atoms in forcefield
	Atom* atoms[3];
	for (int i=0; i<3; ++i) atoms[i] = (c->argType(i) == VTypes::AtomData ? (Atom*) c->argp(i, VTypes::AtomData) : obj.rs()->atom(c->argi(i)-1));
	// Check atom and associated type pointers
	for (int i=0; i<3; ++i)
	{
		if (atoms[i] == NULL)
		{
			Messenger::print("Atom %i given to 'generateangle' is NULL.\n", i);
			return FALSE;
		}
		if (atoms[i]->type() == NULL)
		{
			Messenger::print("Atom %i given to 'generateangle' has no forcefield atom assigned.\n", i);
			return FALSE;
		}
	}
	// Does a suitable definition already exist?
	ForcefieldBound* ffb = obj.ff->findAngle(atoms[0]->type(), atoms[1]->type(), atoms[2]->type());
	if (ffb == NULL) ffb = obj.ff->generateAngle(atoms[0], atoms[1], atoms[2]);
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return TRUE;
}

// Generate (or return existing) bound parameters for specified bond interaction
bool Commands::function_GenerateBond(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer+Bundle::ModelPointer)) return FALSE;
	// Find named atoms in forcefield
	Atom* atoms[2];
	for (int i=0; i<2; ++i) atoms[i] = (c->argType(i) == VTypes::AtomData ? (Atom*) c->argp(i, VTypes::AtomData) : obj.rs()->atom(c->argi(i)-1));
	// Check atom and associated type pointers
	for (int i=0; i<2; ++i)
	{
		if (atoms[i] == NULL)
		{
			Messenger::print("Atom %i given to 'generatebond' is NULL.\n", i);
			return FALSE;
		}
		if (atoms[i]->type() == NULL)
		{
			Messenger::print("Atom %i given to 'generatebond' has no forcefield atom assigned.\n", i);
			return FALSE;
		}
	}
	// Does a suitable definition already exist?
	ForcefieldBound* ffb = obj.ff->findBond(atoms[0]->type(), atoms[1]->type());
	if (ffb == NULL) ffb = obj.ff->generateBond(atoms[0], atoms[1]);
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return TRUE;
}

// Generate (or return existing) bound parameters for specified torsion interaction
bool Commands::function_GenerateTorsion(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer+Bundle::ModelPointer)) return FALSE;
	// Find named atoms in forcefield
	Atom* atoms[4];
	for (int i=0; i<4; ++i) atoms[i] = (c->argType(i) == VTypes::AtomData ? (Atom*) c->argp(i, VTypes::AtomData) : obj.rs()->atom(c->argi(i)-1));
	// Check atom and associated type pointers
	for (int i=0; i<4; ++i)
	{
		if (atoms[i] == NULL)
		{
			Messenger::print("Atom %i given to 'generatetorsion' is NULL.\n", i);
			return FALSE;
		}
		if (atoms[i]->type() == NULL)
		{
			Messenger::print("Atom %i given to 'generatetorsion' has no forcefield atom assigned.\n", i);
			return FALSE;
		}
	}
	// Does a suitable definition already exist?
	ForcefieldBound* ffb = obj.ff->findTorsion(atoms[0]->type(), atoms[1]->type(), atoms[2]->type(), atoms[3]->type());
	if (ffb == NULL) ffb = obj.ff->generateTorsion(atoms[0], atoms[1], atoms[2], atoms[3]);
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return TRUE;
}

// Generate (or return existing) vdw parameters for specified atom
bool Commands::function_GenerateVdw(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer+Bundle::ModelPointer)) return FALSE;
	// Find named atoms in forcefield
	Atom* i = (c->argType(0) == VTypes::AtomData ? (Atom*) c->argp(0, VTypes::AtomData) : obj.rs()->atom(c->argi(0)-1));
	// Check atom and associated type pointers
	if (i == NULL)
	{
		Messenger::print("Atom given to 'generatevdw' is NULL.\n");
		return FALSE;
	}
	ForcefieldAtom* ffa = i->type();
	if (ffa == NULL)
	{
		Messenger::print("Atom given to 'generatevdw' has no forcefield atom assigned.\n");
		return FALSE;
	}
	if (ffa->vdwForm() == VdwFunctions::None) obj.ff->generateVdw(i);
	rv.set(VTypes::ForcefieldAtomData, ffa->vdwForm() == VdwFunctions::None ? NULL : ffa);
	return TRUE;
}

// Get combination rule in use for VDW parameter
bool Commands::function_GetCombinationRule(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.reset();
	// First, get functional form
	VdwFunctions::VdwFunction form = VdwFunctions::vdwFunction(c->argc(0), TRUE);
	if (form == VdwFunctions::nVdwFunctions) return FALSE;
	// Next, get functional form parameter
	int param = VdwFunctions::vdwParameter(form, c->argc(1), TRUE);
	if (param == VdwFunctions::VdwFunctions[form].nParameters) return FALSE;
	// Everything OK, so return combination rule in use
	rv.set(CombinationRules::combinationRule( VdwFunctions::VdwFunctions[form].combinationRules[param] ));
	return TRUE;
}

// Retrieve forcefield
bool Commands::function_GetFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Forcefield* ff = NULL;
	switch (c->argType(0))
	{
		case (VTypes::IntegerData):
			ff = aten_.forcefield(c->argi(0)-1);
			break;
		case (VTypes::StringData):
			ff = aten_.findForcefield(c->argc(0));
			break;
		case (VTypes::ForcefieldData):
			ff = (Forcefield*) c->argp(0, VTypes::ForcefieldData);
			break;
		default:
			Messenger::print("Can't convert a variable of type '%s' into a Forcefield.\n", VTypes::dataType(c->argType(0)));
			break;
	}
	if (ff == NULL)	return FALSE;
	rv.set(VTypes::ForcefieldData, ff);
	return TRUE;
}

// Add a new intermolecular definition to the current forcefield
bool Commands::function_InterDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	// Get functional form of vdw
	VdwFunctions::VdwFunction vdwstyle = VdwFunctions::vdwFunction(c->argc(0), TRUE);
	if (vdwstyle == VdwFunctions::nVdwFunctions) return FALSE;
	// Find typeId referenced by command
	ForcefieldAtom* ffa = obj.ff->findType(c->argi(1));
	if (ffa == NULL)
	{
		Messenger::print("TypeId %i has not been defined - can't define VDW data.\n",c->argi(1));
		return FALSE;
	}
	ffa->setVdwForm(vdwstyle);
	ffa->setCharge(c->argd(2));
	for (int i=3; i<MAXFFPARAMDATA+3; i++) if (c->hasArg(i)) ffa->setParameter(i-3, c->argd(i));
	Messenger::print(Messenger::Verbose, "VDW Data %i : %s %8.4f %8.4f %8.4f %8.4f\n", ffa->typeId(), ffa->name(), ffa->parameter(0), ffa->parameter(1), ffa->parameter(2), ffa->charge());
	rv.reset();
	return TRUE;
}

// Load forcefield ('loadff <filename> [nickname]')
bool Commands::function_LoadFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Forcefield* ff = aten_.loadForcefield(c->argc(0));
	if (ff == NULL) return FALSE;
	else
	{
		if (c->hasArg(1)) ff->setName(c->argc(1));
		Messenger::print("Forcefield '%s' loaded, name '%s'\n", c->argc(0), ff->name());
	}
	rv.set(VTypes::ForcefieldData, ff);
	return TRUE;
}

// Add manual type mappings ('map <name=element,...>')
bool Commands::function_Map(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Get each argument and parse it internally
	int el;
	NameMap<int>* nm;
	LineParser parser;
	for (int m=0; m<c->nArgs(); m++)
	{
		parser.getArgsDelim(0, c->argc(m));
		for (int n=0; n<parser.nArgs(); n++)
		{
			if (strchr(parser.argc(n),'=') == NULL)
			{
				Messenger::print("Mangled map value found (i.e. it contains no '='): '%s'.\n", parser.argc(n));
				continue;
			}
			el = Elements().find(afterChar(parser.argc(n), '='), ElementMap::AlphaZMap);
			if (el == 0) Messenger::print("Unrecognised element '%s' in type map.\n",afterChar(parser.argc(n),'='));
			else
			{
				nm = aten_.typeImportMap.add();
				nm->set(beforeChar(parser.argc(n),'='), el);
			}
		}
	}
	rv.reset();
	return TRUE;
}

// Create new, empty forcefield
bool Commands::function_NewFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	obj.ff = aten_.addForcefield(c->argc(0));
	rv.set(VTypes::ForcefieldData, obj.ff);
	return TRUE;
}

// Print expression setup
bool Commands::function_PrintSetup(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Messenger::print("Current Energy Setup:\n");
	Messenger::print("Intramolecular Terms : %s\n", (prefs.calculateIntra() ? "On" : "Off"));
	Messenger::print("       van der Waals : %s\n", (prefs.calculateVdw() ? "On" : "Off"));
	Messenger::print("      Electrostatics : %s\n", Electrostatics::elecMethod(prefs.electrostaticsMethod()));
	Messenger::print("             Cutoffs : %13.6e (VDW)  %13.6e (elec)\n", prefs.vdwCutoff(), prefs.elecCutoff());
	rv.reset();
	return TRUE;
}

// Print type specified
bool Commands::function_PrintType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	// Does the identified type exist in the forcefield
	ForcefieldAtom* ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		Messenger::print("Error: Type id %i is not defined in forcefield '%s'.\n", c->argi(0), obj.ff->name());
		return FALSE;
	}
	Messenger::print("Internal NETA description for type '%i' (%s, equivalent = %s)\n", ffa->typeId(), ffa->name(), ffa->equivalent());
	ffa->neta()->print();
	rv.reset();
	return TRUE;
}

// Recreate energy expression for current model ('recreateexpression(bool vdwOnly, bool allowdummy, bool assigncharges)'}
bool Commands::function_RecreateExpression(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->clearExpression();
	if (!obj.m->createPatterns()) return FALSE;
	Choice vdwOnly, allowDummy, assignCharges;
	if (c->hasArg(0)) vdwOnly = c->argb(0) ? Choice::Yes : Choice::No;
	if (c->hasArg(1)) allowDummy = c->argb(1) ? Choice::Yes : Choice::No;
	if (c->hasArg(2)) assignCharges = c->argb(2) ? Choice::Yes : Choice::No;
	if (!obj.m->createExpression(vdwOnly, allowDummy, assignCharges, aten_.currentForcefield(), aten_.combinationRules())) return FALSE;
	rv.reset();
	return TRUE;
}

// Save expression ('saveexpression <format> <file>')
bool Commands::function_SaveExpression(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	
	
	// Parse the first option so we can get the filter nickname and any filter options
	LineParser parser;
	parser.getArgsDelim(LineParser::UseQuotes, c->argc(0));
	
	// First part of argument is nickname
	Tree* filter = aten_.findFilter(FilterData::ExpressionExport, parser.argc(0));
	// Check that a suitable format was found
	if (filter == NULL)
	{
		// Print list of valid filter nicknames
		aten_.printValidNicknames(FilterData::ExpressionExport);
		Messenger::print("Not saved.\n");
		return FALSE;
	}

	// Loop over remaining arguments which are widget/global variable assignments
	for (int n = 1; n < parser.nArgs(); ++n) if (!filter->setAccessibleVariable(beforeStr(parser.argc(n),"="), afterStr(parser.argc(n),"="))) return FALSE;

	// Temporarily disable undo/redo for the model, save, and re-enable
	obj.rs()->disableUndoRedo();
	bool result = filter->executeWrite(c->argc(1));
	obj.rs()->enableUndoRedo();
	if (result) Messenger::print("Expression for model '%s' saved to file '%s' (%s)\n", obj.rs()->name(), c->argc(1), filter->filter.name());
	else Messenger::print("Failed to save expression for model '%s'.\n", obj.rs()->name());
	return result;
}

// Set combination rule in use for VDW parameter
bool Commands::function_SetCombinationRule(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.reset();
	
	// First, get functional form
	VdwFunctions::VdwFunction form = VdwFunctions::vdwFunction(c->argc(0), TRUE);
	if (form == VdwFunctions::nVdwFunctions) return FALSE;
	
	// Next, get functional form parameter
	int param = VdwFunctions::vdwParameter(form, c->argc(1), TRUE);
	if (param == VdwFunctions::VdwFunctions[form].nParameters) return FALSE;
	
	// Finally, search combination rule
	CombinationRules::CombinationRule cr = CombinationRules::combinationRule(c->argc(2), TRUE);
	if (cr == CombinationRules::nCombinationRules) return FALSE;
	
	// Everything OK, so set data
	VdwFunctions::VdwFunctions[form].combinationRules[param] = cr;
	return TRUE;
}

// Add a new torsion definition to the current forcefield
bool Commands::function_TorsionDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	int n;
	
	// Get functional form of bond potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(c->argc(0), TRUE);
	if (torsionstyle == TorsionFunctions::nTorsionFunctions) return FALSE;
	
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<4; n++)
	{
		if ((strchr(c->argc(n),'*') == NULL) && (obj.ff->findType(c->argc(n)) == NULL))
			Messenger::print("\t... Warning - torsion atom '%s' does not exist in the forcefield!\n", c->argc(n));
	}
	
	// Create new ff_bond structure
	ForcefieldBound* ffb = obj.ff->addTorsion(torsionstyle);
	for (n=1; n<5; n++) ffb->setTypeName(n-1,c->argc(n));
	for (n=5; n<MAXFFPARAMDATA+3; n++) if (c->hasArg(n)) ffb->setParameter(n-5, c->argd(n));
	Messenger::print(Messenger::Verbose, "TORSION %i : %s-%s-%s-%s  %8.4f %8.4f %8.4f %8.4f, escale=%8.4f vscale=%8.4f\n", obj.ff->nTorsions(), ffb->typeName(0), ffb->typeName(1), ffb->typeName(2), ffb->typeName(3), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5));
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return TRUE;
}

// Add a new type definition to the current forcefield
bool Commands::function_TypeDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	// Search for this ID to make sure it hasn't already been used
	int newffid = c->argi(0);
	ForcefieldAtom* idsearch = obj.ff->findType(newffid);
	if (idsearch != NULL)
	{
		Messenger::print("Duplicate forcefield type ID '%i' - already used by type '%s'.\n", newffid, idsearch->name());
		return FALSE;
	}
	ForcefieldAtom* ffa = obj.ff->addType();
	ffa->setTypeId(newffid);
	ffa->setName(c->argc(1));
	ffa->setEquivalent(c->argc(2));
	ffa->setElement(c->argz(3));
	ffa->neta()->setCharacterElement(c->argz(3));
	ffa->setNeta(c->argc(4), obj.ff);
	if (c->hasArg(5)) ffa->setDescription(c->argc(5));
	rv.set(VTypes::ForcefieldAtomData, ffa);
	return TRUE;
}

// Perform typing on current model
bool Commands::function_TypeModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	rv.set(obj.m->typeAll(aten_.currentForcefield()));
	return TRUE;
}

// Test specified type ID of current forcefield
bool Commands::function_TypeTest(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return FALSE;
	rv.reset();
	// Find the specified type...
	ForcefieldAtom* ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		Messenger::print("Type ID %i does not exist in the forcefield '%s'.\n",c->argi(0), obj.ff->name());
		return FALSE;
	}
	else
	{
		if (obj.m->createPatterns())
		{
			// Prepare for typing
			obj.m->describeAtoms();
			// Get atom, element, and the atom's pattern
			Atom* i = NULL;
			if (c->argType(1) == VTypes::AtomData) i = (Atom*) c->argp(1, VTypes::AtomData);
			else if (c->argType(1) == VTypes::IntegerData) i = obj.m->atomArray()[c->argi(1)-1];
			Pattern* p = obj.m->pattern(i);
			int score = ffa->neta()->matchAtom(i,p->ringList(),obj.m);
			if (score > 0) Messenger::print("Atom %i matched type %i (%s) with score %i.\n", i->id()+1, ffa->typeId(), ffa->name(), score);
			else Messenger::print("Atom %i did not match type %i (%s).\n", i->id()+1, ffa->typeId(), ffa->name());
			rv.set(score);
		}
		else return FALSE;
	}
	return TRUE;
}

// Set units used in the forcefield
bool Commands::function_Units(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return FALSE;
	Prefs::EnergyUnit newunit = Prefs::energyUnit(c->argc(0));
	if (newunit == Prefs::nEnergyUnits) return FALSE;
	obj.ff->setEnergyUnit(newunit);
	Messenger::print("Forcefield energy unit set to %s\n", Prefs::energyUnit(newunit));
	rv.reset();
	return TRUE;
}


