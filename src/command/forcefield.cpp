/*
	*** Forcefield Commands
	*** src/command/forcefield.cpp
	Copyright T. Youngs 2007-2016

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
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	int n;
	// Get functional form of bond potential
	AngleFunctions::AngleFunction anglestyle = AngleFunctions::angleFunction(c->argc(0), true);
	if (anglestyle == AngleFunctions::nAngleFunctions) return false;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<4; n++)
	{
		if ((!c->argc(n).contains('*')) && (!obj.ff->findType(c->argc(n))))
			Messenger::print("\t... Warning - angle atom '%s' does not exist in the forcefield!", qPrintable(c->argc(n)));
	}
	// Create new ff_bond structure
	ForcefieldBound* ffb = obj.ff->addAngle(anglestyle);
	for (n=1; n<4; n++) ffb->setTypeName(n-1,c->argc(n));
	for (n=4; n<MAXFFPARAMDATA+4; n++) if (c->hasArg(n)) ffb->setParameter(n-4, c->argd(n));
	Messenger::print(Messenger::Verbose, "Angle %i : %s-%s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f", obj.ff->nAngles(), qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)) , qPrintable(ffb->typeName(2)), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5));
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return true;
}

// Cet current autoconversion unit
bool Commands::function_AutoConversionUnit(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check that a valid file source/destination exists.
	// ATEN2 TODO ENDOFFILTERS
// 	if (!c->parent()->isFilter())
// 	{
// 		Messenger::print("The 'autoconversionunit' command can only be used from within a Filter.");
// 		return false;
// 	}
// 	if (c->hasArg(0))
// 	{
// 		Prefs::EnergyUnit eu = Prefs::energyUnit(c->argc(0), true);
// 		if (eu == Prefs::nEnergyUnits) return false;
// 		else prefs.setAutoConversionUnit(eu);
// 	}
// 	else prefs.setAutoConversionUnit(Prefs::nEnergyUnits);
	return true;
}

// Add a new bond definition to the current forcefield
bool Commands::function_BondDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	int n;
	// Get functional form of bond potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(c->argc(0),true);
	if (bondstyle == BondFunctions::nBondFunctions) return false;
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<3; n++)
	{
		if ((!c->argc(n).contains('*')) && (!obj.ff->findType(c->argc(n))))
			Messenger::print("\t... Warning - bond atom '%s' does not exist in the forcefield!", qPrintable(c->argc(n)));
	}
	// Create new ff_bond structure
	ForcefieldBound* ffb = obj.ff->addBond(bondstyle);
	for (n=1; n<3; n++) ffb->setTypeName(n-1, qPrintable(c->argc(n)));
	for (n=3; n<MAXFFPARAMDATA+3; n++) if (c->hasArg(n)) ffb->setParameter(n-3, c->argd(n));
	Messenger::print(Messenger::Verbose, "Bond %i : %s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f", obj.ff->nBonds(), qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5)); 
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return true;
}

// Clear manual export type mapping list ('clearexportmap')
bool Commands::function_ClearExportMap(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	aten_.clearTypeExportMap();
	rv.reset();
	return true;
}

// Clear energy expression for current model ('clearexpression'}
bool Commands::function_ClearExpression(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->clearExpression();
	rv.reset();
	return true;
}

// Clear manual type mapping list ('clearmap')
bool Commands::function_ClearMap(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Elements().clearMappings();
	rv.reset();
	return true;
}

// Clear atom types from current model
bool Commands::function_ClearTypes(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->removeTyping();
	rv.reset();
	return true;
}

// Create energy expression for current model ('createexpression(bool vdwOnly, bool allowdummy, bool assigncharges)'}
bool Commands::function_CreateExpression(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (!obj.m->createPatterns()) return false;
	Choice vdwOnly, allowDummy, assignCharges;
	if (c->hasArg(0)) vdwOnly = c->argb(0) ? Choice::Yes : Choice::No;
	if (c->hasArg(1)) allowDummy = c->argb(1) ? Choice::Yes : Choice::No;
	if (c->hasArg(2)) assignCharges = c->argb(2) ? Choice::Yes : Choice::No;
	bool result = obj.m->createExpression(vdwOnly, allowDummy, assignCharges, aten_.currentForcefield(), aten_.combinationRules());
	rv.set(result);
	return true;
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
				Messenger::print("Can't convert a variable of type '%s' into a Forcefield.", VTypes::dataType(c->argType(0)));
				break;
		}
		if (ff == NULL)	return false;
		aten_.setCurrentForcefield(ff);
		rv.set(VTypes::ForcefieldData, ff);
	}
	else rv.set(VTypes::ForcefieldData, aten_.currentForcefield());
	return true;
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
				Messenger::print("Can't convert a variable of type '%s' into a Forcefield.", VTypes::dataType(c->argType(0)));
				break;
		}
		if (ff == NULL)	return false;
		aten_.removeForcefield(ff);
	}
	return true;
}

// Set energetic parameters to convert in generator data
bool Commands::function_EnergyConvert(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	for (int n=0; n<c->nArgs(); n++) obj.ff->addEnergyData(c->argc(n));
	rv.reset();
	return true;
}

// Set equivalent
bool Commands::function_Equivalent(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	ForcefieldAtom* ffa;
	LineParser parser;

	// Loop over command arguments
	for (int n=1; n<c->nArgs(); ++n)
	{
		// Split command into comma-delimited sections
		parser.getArgsDelim(0, qPrintable(c->argc(n)));
		for (int i=0; i<parser.nArgs(); ++i)
		{
			for (ffa = obj.ff->types(); ffa != NULL; ffa = ffa->next)
				if (obj.ff->matchType(ffa->name(),parser.argc(i)) < 10) ffa->setEquivalent(c->argc(0));
		}
	}

	rv.reset();
	return true;
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
			if (!parser.argc(n).contains('='))
			{
				Messenger::print("Mangled exportmap value found (i.e. it contains no '='): '%s'.", qPrintable(parser.argc(n)));
				continue;
			}

			// Split into value/argument
			QStringList items = parser.argc(n).split('=');
			aten_.addTypeExportMapping(items.at(0), items.at(1));
		}
	}
	rv.reset();
	return true;
}

// Associate current ff to current model ('ffmodel [name]')
bool Commands::function_FFModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// If an argument was supplied, select forcefield by name. Otherwise use current
	if (c->hasArg(0)) obj.m->setForcefield(aten_.findForcefield(c->argc(0)));
	else obj.m->setForcefield(obj.ff);
	rv.reset();
	return true;
}

// Set current forcefield for named pattern ('ffpattern')
bool Commands::function_FFPattern(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return false;
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
				Messenger::print("Can't convert a variable of type '%s' into a Forcefield.", VTypes::dataType(c->argType(0)));
				break;
		}
		if (p == NULL)
		{
			Messenger::print("Invalid pattern specified - current model unchanged.");
			return false;
		}
		p->setForcefield(obj.ff);
	}
	rv.reset();
	return true;
}

// Fix atom types
bool Commands::function_FixType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	ForcefieldAtom* ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		if (c->argType(0) == VTypes::IntegerData) Messenger::print("Forcefield type ID %i not defined in forcefield '%s'.", c->argi(0), qPrintable(obj.ff->name()));
		else if (c->argType(0) == VTypes::StringData) Messenger::print("Forcefield type '%s' not defined in forcefield '%s'.", qPrintable(c->argc(0)), qPrintable(obj.ff->name()));
		rv.reset();
		return false;
	}
	if (c->hasArg(1))
	{
		Atom* i = c->argType(1) == VTypes::IntegerData ? obj.m->atom(c->argi(1)-1) : (Atom*) c->argp(1, VTypes::AtomData);
		if (i == NULL) return false;
		obj.m->setAtomType(i, ffa, true);
		Messenger::print("Atom type for atom id %i fixed to %i (%s/%s).", i->id()+1, c->argi(0), qPrintable(ffa->name()), qPrintable(ffa->equivalent()));
	}
	else for (RefListItem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next)
	{
		obj.m->setAtomType(ri->item, ffa, true);
		Messenger::print("Atom type for atom id %i fixed to %i (%s/%s).", ri->item->id()+1, c->argi(0), qPrintable(ffa->name()), qPrintable(ffa->equivalent()));
	}
	obj.m->logChange(Log::Structure);
	rv.reset();
	return true;
}

// Free atom types
bool Commands::function_FreeType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1))
	{
		Atom* i = c->argType(1) == VTypes::IntegerData ? obj.m->atom(c->argi(1)-1) : (Atom*) c->argp(1, VTypes::AtomData);
		if (i == NULL) return false;
		obj.m->setAtomType(i, i->type(), false);
	}
	else for (RefListItem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next) obj.m->setAtomType(ri->item, ri->item->type(), true);
	obj.m->logChange(Log::Structure);
	rv.reset();
	return true;
}

// Generate (or return existing) bound parameters for specified angle interaction
bool Commands::function_GenerateAngle(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer+Bundle::ModelPointer)) return false;
	// Find named atoms in forcefield
	Atom* atoms[3];
	for (int i=0; i<3; ++i) atoms[i] = (c->argType(i) == VTypes::AtomData ? (Atom*) c->argp(i, VTypes::AtomData) : obj.rs()->atom(c->argi(i)-1));
	// Check atom and associated type pointers
	for (int i=0; i<3; ++i)
	{
		if (atoms[i] == NULL)
		{
			Messenger::print("Atom %i given to 'generateangle' is NULL.", i);
			return false;
		}
		if (atoms[i]->type() == NULL)
		{
			Messenger::print("Atom %i given to 'generateangle' has no forcefield atom assigned.", i);
			return false;
		}
	}
	// Does a suitable definition already exist?
	ForcefieldBound* ffb = obj.ff->findAngle(atoms[0]->type(), atoms[1]->type(), atoms[2]->type());
	if (ffb == NULL) ffb = obj.ff->generateAngle(atoms[0], atoms[1], atoms[2]);
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return true;
}

// Generate (or return existing) bound parameters for specified bond interaction
bool Commands::function_GenerateBond(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer+Bundle::ModelPointer)) return false;
	// Find named atoms in forcefield
	Atom* atoms[2];
	for (int i=0; i<2; ++i) atoms[i] = (c->argType(i) == VTypes::AtomData ? (Atom*) c->argp(i, VTypes::AtomData) : obj.rs()->atom(c->argi(i)-1));
	// Check atom and associated type pointers
	for (int i=0; i<2; ++i)
	{
		if (atoms[i] == NULL)
		{
			Messenger::print("Atom %i given to 'generatebond' is NULL.", i);
			return false;
		}
		if (atoms[i]->type() == NULL)
		{
			Messenger::print("Atom %i given to 'generatebond' has no forcefield atom assigned.", i);
			return false;
		}
	}
	// Does a suitable definition already exist?
	ForcefieldBound* ffb = obj.ff->findBond(atoms[0]->type(), atoms[1]->type());
	if (ffb == NULL) ffb = obj.ff->generateBond(atoms[0], atoms[1]);
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return true;
}

// Generate (or return existing) bound parameters for specified torsion interaction
bool Commands::function_GenerateTorsion(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer+Bundle::ModelPointer)) return false;
	// Find named atoms in forcefield
	Atom* atoms[4];
	for (int i=0; i<4; ++i) atoms[i] = (c->argType(i) == VTypes::AtomData ? (Atom*) c->argp(i, VTypes::AtomData) : obj.rs()->atom(c->argi(i)-1));
	// Check atom and associated type pointers
	for (int i=0; i<4; ++i)
	{
		if (atoms[i] == NULL)
		{
			Messenger::print("Atom %i given to 'generatetorsion' is NULL.", i);
			return false;
		}
		if (atoms[i]->type() == NULL)
		{
			Messenger::print("Atom %i given to 'generatetorsion' has no forcefield atom assigned.", i);
			return false;
		}
	}
	// Does a suitable definition already exist?
	ForcefieldBound* ffb = obj.ff->findTorsion(atoms[0]->type(), atoms[1]->type(), atoms[2]->type(), atoms[3]->type());
	if (ffb == NULL) ffb = obj.ff->generateTorsion(atoms[0], atoms[1], atoms[2], atoms[3]);
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return true;
}

// Generate (or return existing) vdw parameters for specified atom
bool Commands::function_GenerateVdw(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer+Bundle::ModelPointer)) return false;
	// Find named atoms in forcefield
	Atom* i = (c->argType(0) == VTypes::AtomData ? (Atom*) c->argp(0, VTypes::AtomData) : obj.rs()->atom(c->argi(0)-1));
	// Check atom and associated type pointers
	if (i == NULL)
	{
		Messenger::print("Atom given to 'generatevdw' is NULL.");
		return false;
	}
	ForcefieldAtom* ffa = i->type();
	if (ffa == NULL)
	{
		Messenger::print("Atom given to 'generatevdw' has no forcefield atom assigned.");
		return false;
	}
	if (ffa->vdwForm() == VdwFunctions::None) obj.ff->generateVdw(i);
	rv.set(VTypes::ForcefieldAtomData, ffa->vdwForm() == VdwFunctions::None ? NULL : ffa);
	return true;
}

// Get combination rule in use for VDW parameter
bool Commands::function_GetCombinationRule(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.reset();
	// First, get functional form
	VdwFunctions::VdwFunction form = VdwFunctions::vdwFunction(c->argc(0), true);
	if (form == VdwFunctions::nVdwFunctions) return false;
	// Next, get functional form parameter
	int param = VdwFunctions::vdwParameter(form, c->argc(1), true);
	if (param == VdwFunctions::VdwFunctions[form].nParameters) return false;
	// Everything OK, so return combination rule in use
	rv.set(CombinationRules::combinationRule( VdwFunctions::VdwFunctions[form].combinationRules[param] ));
	return true;
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
			Messenger::print("Can't convert a variable of type '%s' into a Forcefield.", VTypes::dataType(c->argType(0)));
			break;
	}
	if (ff == NULL)	return false;
	rv.set(VTypes::ForcefieldData, ff);
	return true;
}

// Add a new intermolecular definition to the current forcefield
bool Commands::function_InterDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	// Get functional form of vdw
	VdwFunctions::VdwFunction vdwstyle = VdwFunctions::vdwFunction(c->argc(0), true);
	if (vdwstyle == VdwFunctions::nVdwFunctions) return false;
	// Find typeId referenced by command
	ForcefieldAtom* ffa = obj.ff->findType(c->argi(1));
	if (ffa == NULL)
	{
		Messenger::print("TypeId %i has not been defined - can't define VDW data.",c->argi(1));
		return false;
	}
	ffa->setVdwForm(vdwstyle);
	ffa->setCharge(c->argd(2));
	for (int i=3; i<MAXFFPARAMDATA+3; i++) if (c->hasArg(i)) ffa->setParameter(i-3, c->argd(i));
	Messenger::print(Messenger::Verbose, "VDW Data %i : %s %8.4f %8.4f %8.4f %8.4f", ffa->typeId(), qPrintable(ffa->name()), ffa->parameter(0), ffa->parameter(1), ffa->parameter(2), ffa->charge());
	rv.reset();
	return true;
}

// Load forcefield ('loadff <filename> [nickname]')
bool Commands::function_LoadFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Forcefield* ff = aten_.loadForcefield(c->argc(0));
	if (ff == NULL) return false;
	else
	{
		if (c->hasArg(1)) ff->setName(c->argc(1));
		Messenger::print("Forcefield '%s' loaded, name '%s'", qPrintable(c->argc(0)), qPrintable(ff->name()));
	}
	rv.set(VTypes::ForcefieldData, ff);
	return true;
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
			if (!parser.argc(n).contains('='))
			{
				Messenger::print("Mangled map value found (i.e. it contains no '='): '%s'.", qPrintable(parser.argc(n)));
				continue;
			}

			// Split into value/argument
			QStringList items = parser.argc(n).split('=');
			el = Elements().z(items.at(1));
			if (el == 0) Messenger::print("Unrecognised element '%s' in type map.", qPrintable(items.at(1)));
			else Elements().addMapping(el, items.at(0));
		}
	}
	rv.reset();
	return true;
}

// Create new, empty forcefield
bool Commands::function_NewFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	obj.ff = aten_.addForcefield(c->argc(0));
	rv.set(VTypes::ForcefieldData, obj.ff);
	return true;
}

// Print expression setup
bool Commands::function_PrintSetup(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Messenger::print("Current Energy Setup:");
	Messenger::print("Intramolecular Terms : %s", (prefs.calculateIntra() ? "On" : "Off"));
	Messenger::print("       van der Waals : %s", (prefs.calculateVdw() ? "On" : "Off"));
	Messenger::print("      Electrostatics : %s", Electrostatics::elecMethod(prefs.electrostaticsMethod()));
	Messenger::print("             Cutoffs : %13.6e (VDW)  %13.6e (elec)", prefs.vdwCutoff(), prefs.elecCutoff());
	rv.reset();
	return true;
}

// Print type specified
bool Commands::function_PrintType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	// Does the identified type exist in the forcefield
	ForcefieldAtom* ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		Messenger::print("Error: Type id %i is not defined in forcefield '%s'.", c->argi(0), qPrintable(obj.ff->name()));
		return false;
	}
	Messenger::print("Internal NETA description for type '%i' (%s, equivalent = %s)", ffa->typeId(), qPrintable(ffa->name()), qPrintable(ffa->equivalent()));
	ffa->neta()->print();
	rv.reset();
	return true;
}

// Recreate energy expression for current model ('recreateexpression(bool vdwOnly, bool allowdummy, bool assigncharges)'}
bool Commands::function_RecreateExpression(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->clearExpression();
	if (!obj.m->createPatterns()) return false;
	Choice vdwOnly, allowDummy, assignCharges;
	if (c->hasArg(0)) vdwOnly = c->argb(0) ? Choice::Yes : Choice::No;
	if (c->hasArg(1)) allowDummy = c->argb(1) ? Choice::Yes : Choice::No;
	if (c->hasArg(2)) assignCharges = c->argb(2) ? Choice::Yes : Choice::No;
	if (!obj.m->createExpression(vdwOnly, allowDummy, assignCharges, aten_.currentForcefield(), aten_.combinationRules())) return false;
	rv.reset();
	return true;
}

// Save expression ('saveexpression <format> <file>')
bool Commands::function_SaveExpression(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Parse the first option so we can get the filter nickname and any filter options
	LineParser parser;
	parser.getArgsDelim(Parser::UseQuotes, c->argc(0));
	
	// First part of argument is nickname
	FilePluginInterface* plugin = aten_.pluginStore().findFilePluginByNickname(PluginTypes::ExpressionFilePlugin, PluginTypes::ExportPlugin, parser.argc(0));

	// Check that a suitable format was found
	if (plugin == NULL)
	{
		// Print list of valid plugin nicknames
		aten_.pluginStore().showFilePluginNicknames(PluginTypes::ExpressionFilePlugin, PluginTypes::ExportPlugin);
		Messenger::print("Not saved.");
		return false;
	}

	// Loop over remaining arguments which are widget/global variable assignments
	KVMap pluginOptions;
	for (int n = 1; n < parser.nArgs(); ++n) pluginOptions.add(parser.argc(n));

	bool result = aten_.exportExpression(obj.m, c->argc(1), plugin, FilePluginStandardImportOptions(), pluginOptions);
	if (result) Messenger::print("Expression for model '%s' saved to file '%s' (%s)", qPrintable(obj.rs()->name()), qPrintable(c->argc(1)), qPrintable(plugin->name()));
	else Messenger::print("Failed to save expression for model '%s'.", qPrintable(obj.rs()->name()));
	return result;
}

// Set combination rule in use for VDW parameter
bool Commands::function_SetCombinationRule(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.reset();
	
	// First, get functional form
	VdwFunctions::VdwFunction form = VdwFunctions::vdwFunction(c->argc(0), true);
	if (form == VdwFunctions::nVdwFunctions) return false;
	
	// Next, get functional form parameter
	int param = VdwFunctions::vdwParameter(form, c->argc(1), true);
	if (param == VdwFunctions::VdwFunctions[form].nParameters) return false;
	
	// Finally, search combination rule
	CombinationRules::CombinationRule cr = CombinationRules::combinationRule(c->argc(2), true);
	if (cr == CombinationRules::nCombinationRules) return false;
	
	// Everything OK, so set data
	VdwFunctions::VdwFunctions[form].combinationRules[param] = cr;
	return true;
}

// Add a new torsion definition to the current forcefield
bool Commands::function_TorsionDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	int n;
	
	// Get functional form of bond potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(c->argc(0), true);
	if (torsionstyle == TorsionFunctions::nTorsionFunctions) return false;
	
	// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
	// If not, then check to see that it references an atomname in the atomtypes list
	for (n=1; n<4; n++)
	{
		if ((!c->argc(n).contains('*')) && (!obj.ff->findType(c->argc(n))))
			Messenger::print("\t... Warning - torsion atom '%s' does not exist in the forcefield!", qPrintable(c->argc(n)));
	}
	
	// Create new ff_bond structure
	ForcefieldBound* ffb = obj.ff->addTorsion(torsionstyle);
	for (n=1; n<5; n++) ffb->setTypeName(n-1,c->argc(n));
	for (n=5; n<MAXFFPARAMDATA+3; n++) if (c->hasArg(n)) ffb->setParameter(n-5, c->argd(n));
	Messenger::print(Messenger::Verbose, "TORSION %i : %s-%s-%s-%s  %8.4f %8.4f %8.4f %8.4f, escale=%8.4f vscale=%8.4f", obj.ff->nTorsions(), qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), qPrintable(ffb->typeName(2)), qPrintable(ffb->typeName(3)), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3), ffb->parameter(4), ffb->parameter(5));
	rv.set(VTypes::ForcefieldBoundData, ffb);
	return true;
}

// Add a new type definition to the current forcefield
bool Commands::function_TypeDef(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	// Search for this ID to make sure it hasn't already been used
	int newffid = c->argi(0);
	ForcefieldAtom* idsearch = obj.ff->findType(newffid);
	if (idsearch != NULL)
	{
		Messenger::print("Duplicate forcefield type ID '%i' - already used by type '%s'.", newffid, qPrintable(idsearch->name()));
		return false;
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
	return true;
}

// Perform typing on current model
bool Commands::function_TypeModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	rv.set(obj.m->typeAll(aten_.currentForcefield()));
	return true;
}

// Test specified type ID of current forcefield
bool Commands::function_TypeTest(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer+Bundle::ForcefieldPointer)) return false;
	rv.reset();
	// Find the specified type...
	ForcefieldAtom* ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		Messenger::print("Type ID %i does not exist in the forcefield '%s'.",c->argi(0), qPrintable(obj.ff->name()));
		return false;
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
			if (score > 0) Messenger::print("Atom %i matched type %i (%s) with score %i.", i->id()+1, ffa->typeId(), qPrintable(ffa->name()), score);
			else Messenger::print("Atom %i did not match type %i (%s).", i->id()+1, ffa->typeId(), qPrintable(ffa->name()));
			rv.set(score);
		}
		else return false;
	}
	return true;
}

// Set units used in the forcefield
bool Commands::function_Units(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ForcefieldPointer)) return false;
	Prefs::EnergyUnit newunit = Prefs::energyUnit(c->argc(0));
	if (newunit == Prefs::nEnergyUnits) return false;
	obj.ff->setEnergyUnit(newunit);
	Messenger::print("Forcefield energy unit set to %s", Prefs::energyUnit(newunit));
	rv.reset();
	return true;
}


