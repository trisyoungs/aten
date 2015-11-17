/*
	*** Selection Commands
	*** src/command/select.cpp
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

#include "parser/commandnode.h"
#include "parser/usercommandnode.h"
#include "model/bundle.h"
#include "model/model.h"
#include "base/pattern.h"
#include "parser/atom.h"
#include "ff/forcefield.h"
#include "base/forcefieldatom.h"
#include "parser/character.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Deselect atom, range of atoms, or elements ('deselect <n>')
bool Commands::function_DeSelect(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Store current number of selected atoms
	int nselected = obj.rs()->nSelected();
	bool result = true;

	// Loop over arguments given to command, passing them in turn to selectAtoms
	for (int i=0; i<c->nArgs(); i++) if (!obj.rs()->selectAtoms(c->argNode(i), true))
	{
		result = false;
		break;
	}
	rv.set(nselected - obj.rs()->nSelected());

	return result;
}

// Deselect using conditional code
bool Commands::function_DeSelectCode(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	int nselected = obj.rs()->nSelected();
	
	// Construct program
	QString code;
	code.sprintf("int internalDeselectAtom(Atom i) { %s; return false; }", qPrintable(c->argc(0)));
	Program program;
	if (!program.generateFromString(code, "SelectionCode", "Selection Code"))
	{
		Messenger::print("Error: Couldn't construct selection code.");
		rv.reset();
		return false;
	}
	
	// Get global function and set up variable and UserCommandNode
	Tree* function = program.mainProgram()->findLocalFunction("internalDeselectAtom");
	if (function == NULL)
	{
		Messenger::print("Internal Error: Couldn't find generated deselection function.");
		return false;
	}
	Tree tree;
	UserCommandNode functionNode;
	functionNode.setParent(&tree);
	functionNode.setFunction(function);
	AtomVariable atomVariable;
	functionNode.addArgument(&atomVariable);
	obj.rs()->beginUndoState("Deselect atoms by for loop");
	for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next)
	{
		// Poke atom value 
		rv.set(VTypes::AtomData, i);
		atomVariable.set(rv);
		functionNode.execute(rv);
		if (rv.asBool()) obj.rs()->deselectAtom(i);
	}
	obj.rs()->endUndoState();
	rv.set(nselected - obj.rs()->nSelected());
	return true;
}

// Deselect by formatted string
bool Commands::function_DeSelectFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Store current number of selected atoms
	int nselected = obj.rs()->nSelected();
	bool result = true;
	// Write formatted string, then pass this to select()
	Format* format = c->createFormat(0,1);
	if (!format->writeToString())
	{
		Messenger::print("Failed to format string for output.");
		return false;
	}
	LineParser parser;
	parser.getArgsDelim(0, format->string());
	for (int i=0; i<parser.nArgs(); i++)
	{
		StringVariable stringvar(parser.argc(i));
		if (!obj.rs()->selectAtoms(&stringvar, true)) { result = false; break; }
	}
	rv.set(nselected - obj.rs()->nSelected());
	return result;
}

// Deselect by supplied atom type description ('deselecttype <el> <typedesc>')
bool Commands::function_DeSelectType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	if (obj.rs()->createPatterns())
	{
		// Store current number of selected atoms
		int nselected = obj.rs()->nSelected();
		obj.rs()->beginUndoState("Deselect %s by type (%s)", Elements().symbol(c->argz(0)), qPrintable(c->argc(1)));
		int result = obj.rs()->selectType(c->argz(0), c->argc(1), false, true);
		obj.rs()->endUndoState();
		if (result != -1)
		{
			rv.set(nselected - obj.rs()->nSelected());
			return true;
		}
	}
	else Messenger::print("Can't test atomtype description without a valid pattern definition!");
	rv.reset();
	return false;
}

// Expand current selection
bool Commands::function_Expand(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Expand current selection");
	int nselected = obj.rs()->nSelected();
	if (c->hasArg(0)) for (int n=0; n<c->argi(0); ++n) obj.rs()->selectionExpand();
	else obj.rs()->selectionExpand();
	obj.rs()->endUndoState();
	rv.set( obj.rs()->nSelected() - nselected );
	return true;
}

// Invert selection
bool Commands::function_Invert(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Invert selection");
	obj.rs()->selectionInvert();
	obj.rs()->endUndoState();
	rv.set( obj.rs()->nSelected() );
	return true;
}

// Select all ('selectall')
bool Commands::function_SelectAll(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Select all atoms");
	obj.rs()->selectAll();
	obj.rs()->endUndoState();
	rv.set( obj.rs()->nSelected() );
	return true;
}

// Select atom, range of atoms, or elements ('select <n>')
bool Commands::function_Select(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Store current number of selected atoms
	int nselected = obj.rs()->nSelected();
	bool result = true;
	// Loop over arguments given to command, passing them in turn to selectAtoms
	for (int i=0; i<c->nArgs(); i++) if (!obj.rs()->selectAtoms(c->argNode(i), false)) { result = false; break; }
	rv.set(obj.rs()->nSelected() - nselected);
	return result;
}

// Get selection centre of geometry ('selectioncog')
bool Commands::function_SelectionCog(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Vec3<double> v = obj.rs()->selectionCentreOfGeometry();
	rv.set(v);
	return true;
}

// Get selection centre of mass ('selectioncom')
bool Commands::function_SelectionCom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Vec3<double> v = obj.rs()->selectionCentreOfMass();
	rv.set(v);
	return true;
}

// Select using conditional code
bool Commands::function_SelectCode(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	int nselected = obj.rs()->nSelected();

	// Construct program
	QString code;
	code.sprintf("int internalSelectAtom(atom i) { %s; return false; }", qPrintable(c->argc(0)));
	Program program;
	if (!program.generateFromString(code, "SelectionCode", "Selection Code"))
	{
		Messenger::print("Error: Couldn't construct selection code.");
		rv.reset();
		return false;
	}

	// Get global function and set up variable and UserCommandNode
	Tree* function = program.mainProgram()->findLocalFunction("internalSelectAtom");
	if (function == NULL)
	{
		Messenger::print("Internal Error: Couldn't find generated selection function.");
		return false;
	}
	Tree tree;
	UserCommandNode functionNode;
	functionNode.setParent(&tree);
	functionNode.setFunction(function);
	AtomVariable atomVariable;
	functionNode.addArgument(&atomVariable);
	obj.rs()->beginUndoState("Select atoms by for loop");
	for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next)
	{
		// Poke atom value 
		rv.set(VTypes::AtomData, i);
		atomVariable.set(rv);
		functionNode.execute(rv);
		if (rv.asBool()) obj.rs()->selectAtom(i);
	}
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected() - nselected);
	return true;
}

// Select by forcefield type ('selecffttype <fftype>')
bool Commands::function_SelectFFType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Forcefield* ff = obj.rs()->forcefield();
	if (ff == NULL)
	{
		Messenger::print("No forcefield associated to model.");
		return false;
	}

	// Store current number of selected atoms
	int nselected = obj.rs()->nSelected();
	ForcefieldAtom* ffa;
	obj.rs()->beginUndoState("Select by forcefield type (%s)", qPrintable(c->argc(0)));
	for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next)
	{
		ffa = i->type();
		if (ffa != NULL)
		{
			if (ff->matchType(ffa->name(),c->argc(0)) < 10) obj.rs()->selectAtom(i);
		}
	}
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected() - nselected);

	return true;
}

// Select by formatted string
bool Commands::function_SelectFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Store current number of selected atoms
	int nselected = obj.rs()->nSelected();
	bool result = true;
	// Write formatted string, then pass this to select()
	Format* format = c->createFormat(0,1);
	if (!format->writeToString())
	{
		Messenger::print("Failed to format string for output.");
		return false;
	}
	LineParser parser;
	parser.getArgsDelim(0, format->string());
	for (int i=0; i<parser.nArgs(); i++)
	{
		StringVariable stringvar(parser.argc(i));
		if (!obj.rs()->selectAtoms(&stringvar, false)) { result = false; break; }
	}
	rv.set(obj.rs()->nSelected() - nselected);
	return result;
}

// Select atoms (or molecule COGs) inside the current unit cell
bool Commands::function_SelectInsideCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	int nselected = obj.rs()->nSelected();
	obj.rs()->beginUndoState("Select %s inside cell", c->hasArg(0) ? "molecules" : "atoms");
	obj.rs()->selectInsideCell(c->hasArg(0) ? c->argb(0) : false);
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected() - nselected);
	return true;
}

// Select atoms based on Miller plane definition
bool Commands::function_SelectMiller(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Select atoms from Miller plane (%i%i%i)", c->argi(0), c->argi(1), c->argi(2));	
	obj.rs()->selectNone();
	obj.rs()->selectMiller(c->argi(0), c->argi(1), c->argi(2), c->hasArg(3) ? c->argb(3) : false);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Select bound fragment or molecule
bool Commands::function_SelectMolecule(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	int nselected = obj.rs()->nSelected();
	Atom* i = c->argType(0) == VTypes::IntegerData ? obj.rs()->atom(c->argi(0)-1) : (Atom*) c->argp(0, VTypes::AtomData);
	obj.rs()->beginUndoState("Select bound fragment/molecule");
	obj.rs()->selectTree(i);
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected() - nselected);
	return true;
}

// Select atoms near to defined line
bool Commands::function_SelectLine(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	int nselected = obj.rs()->nSelected();
	obj.rs()->beginUndoState("Select atoms near line");
	obj.rs()->selectLine(c->arg3d(0), c->arg3d(3), c->argd(6));
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected() - nselected);
	return true;
}

// Select no atoms ('selectnone')
bool Commands::function_SelectNone(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Deselect all atoms");
	obj.rs()->selectNone();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Detect and select overlapping atoms
bool Commands::function_SelectOverlaps(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	double tol = c->hasArg(0) ? c->argd(0) : 0.2;
	obj.rs()->beginUndoState("Select overlapping atoms (within %f)", tol);
	obj.rs()->selectOverlaps(tol);
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected());
	return true;
}

// Select atoms (or molecule COGs) outside of the current unit cell
bool Commands::function_SelectOutsideCell(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	int nselected = obj.rs()->nSelected();
	obj.rs()->beginUndoState("Select %s outside cell", c->hasArg(0) ? "molecules" : "atoms");
	obj.rs()->selectOutsideCell(c->hasArg(0) ? c->argb(0) : false);
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected() - nselected);
	return true;
}

// Select all atoms in current (or named/id'd) pattern ('selectpattern [name|id]')
bool Commands::function_SelectPattern(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Pattern* p = NULL;
	if (c->hasArg(0))
	{
		if (c->argType(0) == VTypes::IntegerData) p = obj.rs()->pattern(c->argi(0)-1);
		else p = obj.rs()->findPattern(c->argc(0));
	}
	else p = obj.p;
	int nselected = obj.rs()->nSelected();
	if (p == NULL) Messenger::print("No pattern in which to select atoms.");
	else
	{
		obj.rs()->beginUndoState("Select pattern '%s'", qPrintable(p->name()));
		Atom* i = p->firstAtom();
		for (int n=0; n<p->totalAtoms(); n++)
		{
			obj.rs()->selectAtom(i);
			i = i->next;
		}
		obj.rs()->endUndoState();
	}
	rv.set(obj.rs()->nSelected() - nselected);
	return true;
}

// Select all atoms within a distance of target atom
bool Commands::function_SelectRadial(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Atom* i = c->argType(0) == VTypes::IntegerData ? obj.rs()->atom(c->argi(0)-1) : (Atom*) c->argp(0, VTypes::AtomData);
	if (i == NULL) return false;
	int nselected = obj.rs()->nSelected();
	obj.rs()->beginUndoState("Radial selection %8.4f from atom %i", c->argd(1), i->id()+1);
	obj.rs()->selectRadial(i, c->argd(1));
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected() - nselected);
	return true;
}

// Select all atoms within a distance of target atom
bool Commands::function_SelectTree(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Atom* i = c->argType(0) == VTypes::IntegerData ? obj.rs()->atom(c->argi(0)-1) : (Atom*) c->argp(0, VTypes::AtomData);
	if (i == NULL) return false;
	Bond* b = c->hasArg(1) ? (Bond*) c->argp(1, VTypes::BondData) : NULL;
	int nselected = obj.rs()->nSelected();
	obj.rs()->beginUndoState("Tree selection from atom %i", i->id()+1);
	obj.rs()->selectTree(i, false, false, b);
	obj.rs()->endUndoState();
	rv.set(obj.rs()->nSelected() - nselected);
	return true;
}

// Select by supplied atom type description ('selecttype <el> <typedesc>')
bool Commands::function_SelectType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.rs()->createPatterns())
	{
		// Store current number of selected atoms
		int nselected = obj.rs()->nSelected();
		obj.rs()->beginUndoState("Select %s by type (%s)", Elements().symbol(c->argz(0)), qPrintable(c->argc(1)));
		int result = obj.rs()->selectType(c->argz(0), c->argc(1));
		obj.rs()->endUndoState();
		if (result != -1)
		{
			rv.set(obj.rs()->nSelected() - nselected);
			return true;
		}
	}
	else Messenger::print("Can't test atomtype description without a valid pattern definition!");
	rv.reset();
	return false;
}

// Test selection by atom, range of atoms, or elements
bool Commands::function_TestSelect(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Loop over arguments given to command, passing them in turn to selectAtoms
	for (int i=0; i<c->nArgs(); i++) if (!obj.rs()->selectAtoms(c->argNode(i), true, true))
	{
		rv.set(false);
		return true;
	}
	rv.set(true);

	return true;
}
