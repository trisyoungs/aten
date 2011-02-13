/*
	*** Variable List
	*** src/parser/variablelist.cpp
	Copyright T. Youngs 2007-2011

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

#include "parser/variablelist.h"
#include "parser/aten.h"
#include "parser/atom.h"
#include "parser/basisprimitive.h"
#include "parser/basisshell.h"
#include "parser/bond.h"
#include "parser/cell.h"
#include "parser/character.h"
#include "parser/colourscale.h"
#include "parser/colourscalepoint.h"
#include "parser/double.h"
#include "parser/eigenvector.h"
#include "parser/element.h"
#include "parser/integer.h"
#include "parser/glyph.h"
#include "parser/glyphdata.h"
#include "parser/grid.h"
#include "parser/forcefield.h"
#include "parser/forcefieldatom.h"
#include "parser/forcefieldbound.h"
#include "parser/measurement.h"
#include "parser/model.h"
#include "parser/pattern.h"
#include "parser/patternbound.h"
#include "parser/region.h"
#include "parser/vector.h"
#include "parser/vibration.h"
#include "parser/zmatrix.h"
#include "parser/zmatrixelement.h"
#include <string.h>

// Constructor
VariableList::VariableList()
{
}

// Pass a newly-created variable / constant to the list for it to take ownership of
void VariableList::take(Variable *v, bool forcevariable)
{
	// Check the readonly status to determine where we put it
	if (v->readOnly() && (!forcevariable)) constants_.own(v);
	else variables_.own(v);
}

// Retrieve a named variable from the list
Variable *VariableList::find(const char *name) const
{
	Variable *result = NULL;
	for (result = variables_.first(); result != NULL; result = (Variable*) result->next) if (strcmp(name,result->name()) == 0) break;
	return result;
}

// Create a new variable in the list
Variable *VariableList::makeVariable(VTypes::DataType type, const char *name, TreeNode *initialValue)
{
	Variable *v = NULL;
	switch (type)
	{
		case (VTypes::NoData):
			printf("No data type passed to VariableList::makeVariable().\n");
			break;
		case (VTypes::IntegerData):
			v = (Variable*) new IntegerVariable(0, FALSE);
			break;
		case (VTypes::DoubleData):
			v = (Variable*) new DoubleVariable(0.0, FALSE);
			break;
		case (VTypes::StringData):
			v = (Variable*) new StringVariable("", FALSE);
			break;
		case (VTypes::VectorData):
			v = (Variable*) new VectorVariable(FALSE);
			break;
		case (VTypes::AtenData):
			v = (Variable*) new AtenVariable();
			break;
		case (VTypes::AtomData):
			v = (Variable*) new AtomVariable(NULL, FALSE);
			break;
		case (VTypes::BasisPrimitiveData):
			v = (Variable*) new BasisPrimitiveVariable(NULL, FALSE);
			break;
		case (VTypes::BasisShellData):
			v = (Variable*) new BasisShellVariable(NULL, FALSE);
			break;
		case (VTypes::BondData):
			v = (Variable*) new BondVariable(NULL, FALSE);
			break;
		case (VTypes::CellData):
			v = (Variable*) new CellVariable(NULL, FALSE);
			break;
		case (VTypes::ColourScaleData):
			v = (Variable*) new ColourScaleVariable(NULL, FALSE);
			break;
		case (VTypes::ColourScalePointData):
			v = (Variable*) new ColourScalePointVariable(NULL, FALSE);
			break;
		case (VTypes::EigenvectorData):
			v = (Variable*) new EigenvectorVariable(NULL, FALSE);
			break;
		case (VTypes::ElementData):
			v = (Variable*) new ElementVariable();
			break;
		case (VTypes::ForcefieldData):
			v = (Variable*) new ForcefieldVariable(NULL, FALSE);
			break;
		case (VTypes::ForcefieldAtomData):
			v = (Variable*) new ForcefieldAtomVariable(NULL, FALSE);
			break;
		case (VTypes::ForcefieldBoundData):
			v = (Variable*) new ForcefieldBoundVariable(NULL, FALSE);
			break;
		case (VTypes::GlyphData):
			v = (Variable*) new GlyphVariable(NULL, FALSE);
			break;
		case (VTypes::GlyphDataData):
			v = (Variable*) new GlyphDataVariable(NULL, FALSE);
			break;
		case (VTypes::GridData):
			v = (Variable*) new GridVariable(NULL, FALSE);
			break;
		case (VTypes::MeasurementData):
			v = (Variable*) new MeasurementVariable(NULL, FALSE);
			break;
		case (VTypes::ModelData):
			v = (Variable*) new ModelVariable(NULL, FALSE);
			break;
		case (VTypes::PatternData):
			v = (Variable*) new PatternVariable(NULL, FALSE);
			break;
		case (VTypes::PatternBoundData):
			v = (Variable*) new PatternBoundVariable(NULL, FALSE);
			break;
		case (VTypes::RegionData):
			v = (Variable*) new RegionVariable(NULL, FALSE);
			break;
		case (VTypes::VibrationData):
			v = (Variable*) new VibrationVariable(NULL, FALSE);
			break;
		case (VTypes::ZMatrixData):
			v = (Variable*) new ZMatrixVariable(NULL, FALSE);
			break;
		case (VTypes::ZMatrixElementData):
			v = (Variable*) new ZMatrixElementVariable(NULL, FALSE);
			break;
		default:
			printf("Don't know how to create a variable of type %s.\n", VTypes::dataType(type));
			break;
	} 
	if (v != NULL)
	{
		v->setName(name);
		if (!v->setInitialValue(initialValue))
		{
			delete v;
			v = NULL;
		}
	}
	return v;
}

// Create a new array variable in the list
Variable *VariableList::makeArray(VTypes::DataType type, const char *name, TreeNode *sizeexpr, TreeNode *initialValue)
{
	Variable *var = NULL;
	switch (type)
	{
		case (VTypes::AtomData):
			var = new AtomArrayVariable(sizeexpr);
			break;
		case (VTypes::BasisPrimitiveData):
			var = new BasisPrimitiveArrayVariable(sizeexpr);
			break;
		case (VTypes::BasisShellData):
			var = new BasisShellArrayVariable(sizeexpr);
			break;
		case (VTypes::BondData):
			var = new BondArrayVariable(sizeexpr);
			break;
		case (VTypes::CellData):
			var = new CellArrayVariable(sizeexpr);
			break;
		case (VTypes::ColourScaleData):
			var = new ColourScaleArrayVariable(sizeexpr);
			break;
		case (VTypes::ColourScalePointData):
			var = new ColourScalePointArrayVariable(sizeexpr);
			break;
		case (VTypes::EigenvectorData):
			var = new EigenvectorArrayVariable(sizeexpr);
			break;
		case (VTypes::IntegerData):
			var = new IntegerArrayVariable(sizeexpr);
			break;
		case (VTypes::DoubleData):
			var = new DoubleArrayVariable(sizeexpr);
			break;
		case (VTypes::ForcefieldData):
			var = new ForcefieldArrayVariable(sizeexpr);
			break;
		case (VTypes::ForcefieldAtomData):
			var = new ForcefieldAtomArrayVariable(sizeexpr);
			break;
		case (VTypes::ForcefieldBoundData):
			var = new ForcefieldBoundArrayVariable(sizeexpr);
			break;
		case (VTypes::GlyphData):
			var = new GlyphArrayVariable(sizeexpr);
			break;
		case (VTypes::GlyphDataData):
			var = new GlyphDataArrayVariable(sizeexpr);
			break;
		case (VTypes::GridData):
			var = new GridArrayVariable(sizeexpr);
			break;
		case (VTypes::MeasurementData):
			var = new MeasurementArrayVariable(sizeexpr);
			break;
		case (VTypes::ModelData):
			var = new ModelArrayVariable(sizeexpr);
			break;
		case (VTypes::PatternData):
			var = new PatternArrayVariable(sizeexpr);
			break;
		case (VTypes::PatternBoundData):
			var = new PatternBoundArrayVariable(sizeexpr);
			break;
		case (VTypes::StringData):
			var = new StringArrayVariable(sizeexpr);
			break;
		case (VTypes::VectorData):
			var = new VectorArrayVariable(sizeexpr);
			break;
		case (VTypes::VibrationData):
			var = new VibrationArrayVariable(sizeexpr);
			break;
		case (VTypes::ZMatrixData):
			var = new ZMatrixArrayVariable(sizeexpr);
			break;
		case (VTypes::ZMatrixElementData):
			var = new ZMatrixElementArrayVariable(sizeexpr);
			break;
		default:
			printf("Internal Error: Don't know how to create an array of type %s.\n", VTypes::dataType(type));
			break;
	}
	if (var != NULL)
	{
		var->setName(name);
		if (!var->setInitialValue(initialValue))
		{
			delete var;
			var = NULL;
		}
	}
	return var;
}

// Create variable
Variable *VariableList::create(VTypes::DataType type, const char *name, TreeNode *initialValue)
{
	Variable *v = makeVariable(type, name, initialValue);
	if (v != NULL) variables_.own(v);
	return v;
}

// Create variable without owning it
Variable *VariableList::createFree(VTypes::DataType type, const char *name, TreeNode *initialValue)
{
	return makeVariable(type, name, initialValue);
}

// Create a new array variable in the list
Variable *VariableList::createArray(VTypes::DataType type, const char *name, TreeNode *sizeexpr, TreeNode *initialValue)
{
	Variable *var = makeArray(type, name, sizeexpr, initialValue);
	if (var == NULL) return NULL;
	variables_.own(var);
	var->setName(name);
	return var;
}

// Create a new array constant in the list
Variable *VariableList::createArrayConstant(VTypes::DataType type, int size)
{
	// Create a new constant integer to store the size of the array
	Variable *sizeconst = new IntegerVariable(size, TRUE);
	constants_.own(sizeconst);
	Variable *var = makeArray(type, "constarray", sizeconst);
	if (var == NULL) return NULL;
	var->setReadOnly();
// 	constants_.own(var);
	return var;
}

// Return the number of variables (not constants) contained in the list
int VariableList::nVariables() const
{
	return variables_.nItems();
}

// Return first variable in the list
Variable *VariableList::variables() const
{
	return variables_.first();
}

// Return specified variable in the list (slow)
Variable *VariableList::variable(int index)
{
	if ((index < 0) || (index >= variables_.nItems())) printf("Array index %i is out of bounds for VariableList::variable()\n", index);
	else
	{
		// Since the Variable class uses next/prev pointers from the base TreeNode class, the List<T> template array-access function cannot be used, since it will try to cast the TreeNode* next pointer into a Variable*, which won't work...
		Variable *result;
		int count = 0;
		for (result = variables_.first(); result != NULL; result = (Variable*) result->next) if (count++ == index) break;
		return result;
	}
	return NULL;
}

// Initialise/reset all variables
bool VariableList::initialise()
{
	for (Variable *v = variables_.first(); v != NULL; v = (Variable*) v->next) if (!v->initialise()) return FALSE;
	return TRUE;
}

// Clear all variables and constants
void VariableList::clear()
{
	variables_.clear();
}

// Print list of variables and their values
void VariableList::print() const
{
	for (Variable *v = variables_.first(); v != NULL; v = (Variable*) v->next) v->nodePrint(0,"");
}
