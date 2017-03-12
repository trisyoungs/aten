/*
	*** Variable List
	*** src/parser/variablelist.cpp
	Copyright T. Youngs 2007-2017

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
#include "parser/dialog.h"
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
#include "parser/matrix.h"
#include "parser/measurement.h"
#include "parser/model.h"
#include "parser/pattern.h"
#include "parser/patternbound.h"
#include "parser/vector.h"
#include "parser/vibration.h"
#include "parser/widget.h"
#include "parser/zmatrix.h"
#include "parser/zmatrixelement.h"

ATEN_USING_NAMESPACE

// Constructor
VariableList::VariableList()
{
}

// Pass a newly-created variable / constant to the list for it to take ownership of
void VariableList::take(Variable* v, bool forcevariable)
{
	// Check the readonly status to determine where we put it
	if (v->readOnly() && (!forcevariable)) constants_.own(v);
	else variables_.own(v);
}

// Retrieve a named variable from the list
Variable* VariableList::find(QString name) const
{
	Variable* var;
	for (TreeNode* node = variables_.first(); node != NULL; node = node->next)
	{
		var = (Variable*) node;
		if (name == var->name()) return var;
	}
	return NULL;
}

// Create a new variable in the list
Variable* VariableList::makeVariable(VTypes::DataType type, QString name, TreeNode* initialValue)
{
	Variable* v = NULL;
	switch (type)
	{
		case (VTypes::NoData):
			printf("No data type passed to VariableList::makeVariable().\n");
			break;
		case (VTypes::IntegerData):
			v = (Variable*) new IntegerVariable(0, false);
			break;
		case (VTypes::DoubleData):
			v = (Variable*) new DoubleVariable(0.0, false);
			break;
		case (VTypes::StringData):
			v = (Variable*) new StringVariable("", false);
			break;
		case (VTypes::VectorData):
			v = (Variable*) new VectorVariable(false);
			break;
		case (VTypes::MatrixData):
			v = (Variable*) new MatrixVariable(false);
			break;
		case (VTypes::AtenData):
			v = (Variable*) new AtenVariable();
			break;
		case (VTypes::AtomData):
			v = (Variable*) new AtomVariable(NULL, false);
			break;
		case (VTypes::BasisPrimitiveData):
			v = (Variable*) new BasisPrimitiveVariable(NULL, false);
			break;
		case (VTypes::BasisShellData):
			v = (Variable*) new BasisShellVariable(NULL, false);
			break;
		case (VTypes::BondData):
			v = (Variable*) new BondVariable(NULL, false);
			break;
		case (VTypes::CellData):
			v = (Variable*) new CellVariable(NULL, false);
			break;
		case (VTypes::ColourScaleData):
			v = (Variable*) new ColourScaleVariable(NULL, false);
			break;
		case (VTypes::ColourScalePointData):
			v = (Variable*) new ColourScalePointVariable(NULL, false);
			break;
		case (VTypes::DialogData):
			v = (Variable*) new DialogVariable(NULL, false);
			break;
		case (VTypes::EigenvectorData):
			v = (Variable*) new EigenvectorVariable(NULL, false);
			break;
		case (VTypes::ElementData):
			v = (Variable*) new ElementVariable();
			break;
		case (VTypes::ForcefieldData):
			v = (Variable*) new ForcefieldVariable(NULL, false);
			break;
		case (VTypes::ForcefieldAtomData):
			v = (Variable*) new ForcefieldAtomVariable(NULL, false);
			break;
		case (VTypes::ForcefieldBoundData):
			v = (Variable*) new ForcefieldBoundVariable(NULL, false);
			break;
		case (VTypes::GlyphData):
			v = (Variable*) new GlyphVariable(NULL, false);
			break;
		case (VTypes::GlyphDataData):
			v = (Variable*) new GlyphDataVariable(NULL, false);
			break;
		case (VTypes::GridData):
			v = (Variable*) new GridVariable(NULL, false);
			break;
		case (VTypes::MeasurementData):
			v = (Variable*) new MeasurementVariable(NULL, false);
			break;
		case (VTypes::ModelData):
			v = (Variable*) new ModelVariable(NULL, false);
			break;
		case (VTypes::MonteCarloData):
			v = (Variable*) new AtenVariable();
			break;
		case (VTypes::PatternData):
			v = (Variable*) new PatternVariable(NULL, false);
			break;
		case (VTypes::PatternBoundData):
			v = (Variable*) new PatternBoundVariable(NULL, false);
			break;
		case (VTypes::VibrationData):
			v = (Variable*) new VibrationVariable(NULL, false);
			break;
		case (VTypes::WidgetData):
			v = (Variable*) new WidgetVariable(NULL, false);
			break;
		case (VTypes::ZMatrixData):
			v = (Variable*) new ZMatrixVariable(NULL, false);
			break;
		case (VTypes::ZMatrixElementData):
			v = (Variable*) new ZMatrixElementVariable(NULL, false);
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
Variable* VariableList::makeArray(VTypes::DataType type, QString name, TreeNode* sizeExpr, TreeNode* initialValue)
{
	Variable* var = NULL;
	switch (type)
	{
		case (VTypes::AtomData):
			var = new AtomArrayVariable(sizeExpr);
			break;
		case (VTypes::BasisPrimitiveData):
			var = new BasisPrimitiveArrayVariable(sizeExpr);
			break;
		case (VTypes::BasisShellData):
			var = new BasisShellArrayVariable(sizeExpr);
			break;
		case (VTypes::BondData):
			var = new BondArrayVariable(sizeExpr);
			break;
		case (VTypes::CellData):
			var = new CellArrayVariable(sizeExpr);
			break;
		case (VTypes::ColourScaleData):
			var = new ColourScaleArrayVariable(sizeExpr);
			break;
		case (VTypes::ColourScalePointData):
			var = new ColourScalePointArrayVariable(sizeExpr);
			break;
		case (VTypes::DialogData):
			var = new DialogArrayVariable(sizeExpr);
			break;
		case (VTypes::EigenvectorData):
			var = new EigenvectorArrayVariable(sizeExpr);
			break;
		case (VTypes::IntegerData):
			var = new IntegerArrayVariable(sizeExpr);
			break;
		case (VTypes::DoubleData):
			var = new DoubleArrayVariable(sizeExpr);
			break;
		case (VTypes::ForcefieldData):
			var = new ForcefieldArrayVariable(sizeExpr);
			break;
		case (VTypes::ForcefieldAtomData):
			var = new ForcefieldAtomArrayVariable(sizeExpr);
			break;
		case (VTypes::ForcefieldBoundData):
			var = new ForcefieldBoundArrayVariable(sizeExpr);
			break;
		case (VTypes::GlyphData):
			var = new GlyphArrayVariable(sizeExpr);
			break;
		case (VTypes::GlyphDataData):
			var = new GlyphDataArrayVariable(sizeExpr);
			break;
		case (VTypes::GridData):
			var = new GridArrayVariable(sizeExpr);
			break;
		case (VTypes::MeasurementData):
			var = new MeasurementArrayVariable(sizeExpr);
			break;
		case (VTypes::ModelData):
			var = new ModelArrayVariable(sizeExpr);
			break;
		case (VTypes::PatternData):
			var = new PatternArrayVariable(sizeExpr);
			break;
		case (VTypes::PatternBoundData):
			var = new PatternBoundArrayVariable(sizeExpr);
			break;
		case (VTypes::StringData):
			var = new StringArrayVariable(sizeExpr);
			break;
		case (VTypes::VectorData):
			var = new VectorArrayVariable(sizeExpr);
			break;
		case (VTypes::MatrixData):
			var = new MatrixArrayVariable(sizeExpr);
			break;
		case (VTypes::VibrationData):
			var = new VibrationArrayVariable(sizeExpr);
			break;
		case (VTypes::WidgetData):
			var = new WidgetArrayVariable(sizeExpr);
			break;
		case (VTypes::ZMatrixData):
			var = new ZMatrixArrayVariable(sizeExpr);
			break;
		case (VTypes::ZMatrixElementData):
			var = new ZMatrixElementArrayVariable(sizeExpr);
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
Variable* VariableList::create(VTypes::DataType type, QString name, TreeNode* initialValue)
{
	Variable* v = makeVariable(type, name, initialValue);
	if (v != NULL) variables_.own(v);
	return v;
}

// Create variable without owning it
Variable* VariableList::createFree(VTypes::DataType type, QString name, TreeNode* initialValue)
{
	return makeVariable(type, name, initialValue);
}

// Create a new array variable in the list
Variable* VariableList::createArray(VTypes::DataType type, QString name, TreeNode* sizeexpr, TreeNode* initialValue)
{
	Variable* var = makeArray(type, name, sizeexpr, initialValue);
	if (var == NULL) return NULL;
	variables_.own(var);
	var->setName(name);
	return var;
}

// Create a new array constant in the list
Variable* VariableList::createArrayConstant(VTypes::DataType type, int size)
{
	// Create a new constant integer to store the size of the array
	Variable* sizeconst = new IntegerVariable(size, true);
	constants_.own(sizeconst);
	Variable* var = makeArray(type, "constarray", sizeconst);
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

// Return first variable in the list (as a TreeNode)
TreeNode* VariableList::variables() const
{
	return variables_.first();
}

// Return specified variable in the list (slow)
Variable* VariableList::variable(int index)
{
	if ((index < 0) || (index >= variables_.nItems())) printf("Array index %i is out of bounds for VariableList::variable()\n", index);
	else
	{
		TreeNode* node = variables_.item(index);
		if (node) return (Variable*) node;
	}
	return NULL;
}

// Initialise/reset all variables
bool VariableList::initialise()
{
	Variable* var;
	for (TreeNode* node = variables_.first(); node != NULL; node = node->next)
	{
		var = (Variable*) node;
		if (!var->initialise()) return false;
	}
	return true;
}

// Clear all variables and constants
void VariableList::clear()
{
	variables_.clear();
}

// Print list of variables and their values
void VariableList::print() const
{
	Variable* var;
	for (TreeNode* node = variables_.first(); node != NULL; node = node->next)
	{
		var = (Variable*) node;
		var->nodePrint(0,"");
	}
}
