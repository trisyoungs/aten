/*
	*** Variable
	*** src/variables/variable.cpp
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

#include "variables/variable.h"
#include "variables/expression.h"
#include "model/model.h"
#include "classes/forcefieldatom.h"
#include "classes/grid.h"
#include "base/pattern.h"
#include "base/elements.h"
#include <string.h>

// Constructor
Variable::Variable()
{
	// Private variables
	name_.set("unnamed");
	arrayType_ = VTypes::NoArray;
	dataType_ = VTypes::NoData;
	arrayIndex_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
Variable::~Variable()
{
	// Free expression object if one was created
// 	if ((dataType_ == Variable::ExpressionVariable) && (ptrValue_ != NULL)) delete (Expression*) ptrValue_;
}

// Set name of variable
void Variable::setName(const char* s)
{
	name_.set(s);
}

// Sets the content type of the variable
void Variable::setType(VTypes::DataType dt)
{
	dataType_ = dt;
}

// Returns content type of the variable
VTypes::DataType Variable::type()
{
	return dataType_;
}

// Sets the content type of the variable
void Variable::setArrayType(VTypes::ArrayType at)
{
	arrayType_ = at;
}

// Returns content type of the variable
VTypes::ArrayType Variable::arrayType()
{
	return arrayType_;
}

// Get name of variable
const char *Variable::name()
{
	return name_.get();
}

// Set parent VariableList
void Variable::setParent(VariableList *vlist)
{
	parent_ = vlist;
}

// Set array index variable
void Variable::setArrayIndex(Variable *v)
{
	arrayIndex_ = v;
}

// Return array index variable
Variable *Variable::arrayIndex()
{
	return arrayIndex_;
}

// // Reset
// void Variable::reset()
// {
// 	switch (dataType_)
// 	{
// 		case (Variable::CharacterVariable):
// 			charValue_.set("");
// 			break;
// 		case (Variable::IntegerVariable):
// 			intValue_ = 0;
// 			break;
// 		case (Variable::FloatVariable):
// 			doubleValue_ = 0.0;
// 			break;
// 		case (Variable::AtomVariable):
// 		case (Variable::PatternVariable):
// 		case (Variable::ModelVariable):
// 		case (Variable::BondVariable):
// 		case (Variable::AngleVariable):
// 		case (Variable::TorsionVariable):
// 		case (Variable::AtomtypeVariable):
// 			ptrValue_ = NULL;
// 			break;
// 	}
// }

// // Integer increase
// void Variable::increase(int n)
// {
// 	switch (dataType_)
// 	{
// 		case (Variable::IntegerVariable):
// 			intValue_ ++;
// 			break;
// 		case (Variable::FloatVariable):
// 			doubleValue_ += 1.0;
// 			break;
// 		case (Variable::AtomVariable):
// 			ptrValue_ = ( (Atom*) ptrValue_)->next;
// 			break;
// 		case (Variable::PatternVariable):
// 			ptrValue_ = ( (Pattern*) ptrValue_)->next;
// 			break;
// 		case (Variable::ModelVariable):
// 			ptrValue_ = ( (Model*) ptrValue_)->next;
// 			break;
// 		case (Variable::BondVariable):
// 		case (Variable::AngleVariable):
// 		case (Variable::TorsionVariable):
// 			ptrValue_ = ( (PatternBound*) ptrValue_)->next;
// 			break;
// 		case (Variable::AtomtypeVariable):
// 			ptrValue_ = ( (ForcefieldAtom*) ptrValue_)->next;
// 			break;
// 		default:
// 			printf("Variable::increase <<<< Don't know how to increase variable '%s', dataType_ '%s' >>>>\n", name_.get(), Variable::variableType(dataType_));
// 			break;
// 	}
// }
// 
// // Integer Decrease
// void Variable::decrease(int n)
// {
// 	switch (dataType_)
// 	{
// 		case (Variable::IntegerVariable):
// 			intValue_ --;
// 			break;
// 		case (Variable::FloatVariable):
// 			doubleValue_ -= 1.0;
// 			break;
// 		case (Variable::AtomVariable):
// 			ptrValue_ = ( (Atom*) ptrValue_)->prev;
// 			break;
// 		case (Variable::PatternVariable):
// 			ptrValue_ = ( (Pattern*) ptrValue_)->prev;
// 			break;
// 		case (Variable::ModelVariable):
// 			ptrValue_ = ( (Model*) ptrValue_)->prev;
// 			break;
// 		case (Variable::BondVariable):
// 		case (Variable::AngleVariable):
// 		case (Variable::TorsionVariable):
// 			ptrValue_ = ( (PatternBound*) ptrValue_)->prev;
// 			break;
// 		case (Variable::AtomtypeVariable):
// 			ptrValue_ = ( (ForcefieldAtom*) ptrValue_)->prev;
// 			break;
// 		default:
// 			printf("Variable::decrease <<<< Don't know how to decrease variable '%s', dataType_ '%s' >>>>\n", name_.get(), Variable::variableType(dataType_));
// 			break;
// 	}
// }
