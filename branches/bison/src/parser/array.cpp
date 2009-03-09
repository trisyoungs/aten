/*
	*** Array Variable
	*** src/parser/array.cpp
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

#include "parser/array.h"
#include "parser/atom.h"
#include "parser/bond.h"
#include "parser/cell.h"
#include "parser/character.h"
#include "parser/integer.h"
#include "parser/forcefield.h"
#include "parser/forcefieldatom.h"
#include "parser/forcefieldbound.h"
#include "parser/model.h"
#include "parser/pattern.h"
#include "parser/real.h"
#include "parser/vector.h"
#include "base/constants.h"
#include <string.h>

// Constructor
ArrayVariable::ArrayVariable(NuVTypes::DataType type, TreeNode *sizeexpr) : arraySizeExpression_(sizeexpr)
{
	// Private variables
	returnType_ = type;
	readOnly_ = FALSE;
	arrayData_ = NULL;
	arraySize_ = 0;
}

// Destructor
ArrayVariable::~ArrayVariable()
{
	if (arrayData_ != NULL) delete[] arrayData_;
}

/*
// Set / Get
*/

// Set from returnvalue node
bool ArrayVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case an array?) cannot be assigned to.\n");
		return FALSE;
	}
	if (arrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) if (!arrayData_[i].set(rv)) return FALSE;
	return TRUE;
}

// Reset variable
void ArrayVariable::reset()
{
	if (arrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) arrayData_[i].reset();
}


// Return value of node
bool ArrayVariable::execute(NuReturnValue &rv)
{
	msg.print("A whole array ('%s') cannot be passed as value.\n", name_.get());
	return FALSE;
}

// Print node contents
void ArrayVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	printf("[V]%s (variable array, name=%s, current size=%i)\n", tab, name_.get(), arraySize_);
	delete[] tab;
}

// Initialise variable
bool ArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from NuVariable
	// If the array is already allocated, free it.
	if (arrayData_ != NULL) delete[] arrayData_;
	// Get size of array to create
	NuReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		msg.print("Failed to find array size for '%s'.\n", name_.get());
		return FALSE;
	}
	// Create new array
	int n = newsize.asInteger();
	if (n > 0)
	{
		switch (returnType_)
		{
			case (NuVTypes::IntegerData):
				arrayData_ = new NuIntegerVariable[n];
				break;
			case (NuVTypes::RealData):
				arrayData_ = new NuRealVariable[n];
				break;
			case (NuVTypes::CharacterData):
				arrayData_ = new NuCharacterVariable[n];
				break;
			case (NuVTypes::VectorData):
				arrayData_ = new NuVectorVariable[n];
				break;
			case (NuVTypes::AtomData):
				arrayData_ = new AtomVariable[n];
				break;
			case (NuVTypes::BondData):
				arrayData_ = new BondVariable[n];
				break;
			case (NuVTypes::CellData):
				arrayData_ = new CellVariable[n];
				break;
			case (NuVTypes::ForcefieldData):
				arrayData_ = new ForcefieldVariable[n];
				break;
			case (NuVTypes::ForcefieldAtomData):
				arrayData_ = new ForcefieldAtomVariable[n];
				break;
			case (NuVTypes::ForcefieldBoundData):
				arrayData_ = new ForcefieldBoundVariable[n];
				break;
// 			case (NuVTypes::GridData):
// 				arrayData_ = new NuGridVariable[n];
// 				break;
			case (NuVTypes::ModelData):
				arrayData_ = new ModelVariable[n];
				break;
			case (NuVTypes::PatternData):
				arrayData_ = new PatternVariable[n];
				break;
			default:
				printf("Internal Error: Array variable doesn't know how to create an array of type '%s'.\n", NuVTypes::dataType(returnType_));
				return FALSE;
				break;
		}
	}
	if (initialValue_ == NULL) reset();
	else
	{
		NuReturnValue rv;
		if (initialValue_->execute(rv))
		{
			if (set(rv)) return TRUE;
			else
			{
				msg.print("Error: Variable %s is of type '%s', and cannot be initialised from a value of type '%s'.\n", name_.get(), NuVTypes::dataType(returnType_), NuVTypes::dataType(rv.type()));
				return FALSE;
			}
		}
		return FALSE;
	}
	return TRUE;
}