/*
	*** 'New' Command Node
	*** src/parser/newnode.cpp
	Copyright T. Youngs 2007-2018

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

#include "parser/newnode.h"
#include "base/atom.h"
#include "base/bond.h"
#include "base/cell.h"
#include "base/eigenvector.h"
#include "base/glyph.h"
#include "base/vibration.h"
#include "base/basisshell.h"
#include "base/colourscale.h"
#include "base/forcefieldatom.h"
#include "base/forcefieldbound.h"
#include "base/grid.h"
#include "base/zmatrix.h"
#include "ff/forcefield.h"

ATEN_USING_NAMESPACE

// Constructor
NewNode::NewNode(VTypes::DataType type) : TreeNode()
{
	// Private variables
	nodeType_ = TreeNode::NewNode;
	type_ = type;
	returnType_ = type_;
	object_ = NULL;
}

// Destructor
NewNode::~NewNode()
{
	// Destroy object (if it was created)
	if (object_) switch (type_)
	{
		case (VTypes::AtomData):
			delete (Atom*) object_;
			break;
		case (VTypes::BasisPrimitiveData):
			delete (BasisPrimitive*) object_;
			break;
		case (VTypes::BasisShellData):
			delete (BasisShell*) object_;
			break;
		case (VTypes::BondData):
			delete (Bond*) object_;
			break;
		case (VTypes::CellData):
			delete (UnitCell*) object_;
			break;
		case (VTypes::ColourScaleData):
			delete (ColourScale*) object_;
			break;
		case (VTypes::ColourScalePointData):
			delete (ColourScalePoint*) object_;
			break;
		case (VTypes::EigenvectorData):
			delete (Eigenvector*) object_;
			break;
		case (VTypes::ForcefieldData):
			delete (Forcefield*) object_;
			break;
		case (VTypes::ForcefieldAtomData):
			delete (ForcefieldAtom*) object_;
			break;
		case (VTypes::ForcefieldBoundData):
			delete (ForcefieldBound*) object_;
			break;
		case (VTypes::GlyphData):
			delete (Glyph*) object_;
			break;
		case (VTypes::GridData):
			delete (Grid*) object_;
			break;
		case (VTypes::VibrationData):
			delete (Vibration*) object_;
			break;
		case (VTypes::ZMatrixData):
			delete (ZMatrix*) object_;
			break;
		default:
			Messenger::print("Internal Error: NewNode doesn't know how to delete a variable of type %s.", VTypes::dataType(type_));
			break;
	}
}

// Execute command
bool NewNode::execute(ReturnValue& rv)
{
	// Create a new variable of the desired type...
	switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal Error: No data type set in NewNode::execute().");
			rv.reset();
			return false;
			break;
		case (VTypes::AtomData):
			object_ = new Atom;
			break;
		case (VTypes::BasisPrimitiveData):
			object_ = new BasisPrimitive;
			break;
		case (VTypes::BasisShellData):
			object_ = new BasisShell;
			break;
		case (VTypes::BondData):
			object_ = new Bond;
			break;
		case (VTypes::CellData):
			object_ = new UnitCell;
			break;
		case (VTypes::ColourScaleData):
			object_ = new ColourScale;
			break;
		case (VTypes::ColourScalePointData):
			object_ = new ColourScalePoint;
			break;
		case (VTypes::EigenvectorData):
			object_ = new Eigenvector;
			break;
		case (VTypes::ForcefieldData):
			object_ = new Forcefield;
			break;
		case (VTypes::ForcefieldAtomData):
			object_ = new ForcefieldAtom;
			break;
		case (VTypes::ForcefieldBoundData):
			object_ = new ForcefieldBound;
			break;
		case (VTypes::GlyphData):
			object_ = new Glyph;
			break;
		case (VTypes::GridData):
			object_ = new Grid;
			break;
		case (VTypes::VibrationData):
			object_ = new Vibration;
			break;
		case (VTypes::ZMatrixData):
			object_ = new ZMatrix;
			break;
		default:
			Messenger::print("Internal Error: NewNode doesn't know how to create a variable of type %s.", VTypes::dataType(type_));
			rv.reset();
			return false;
			break;
	}
	
	// Set the value...
	rv.set(type_, object_);
	
	return true;
}

// Set from returnvalue node
bool NewNode::set(ReturnValue& rv)
{
	printf("Internal Error: Trying to 'set' a NewNode.\n");
	return false;
}

// Initialise node
bool NewNode::initialise()
{
	printf("Internal Error: A NewNode cannot be initialised.\n");
	return false;
}

// Print node contents
void NewNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	printf("[NN]%s ('New' Node) (type = %s)\n", qPrintable(tab), VTypes::dataType(type_));
}
