/*
	*** Step Node
	*** src/parser/stepnode.cpp
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

#include "parser/stepnode.h"
#include "parser/aten.h"
#include "parser/atom.h"
#include "parser/basisprimitive.h"
#include "parser/basisshell.h"
#include "parser/bond.h"
#include "parser/cell.h"
#include "parser/colourscale.h"
#include "parser/colourscalepoint.h"
#include "parser/dialog.h"
#include "parser/eigenvector.h"
#include "parser/element.h"
#include "parser/energystore.h"
#include "parser/forcefield.h"
#include "parser/forcefieldatom.h"
#include "parser/forcefieldbound.h"
#include "parser/glyph.h"
#include "parser/glyphdata.h"
#include "parser/grid.h"
#include "parser/matrix.h"
#include "parser/mc.h"
#include "parser/measurement.h"
#include "parser/model.h"
#include "parser/pattern.h"
#include "parser/patternbound.h"
#include "parser/prefs.h"
#include "parser/site.h"
#include "parser/vector.h"
#include "parser/vibration.h"
#include "parser/widget.h"
#include "parser/zmatrix.h"
#include "parser/zmatrixelement.h"

ATEN_USING_NAMESPACE

// Constructors
StepNode::StepNode(int id, VTypes::DataType prevtype, TreeNode* arrayIndex, VTypes::DataType rtntype, bool readonly, int arraysize) : TreeNode()
{
	// Private variables
	previousType_ = prevtype;
	accessor_ = id;
	arrayIndex_ = arrayIndex;
	arraySize_ = arraysize;
	readOnly_ = readonly;
	returnType_ = rtntype;
	nodeType_ = TreeNode::SteppedNode;
	functionAccessor_ = FALSE;
// 	printf("Return type of StepNode is %s\n", VTypes::dataType(returnType_));
}
StepNode::StepNode(int id, VTypes::DataType prevtype, VTypes::DataType rtntype) : TreeNode()
{
	// Private variables
	previousType_ = prevtype;
	accessor_ = id;
	arraySize_ = 0;
	arrayIndex_ = NULL;
	readOnly_ = FALSE;
	returnType_ = rtntype;
	nodeType_ = TreeNode::SteppedNode;
	functionAccessor_ = TRUE;
}

// Destructor
StepNode::~StepNode()
{
}

// Return associated array index
TreeNode* StepNode::arrayIndex()
{
	return arrayIndex_;
}

// Return accessor id
int StepNode::accessor()
{
	return accessor_;
}

// Return array size of the associated accessor
int StepNode::arraySize()
{
	return arraySize_;
}

// Execute command
bool StepNode::execute(ReturnValue& rv)
{
	Messenger::enter("StepNode::execute");
	// Check that the ReturnValue contains the type that we are expecting
	if (rv.type() != previousType_)
	{
		printf("Internal Error: StepNode was expecting a type of '%s' but was given type '%s'\n", VTypes::dataType(previousType_), VTypes::dataType(rv.type()));
		Messenger::exit("StepNode::execute");
		return FALSE;
	}
	// Retrieve a value from the relevant class
	bool result = FALSE;
	// Get array index if present
	int i = -1;
	if (arrayIndex_ != NULL)
	{
		ReturnValue arrayrv;
		if (!arrayIndex_->execute(arrayrv))
		{
			printf("Failed to retrieve array index.\n");
			return FALSE;
		}
		if ((arrayrv.type() != VTypes::IntegerData) && (arrayrv.type() != VTypes::DoubleData) && (arrayrv.type() != VTypes::ElementData))
		{
			printf("Invalid datatype used as an array index (%s).\n", arrayrv.info());
			return FALSE;
		}
		i = arrayrv.asInteger();
	}
	switch (previousType_)
	{
		case (VTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData (execute).\n");
			break;
		case (VTypes::AtenData):
			if (functionAccessor_) result = AtenVariable::performFunction(accessor_, rv, this);
			else result = AtenVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::AtomData):
			if (functionAccessor_) result = AtomVariable::performFunction(accessor_, rv, this);
			else result = AtomVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::BasisPrimitiveData):
			if (functionAccessor_) result = BasisPrimitiveVariable::performFunction(accessor_, rv, this);
			else result = BasisPrimitiveVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::BasisShellData):
			if (functionAccessor_) result = BasisShellVariable::performFunction(accessor_, rv, this);
			else result = BasisShellVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::BondData):
			if (functionAccessor_) result = BondVariable::performFunction(accessor_, rv, this);
			else result = BondVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::CellData):
			if (functionAccessor_) result = CellVariable::performFunction(accessor_, rv, this);
			else result = CellVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ColourScaleData):
			if (functionAccessor_) result = ColourScaleVariable::performFunction(accessor_, rv, this);
			else result = ColourScaleVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ColourScalePointData):
			if (functionAccessor_) result = ColourScalePointVariable::performFunction(accessor_, rv, this);
			else result = ColourScalePointVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::DialogData):
			if (functionAccessor_) result = DialogVariable::performFunction(accessor_, rv, this);
			else result = DialogVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::EigenvectorData):
			if (functionAccessor_) result = EigenvectorVariable::performFunction(accessor_, rv, this);
			else result = EigenvectorVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ElementData):
			if (functionAccessor_) result = ElementVariable::performFunction(accessor_, rv, this);
			else result = ElementVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::EnergyStoreData):
			if (functionAccessor_) result = EnergyStoreVariable::performFunction(accessor_, rv, this);
			else result = EnergyStoreVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldData):
			if (functionAccessor_) result = ForcefieldVariable::performFunction(accessor_, rv, this);
			else result = ForcefieldVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldAtomData):
			if (functionAccessor_) result = ForcefieldAtomVariable::performFunction(accessor_, rv, this);
			else result = ForcefieldAtomVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldBoundData):
			if (functionAccessor_) result = ForcefieldBoundVariable::performFunction(accessor_, rv, this);
			else result = ForcefieldBoundVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::GlyphData):
			if (functionAccessor_) result = GlyphVariable::performFunction(accessor_, rv, this);
			else result = GlyphVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::GlyphDataData):
			if (functionAccessor_) result = GlyphDataVariable::performFunction(accessor_, rv, this);
			else result = GlyphDataVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::GridData):
			if (functionAccessor_) result = GridVariable::performFunction(accessor_, rv, this);
			else result = GridVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::MatrixData):
			if (functionAccessor_) result = MatrixVariable::performFunction(accessor_, rv, this);
			else result = MatrixVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::MeasurementData):
			if (functionAccessor_) result = MeasurementVariable::performFunction(accessor_, rv, this);
			else result = MeasurementVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ModelData):
			if (functionAccessor_) result = ModelVariable::performFunction(accessor_, rv, this);
			else result = ModelVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::MonteCarloData):
			if (functionAccessor_) result = MonteCarloVariable::performFunction(accessor_, rv, this);
			else result = MonteCarloVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::PatternData):
			if (functionAccessor_) result = PatternVariable::performFunction(accessor_, rv, this);
			else result = PatternVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::PatternBoundData):
			if (functionAccessor_) result = PatternBoundVariable::performFunction(accessor_, rv, this);
			else result = PatternBoundVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::PreferencesData):
			if (functionAccessor_) result = PreferencesVariable::performFunction(accessor_, rv, this);
			else result = PreferencesVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::SiteData):
			if (functionAccessor_) result = SiteVariable::performFunction(accessor_, rv, this);
			else result = SiteVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::VectorData):
			if (functionAccessor_) result = VectorVariable::performFunction(accessor_, rv, this);
			else result = VectorVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::VibrationData):
			if (functionAccessor_) result = VibrationVariable::performFunction(accessor_, rv, this);
			else result = VibrationVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::WidgetData):
			if (functionAccessor_) result = WidgetVariable::performFunction(accessor_, rv, this);
			else result = WidgetVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ZMatrixData):
			if (functionAccessor_) result = ZMatrixVariable::performFunction(accessor_, rv, this);
			else result = ZMatrixVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ZMatrixElementData):
			if (functionAccessor_) result = ZMatrixElementVariable::performFunction(accessor_, rv, this);
			else result = ZMatrixElementVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		default:
			printf("Internal Error: StepNode doesn't recognise this type (%s)\n", VTypes::dataType(previousType_));
			break;
	}
	Messenger::exit("StepNode::execute");
	return result;
}

// Print node contents
void StepNode::nodePrint(int offset, const char* prefix)
{
	// Stepnodes print in a slightly different way, with no newlines...
	switch (previousType_)
	{
		case (VTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData (print).\n");
			break;
		case (VTypes::AtenData):
			printf("%s", AtenVariable::accessorData[accessor_].name);
			break;
		case (VTypes::AtomData):
			printf("%s", AtomVariable::accessorData[accessor_].name);
			break;
		case (VTypes::BasisPrimitiveData):
			printf("%s", BasisPrimitiveVariable::accessorData[accessor_].name);
			break;
		case (VTypes::BasisShellData):
			printf("%s", BasisShellVariable::accessorData[accessor_].name);
			break;
		case (VTypes::BondData):
			printf("%s", BondVariable::accessorData[accessor_].name);
			break;
		case (VTypes::CellData):
			printf("%s", CellVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ColourScaleData):
			printf("%s", ColourScaleVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ColourScalePointData):
			printf("%s", ColourScalePointVariable::accessorData[accessor_].name);
			break;
		case (VTypes::DialogData):
			printf("%s", DialogVariable::accessorData[accessor_].name);
			break;
		case (VTypes::EigenvectorData):
			printf("%s", EigenvectorVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ElementData):
			printf("%s", ElementVariable::accessorData[accessor_].name);
			break;
		case (VTypes::EnergyStoreData):
			printf("%s", EnergyStoreVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ForcefieldData):
			printf("%s", ForcefieldVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ForcefieldAtomData):
			printf("%s", ForcefieldAtomVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ForcefieldBoundData):
			printf("%s", ForcefieldBoundVariable::accessorData[accessor_].name);
			break;
		case (VTypes::GlyphData):
			printf("%s", GlyphVariable::accessorData[accessor_].name);
			break;
		case (VTypes::GlyphDataData):
			printf("%s", GlyphDataVariable::accessorData[accessor_].name);
			break;
		case (VTypes::GridData):
			printf("%s", GridVariable::accessorData[accessor_].name);
			break;
		case (VTypes::MatrixData):
			printf("%s", MatrixVariable::accessorData[accessor_].name);
			break;
		case (VTypes::MeasurementData):
			printf("%s", MeasurementVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ModelData):
			printf("%s", ModelVariable::accessorData[accessor_].name);
			break;
		case (VTypes::MonteCarloData):
			printf("%s", MonteCarloVariable::accessorData[accessor_].name);
			break;
		case (VTypes::PatternData):
			printf("%s", PatternVariable::accessorData[accessor_].name);
			break;
		case (VTypes::PatternBoundData):
			printf("%s", PatternBoundVariable::accessorData[accessor_].name);
			break;
		case (VTypes::PreferencesData):
			printf("%s", PreferencesVariable::accessorData[accessor_].name);
			break;
		case (VTypes::SiteData):
			printf("%s", SiteVariable::accessorData[accessor_].name);
			break;
		case (VTypes::VectorData):
			printf("%s", VectorVariable::accessorData[accessor_].name);
			break;
		case (VTypes::VibrationData):
			printf("%s", VectorVariable::accessorData[accessor_].name);
			break;
		case (VTypes::WidgetData):
			printf("%s", WidgetVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ZMatrixData):
			printf("%s", ZMatrixVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ZMatrixElementData):
			printf("%s", ZMatrixElementVariable::accessorData[accessor_].name);
			break;
		default:
			printf("Internal Error: StepNode doesn't know how to print a member from type (%s)\n", VTypes::dataType(previousType_));
			break;
	}
}

// Set from returnvalue nodes
bool StepNode::set(ReturnValue& executerv, ReturnValue& setrv)
{
	Messenger::enter("StepNode::set");
	// Check that the ReturnValue contains the type that we are expecting
	if (executerv.type() != previousType_)
	{
		printf("Internal Error: StepNode was expecting a type of '%s' but was given type '%s' (in set)\n", VTypes::dataType(previousType_), VTypes::dataType(executerv.type()));
		Messenger::exit("StepNode::set");
		return FALSE;
	}
	// Retrieve a value from the relevant class
	int i = -1;
	if (arrayIndex_ != NULL)
	{
		ReturnValue arrayrv;
		if (!arrayIndex_->execute(arrayrv))
		{
			printf("Failed to retrieve array index.\n");
			return FALSE;
		}
		if ((arrayrv.type() != VTypes::IntegerData) && (arrayrv.type() != VTypes::DoubleData) && (arrayrv.type() != VTypes::ElementData))
		{
			printf("Invalid datatype used as an array index (%s).\n", arrayrv.info());
			return FALSE;
		}
		i = arrayrv.asInteger();
	}
	bool result = FALSE;
	switch (previousType_)
	{
		case (VTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData (set).\n");
			break;
		case (VTypes::AtenData):
			result = AtenVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::AtomData):
			result = AtomVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::BasisPrimitiveData):
			result = BasisPrimitiveVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::BasisShellData):
			result = BasisShellVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::BondData):
			result = BondVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::CellData):
			result = CellVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ColourScaleData):
			result = ColourScaleVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ColourScalePointData):
			result = ColourScalePointVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::DialogData):
			result = DialogVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::EigenvectorData):
			result = EigenvectorVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ElementData):
			result = ElementVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::EnergyStoreData):
			result = EnergyStoreVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldData):
			result = ForcefieldVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldAtomData):
			result = ForcefieldAtomVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldBoundData):
			result = ForcefieldBoundVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::GlyphData):
			result = GlyphVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::GlyphDataData):
			result = GlyphDataVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::GridData):
			result = GridVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::MatrixData):
			result = MatrixVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::MeasurementData):
			result = MeasurementVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ModelData):
			result = ModelVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::MonteCarloData):
			result = MonteCarloVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::PatternData):
			result = PatternVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::PatternBoundData):
			result = PatternBoundVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::PreferencesData):
			result = PreferencesVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::SiteData):
			result = SiteVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::VectorData):
			result = VectorVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::VibrationData):
			result = VibrationVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::WidgetData):
			result = WidgetVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ZMatrixData):
			result = ZMatrixVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ZMatrixElementData):
			result = ZMatrixElementVariable::setAccessor(accessor_, executerv, setrv, arrayIndex_ != NULL, i);
			break;
		default:
			printf("Internal Error: StepNode doesn't recognise this type (%s) (set)\n", VTypes::dataType(previousType_));
			break;
	}
	Messenger::exit("StepNode::set");
	return result;
}

// Set from returnvalue node
bool StepNode::set(ReturnValue& rv)
{
	printf("Internal Error: Use StepNode::set(ReturnValue) for StepNodes.\n");
	return FALSE;
}

// Initialise node
bool StepNode::initialise()
{
	printf("Internal Error: A StepNode cannot be initialised.\n");
	return FALSE;
}

// Static function to search accessors of type represented by this path step
StepNode* StepNode::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("StepNode::findAccessor");
	// From the return type of the node, determine which (static) function to call
	StepNode* result = NULL;
	switch (returnType_)
	{
		case (VTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData.\n");
			break;
		case (VTypes::AtomData):
			result = AtomVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::BasisPrimitiveData):
			result = BasisPrimitiveVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::BasisShellData):
			result = BasisShellVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::BondData):
			result = BondVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::CellData):
			result = CellVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ColourScaleData):
			result = ColourScaleVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ColourScalePointData):
			result = ColourScalePointVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::DialogData):
			result = DialogVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::EigenvectorData):
			result = EigenvectorVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ElementData):
			result = ElementVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::EnergyStoreData):
			result = EnergyStoreVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ForcefieldData):
			result = ForcefieldVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ForcefieldAtomData):
			result = ForcefieldAtomVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ForcefieldBoundData):
			result = ForcefieldBoundVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::GlyphData):
			result = GlyphVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::GlyphDataData):
			result = GlyphDataVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::GridData):
			result = GridVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::MatrixData):
			result = VectorVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::MeasurementData):
			result = MeasurementVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ModelData):
			result = ModelVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::MonteCarloData):
			result = MonteCarloVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::PatternData):
			result = PatternVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::PatternBoundData):
			result = PatternBoundVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::PreferencesData):
			result = PreferencesVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::SiteData):
			result = SiteVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::VectorData):
			result = VectorVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::WidgetData):
			result = WidgetVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ZMatrixData):
			result = ZMatrixVariable::accessorSearch(name, arrayIndex, argList);
			break;
		case (VTypes::ZMatrixElementData):
			result = ZMatrixElementVariable::accessorSearch(name, arrayIndex, argList);
			break;
		default:
			printf("Internal Error: StepNode doesn't know how to search for accessors in type '%s'.\n", VTypes::dataType(returnType_));
			break;
	}
	Messenger::exit("StepNode::findAccessor");
	return result;
}
