/*
	*** Grid Variable and Array
	*** src/parser/grid.cpp
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

#include "parser/grid.h"
#include "parser/stepnode.h"
#include "model/model.h"
#include "base/grid.h"

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
GridVariable::GridVariable(Grid* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GridData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
GridVariable::~GridVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor GridVariable::accessorData[GridVariable::nAccessors] = {
	{ "axes",			VTypes::CellData,	0, true },
	{ "axisMajorSpacing", 		VTypes::VectorData,	0, false },
	{ "axisMinorTicks", 		VTypes::IntegerData,	3, false },
	{ "axisPositionX", 		VTypes::VectorData,	0, false },
	{ "axisPositionY", 		VTypes::VectorData,	0, false },
	{ "axisPositionZ", 		VTypes::VectorData,	0, false },
	{ "axisVisible", 		VTypes::IntegerData,	3, false },
	{ "colour",			VTypes::DoubleData,	4, false },
	{ "colourScale",		VTypes::IntegerData,	0, false },
	{ "cutoff",			VTypes::DoubleData,	0, false },
	{ "dataMaximum", 		VTypes::VectorData,	0, false },
	{ "dataMinimum", 		VTypes::VectorData,	0, false },
	{ "loopOrder", 			VTypes::IntegerData,	3, false },
	{ "name",			VTypes::StringData,	0, false },
	{ "nx",				VTypes::IntegerData,	0, true },
	{ "ny",				VTypes::IntegerData,	0, true },
	{ "nz",				VTypes::IntegerData,	0, true },
	{ "nPoints",			VTypes::IntegerData,	0, true },
	{ "origin", 			VTypes::VectorData,	0, false },
	{ "outlineVolume",		VTypes::IntegerData,	0, false },
	{ "periodic",			VTypes::IntegerData,	0, false },
	{ "secondaryColour",		VTypes::DoubleData,	4, false },
	{ "secondaryCutoff",		VTypes::DoubleData,	0, false },
	{ "secondaryUpperCutoff",	VTypes::DoubleData,	0, false },
	{ "shiftX",			VTypes::IntegerData,	0, false },
	{ "shiftY",			VTypes::IntegerData,	0, false },
	{ "shiftZ",			VTypes::IntegerData,	0, false },
	{ "type",			VTypes::StringData,	0, true },
	{ "upperCutoff",		VTypes::DoubleData,	0, false },
	{ "useColourScale",		VTypes::IntegerData,	0, false },
	{ "useDataForZ",		VTypes::IntegerData,	0, false },
	{ "visible",			VTypes::IntegerData,	0, false }
};

// Function data
FunctionAccessor GridVariable::functionData[GridVariable::nFunctions] = {
	{ "data",		VTypes::DoubleData,	"IIi",	"int i, int j, int k = -1" },
	{ "initialise",		VTypes::IntegerData,	"SIIi",	"string type, int nx, int ny, int nz = -1" },
	{ "shift",		VTypes::NoData,		"IIIi",	"int dx, int dy, int dz, bool shiftAtoms = false" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* GridVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return GridVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* GridVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("GridVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(name, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'Grid&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("GridVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Grid&' function named '%s'.", qPrintable(name));
			Messenger::exit("GridVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::GridData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Grid&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.", accessorData[i].name);
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (argList != NULL)
		{
			Messenger::print("Error: Argument list given to 'Grid&' array member '%s'.", qPrintable(name));
			Messenger::exit("GridVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::GridData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("GridVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool GridVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("GridVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Grid type.\n", i);
		Messenger::exit("GridVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("GridVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("GridVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Grid* ptr = (Grid*) rv.asPointer(VTypes::GridData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::GridData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (GridVariable::Axes):
			rv.set(VTypes::CellData, ptr->cell());
			break;
		case (GridVariable::AxisMajorSpacing):
			if (hasArrayIndex) rv.set( ptr->axisMajorSpacing()[arrayIndex-1] );
			else rv.set(ptr->axisMajorSpacing());
			break;
		case (GridVariable::AxisMinorTicks):
			if (hasArrayIndex) rv.set( ptr->axisMinorTicks()[arrayIndex-1] );
			else
			{
				int tempArray[3] = { ptr->axisMinorTicks().x, ptr->axisMinorTicks().y, ptr->axisMinorTicks().z };
				rv.setArray( VTypes::IntegerData, tempArray, 3);
			}
		case (GridVariable::AxisPositionX):
		case (GridVariable::AxisPositionY):
		case (GridVariable::AxisPositionZ):
			if (hasArrayIndex) rv.set( ptr->axisPosition(acc-AxisPositionX)[arrayIndex-1] );
			else rv.set(ptr->axisPosition(acc-AxisPositionX));
			break;
		case (GridVariable::AxisVisible):
			if (hasArrayIndex) rv.set( ptr->axisVisible()[arrayIndex-1] );
			else rv.setArray( VTypes::IntegerData, ptr->axisVisible(), 3);
			break;
		case (GridVariable::Colour):
			if (hasArrayIndex) rv.set( ptr->primaryColour()[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->primaryColour(), 4);
			break;
		case (GridVariable::ColourScale):
			rv.set(ptr->colourScale()+1);
			break;
		case (GridVariable::Cutoff):
			rv.set(ptr->lowerPrimaryCutoff());
			break;
		case (GridVariable::DataMaximum):
			rv.set(ptr->dataMaximum());
			break;
		case (GridVariable::DataMinimum):
			rv.set(ptr->dataMinimum());
			break;
		case (GridVariable::LoopOrder):
			if (hasArrayIndex) rv.set( ptr->loopOrder()[arrayIndex-1] );
			else
			{
				int tempArray[3] = { ptr->loopOrder().x, ptr->loopOrder().y, ptr->loopOrder().z };
				rv.setArray( VTypes::IntegerData, tempArray, 3);
			}
			break;
		case (GridVariable::Name):
			rv.set(ptr->name());
			break;
		case (GridVariable::NX):
		case (GridVariable::NY):
		case (GridVariable::NZ):
			rv.set(ptr->nXYZ().get(acc-GridVariable::NX));
			break;
		case (GridVariable::NPoints):
			rv.set(ptr->nPoints());
			break;
		case (GridVariable::Origin):
			rv.set(ptr->origin());
			break;
		case (GridVariable::OutlineVolume):
			rv.set(ptr->outlineVolume());
			break;
		case (GridVariable::Periodic):
			rv.set(ptr->periodic());
			break;
		case (GridVariable::SecondaryColour):
			if (hasArrayIndex) rv.set( ptr->secondaryColour()[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->secondaryColour(), 4);
			break;
		case (GridVariable::SecondaryCutoff):
			rv.set(ptr->lowerSecondaryCutoff());
			break;
		case (GridVariable::SecondaryUpperCutoff):
			rv.set(ptr->upperSecondaryCutoff());
			break;
		case (GridVariable::ShiftX):
			rv.set(ptr->shift().x);
			break;
		case (GridVariable::ShiftY):
			rv.set(ptr->shift().y);
			break;
		case (GridVariable::ShiftZ):
			rv.set(ptr->shift().z);
			break;
		case (GridVariable::Type):
			rv.set(Grid::gridType(ptr->type()));
			break;
		case (GridVariable::UpperCutoff):
			rv.set(ptr->upperPrimaryCutoff());
			break;
		case (GridVariable::UseColourScale):
			rv.set(ptr->useColourScale());
			break;
		case (GridVariable::UseDataForZ):
			rv.set(ptr->useDataForZ());
			break;
		case (GridVariable::Visible):
			rv.set(ptr->isVisible());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in GridVariable.\n", accessorData[i].name);
			result = false;
			break;
	}
	Messenger::exit("GridVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool GridVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("GridVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Grid type.\n", i);
		Messenger::exit("GridVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = true;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = false;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = false;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newValue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = false;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("GridVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	Grid* ptr = (Grid*) sourcerv.asPointer(VTypes::GridData, result);
	int n;
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::GridData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (GridVariable::AxisMajorSpacing):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setAxisMajorSpacing(n, newValue.asVector(result)[n]);
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->setAxisMajorSpacing(n, newValue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setAxisMajorSpacing(arrayIndex-1, newValue.asDouble(result));
			else for (n=0; n<3; ++n) ptr->setAxisMajorSpacing(n, newValue.asDouble(result));
			break;
		case (GridVariable::AxisMinorTicks):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setAxisMinorTicks(n, newValue.asVector(result)[n]);
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->setAxisMinorTicks(n, newValue.asInteger(n, result));
			else if (hasArrayIndex) ptr->setAxisMinorTicks(arrayIndex-1, newValue.asInteger(result));
			else for (n=0; n<3; ++n) ptr->setAxisMinorTicks(n, newValue.asInteger(result));
			break;
		case (GridVariable::AxisPositionX):
		case (GridVariable::AxisPositionY):
		case (GridVariable::AxisPositionZ):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setAxisPosition(acc-AxisPositionX, n, newValue.asVector(result)[n]);
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->setAxisPosition(acc-AxisPositionX, n, newValue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setAxisPosition(acc-AxisPositionX, arrayIndex-1, newValue.asDouble(result));
			else for (n=0; n<3; ++n) ptr->setAxisPosition(acc-AxisPositionX, n, newValue.asDouble(result));
			break;
		case (GridVariable::AxisVisible):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setAxisVisible(n, newValue.asVector(result)[n]);
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->setAxisVisible(n, newValue.asInteger(n, result));
			else if (hasArrayIndex) ptr->setAxisVisible(arrayIndex-1, newValue.asBool());
			else for (n=0; n<3; ++n) ptr->setAxisVisible(n, newValue.asBool());
			break;
		case (GridVariable::Colour):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->primaryColour()[n] = newValue.asVector(result)[n];
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->primaryColour()[n] = newValue.asDouble(n, result);
			else if (hasArrayIndex) ptr->primaryColour()[arrayIndex-1] = newValue.asDouble(result);
			else for (n=0; n<4; ++n) ptr->primaryColour()[n] = newValue.asDouble(result);
			break;
		case (GridVariable::ColourScale):
			ptr->setColourScale( newValue.asInteger()-1 );
			ptr->setUseColourScale( ptr->colourScale() != 0 );
			break;
		case (GridVariable::Cutoff):
			ptr->setLowerPrimaryCutoff( newValue.asDouble() );
			break;
		case (GridVariable::DataMaximum):
			ptr->setDataMaximum( newValue.asVector() );
			break;
		case (GridVariable::DataMinimum):
			ptr->setDataMinimum( newValue.asVector() );
			break;
		case (GridVariable::Name):
			ptr->setName( newValue.asString() );
			break;
		case (GridVariable::Origin):
			ptr->setOrigin( newValue.asVector() );
			break;
		case (GridVariable::OutlineVolume):
			ptr->setOutlineVolume( newValue.asInteger() );
			break;
		case (GridVariable::Periodic):
			ptr->setPeriodic( newValue.asInteger() );
			break;
		case (GridVariable::SecondaryColour):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->secondaryColour()[n] = newValue.asVector(result)[n];
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->secondaryColour()[n] = newValue.asDouble(n, result);
			else if (hasArrayIndex) ptr->secondaryColour()[arrayIndex-1] = newValue.asDouble(result);
			else for (n=0; n<4; ++n) ptr->secondaryColour()[n] = newValue.asDouble(result);
			break;
		case (GridVariable::SecondaryCutoff):
			ptr->setLowerSecondaryCutoff( newValue.asDouble() );
			break;
		case (GridVariable::SecondaryUpperCutoff):
			ptr->setUpperSecondaryCutoff( newValue.asDouble() );
			break;
		case (GridVariable::ShiftX):
			ptr->setShift(0, newValue.asInteger());
			break;
		case (GridVariable::ShiftY):
			ptr->setShift(1, newValue.asInteger());
			break;
		case (GridVariable::ShiftZ):
			ptr->setShift(2, newValue.asInteger());
			break;
		case (GridVariable::UpperCutoff):
			ptr->setUpperPrimaryCutoff( newValue.asDouble() );
			break;
		case (GridVariable::UseColourScale):
			ptr->setUseColourScale( newValue.asBool() );
			break;
		case (GridVariable::UseDataForZ):
			ptr->setUseDataForZ( newValue.asBool() );
			break;
		case (GridVariable::Visible):
			ptr->setVisible( newValue.asBool() );
			break;
		default:
			printf("GridVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("GridVariable::setAccessor");
	return result;
}

// Perform desired function
bool GridVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("GridVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Grid type.\n", i);
		Messenger::exit("GridVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Grid* ptr = (Grid*) rv.asPointer(VTypes::GridData, result);
	Grid::GridType gt;
	int nx, ny, nz;
	if (result) switch (i)
	{
		case (GridVariable::Data):
			// Check type of grid data stored...
			switch (ptr->type())
			{
				case (Grid::RegularXYData):
					if (node->nArgs() == 3) Messenger::print("Warning: Third dimension given to 'data' function will be ignored...");
					nx = node->argi(0) - 1;
					ny = node->argi(1) - 1;
					if ((nx < 0) || (nx >= ptr->nXYZ().x))
					{
						Messenger::print("Error: X value for grid (%i) is out of range (nx = %i)", nx+1, ptr->nXYZ().x);
						result = false;
					}
					else if ((ny < 0) || (ny >= ptr->nXYZ().y))
					{
						Messenger::print("Error: Y value for grid (%i) is out of range (ny = %i)", ny+1, ptr->nXYZ().y);
						result = false;
					}
					else rv.set( ptr->data2d()[nx][ny] );
					break;
				case (Grid::RegularXYZData):
					if (node->nArgs() != 3)
					{
						Messenger::print("Error: Third dimension for 3D grid not provided in 'data' function.");
						result = false;
						break;
					}
					nx = node->argi(0) - 1;
					ny = node->argi(1) - 1;
					nz = node->argi(2) - 1;
					if ((nx < 0) || (nx >= ptr->nXYZ().x))
					{
						Messenger::print("Error: X value for grid (%i) is out of range (nx = %i)", nx+1, ptr->nXYZ().x);
						result = false;
					}
					else if ((ny < 0) || (ny >= ptr->nXYZ().y))
					{
						Messenger::print("Error: Y value for grid (%i) is out of range (ny = %i)", ny+1, ptr->nXYZ().y);
						result = false;
					}
					else if ((nz < 0) || (nz >= ptr->nXYZ().z))
					{
						Messenger::print("Error: Z value for grid (%i) is out of range (nz = %i)", nz+1, ptr->nXYZ().z);
						result = false;
					}
					else rv.set( ptr->data3d()[nx][ny][nz] );
					break;
				case (Grid::FreeXYZData):
					Messenger::print("Free (irregular) grid data cannot be accessed with the 'data' function.");
					result = false;
					break;
			}
			break;
		case (GridVariable::Initialise):
			gt = Grid::gridType(node->argc(0), true);
			if (gt == Grid::nGridTypes) return false;
			result = ptr->initialise(gt, Vec3<int>(node->argi(1), node->argi(2), node->nArgs() == 3 ? -1 : node->argi(3)));
			break;
		case (GridVariable::Shift):
			ptr->setShift(ptr->shift().x+node->argi(0), ptr->shift().y+node->argi(1), ptr->shift().z+node->argi(2));
			if (node->argb(3))
			{
				Model* m = ptr->parent();
				// Determine shift amount...
				Vec3<double> vec;
				vec += ptr->axes().columnAsVec3(0) * node->argi(0);
				vec += ptr->axes().columnAsVec3(1) * node->argi(1);
				vec += ptr->axes().columnAsVec3(2) * node->argi(2);
				// Move atoms....
				m->beginUndoState("Shift atoms with grid");
				m->markAll();
				m->translateSelectionLocal(vec, true);
				m->endUndoState();
			}
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in GridVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("GridVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void GridVariable::printAccessors()
{
	if (GridVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<GridVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((GridVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<GridVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
 * Variable Array
 */

// Constructor
GridArrayVariable::GridArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GridData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* GridArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return GridVariable::accessorSearch(name, arrayIndex, argList);
}

