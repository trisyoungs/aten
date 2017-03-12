/*
	*** Partitioning Scheme
	*** src/methods/partitioningscheme.cpp
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

#include "methods/partitioningscheme.h"
#include "methods/partitiondata.h"

ATEN_USING_NAMESPACE

// Constructor
PartitioningScheme::PartitioningScheme() : ListItem<PartitioningScheme>()
{
	// Private variables
	partitionFunction_ = NULL;
	partitionNameFunction_ = NULL;
	partitionOptionsFunction_ = NULL;
	staticData_ = false;
	partitionLogPoint_ = -1;
	changeLog_ = 0;

	// Setup local UserCommandNodes
	partitionFunctionNode_.setParent(&tree_);
	partitionFunctionNode_.addArgument(&xVariable_);
	partitionFunctionNode_.addArgument(&yVariable_);
	partitionFunctionNode_.addArgument(&zVariable_);
	partitionNameNode_.setParent(&tree_);
	partitionNameNode_.addArgument(&idVariable_);
	partitionOptionsNode_.setParent(&tree_);
}

// Destructor
PartitioningScheme::~PartitioningScheme()
{
}

// Return Program structure
Program &PartitioningScheme::schemeDefinition()
{
	return schemeDefinition_;
}

// Setup scheme information from generated program structure
bool PartitioningScheme::initialiseFromProgram()
{
	Messenger::enter("PartitioningScheme::initialiseFromProgram");

	ReturnValue rv;
	bool success;
	Variable* v;
	int scopelevel, nparts;
	
	// Retrieve name
	v = schemeDefinition_.mainProgram()->findLocalVariable("name", scopelevel);
	if (v != NULL)
	{
		v->initialise();
		v->execute(rv);
		name_ = rv.asString();
	}
	else
	{
		Messenger::print("No 'name' variable defined in this partitioning scheme.");
		name_ = "???";
	}

	// Retrieve description
	v = schemeDefinition_.mainProgram()->findLocalVariable("description", scopelevel);
	if (v != NULL)
	{
		v->initialise();
		v->execute(rv);
		description_ = rv.asString();
	}
	else
	{
		Messenger::print("No 'description' variable defined in this partitioning scheme.");
		description_ = "???";
	}

	// Locate 'nPartitions' variable
	v = schemeDefinition_.mainProgram()->findLocalVariable("nPartitions", scopelevel);
	if (v == NULL)
	{
		Messenger::print("Error: No 'nPartitions' variable defined in partitioning scheme '%s'", qPrintable(name_));
		Messenger::exit("PartitioningScheme::initialiseFromProgram");
		return false;
	}
	Messenger::print(Messenger::Verbose, "  --> Found 'nPartitions' variable in partitioning scheme '%s'.", qPrintable(name_));
	v->initialise();
	success = v->execute(rv);
	nparts  = rv.asInteger();
	if (nparts < 1)
	{
		Messenger::print("Error: Invalid 'nPartitions' (%i) found in partitioning scheme '%s'", nparts, qPrintable(name_));
		Messenger::exit("PartitioningScheme::initialiseFromProgram");
		return false;
	}
	
	// Set initial grid size (from prefs)
	setGridSize(prefs.partitionGridSize());
	
	// Locate 'partition' function
	partitionFunction_ = schemeDefinition_.mainProgram()->findLocalFunction("partition");
	if (partitionFunction_) Messenger::print(Messenger::Verbose, "  --> Found 'partition' function in partitioning scheme '%s'.", qPrintable(name_));
	else
	{
		Messenger::print("Error: No 'partition' function defined in partitioning scheme '%s'", qPrintable(name_));
		Messenger::exit("PartitioningScheme::initialiseFromProgram");
		return false;
	}
	partitionFunctionNode_.setFunction(partitionFunction_);

	// Locate 'partitionName' function
	partitionNameFunction_ = schemeDefinition_.mainProgram()->findLocalFunction("partitionName");
	if (partitionNameFunction_) Messenger::print(Messenger::Verbose, "  --> Found 'partitionName' function in partitioning scheme '%s'.", qPrintable(name_));
	else
	{
		Messenger::print("Error: No 'partitionName' function defined in partitioning scheme '%s'", qPrintable(name_));
		Messenger::exit("PartitioningScheme::initialiseFromProgram");
		return false;
	}
	partitionNameNode_.setFunction(partitionNameFunction_);

	// Locate 'partitionOptions' function (if one exists)
	partitionOptionsFunction_ = schemeDefinition_.mainProgram()->findLocalFunction("partitionOptions");
	if (partitionOptionsFunction_)
	{
		Messenger::print(Messenger::Verbose, "  --> Found 'partitionOptions' function in partitioning scheme '%s'.", qPrintable(name_));
		partitionOptionsNode_.setFunction(partitionOptionsFunction_);
		hasOptions_ = true;
	}
	else hasOptions_ = false;

	// Run main partition tree so all variables / globals are set
	if (!schemeDefinition_.mainProgram()->execute(rv))
	{
		Messenger::print("Error: Failed to run through partitioning scheme code.");
		Messenger::exit("PartitioningScheme::initialiseFromProgram");
		return false;
	}

	// Now can set up basic partition list
	partitions_.clear();
	for (int n = 0; n<nparts; ++n)
	{
		PartitionData* pd = partitions_.add();
		pd->setParent(this);
		pd->setId(n);
		pd->setName(partitionName(n));
	}
	
	Messenger::exit("PartitioningScheme::initialiseFromProgram");
	return true;
}

// Setup scheme information manually (for absolute grid data)
void PartitioningScheme::initialiseAbsolute(QString name, QString description)
{
	name_ = name;
	description_ = description;
	staticData_ = true;
}

// Set name and description of scheme manually
void PartitioningScheme::setName(QString name, QString description)
{
	name_ = name;
	if (description != NULL) description_ = description;
}

// Return name of partitioning scheme
QString PartitioningScheme::name()
{
	return name_;
}

// Return description of partitioning scheme
QString PartitioningScheme::description()
{
	return description_;
}

// Find and set named variable in partitionFunction_
bool PartitioningScheme::setVariable(QString varName, QString value)
{	
	Messenger::enter("PartitioningScheme::setVariable");
	if (partitionFunction_ == NULL)
	{
		Messenger::print("Internal Error: No partitionFunction_ defined for variable search.");
		Messenger::exit("PartitioningScheme::setVariable");
		return false;
	}
	// Search for global variable by that name...
	bool result;
	Variable* var = schemeDefinition_.mainProgram()->globalVariables().find(varName);
	if (var != NULL)
	{
		Messenger::print(Messenger::Verbose, "Found global variable '%s' in partitioning scheme '%s' - setting value to '%s'", qPrintable(varName), qPrintable(name_), qPrintable(value));
		ReturnValue rv(value);
		var->set(rv);
		result = true;
	}
	else
	{
		result = partitionFunction_->defaultDialog().setWidgetValue(varName, value);
	}

	if (result) ++changeLog_;
	Messenger::exit("PartitioningScheme::setVariable");
	return result;
}

// Return whether scheme contains static data
bool PartitioningScheme::staticData()
{
	return staticData_;
}

// Create partition information from current grid data
void PartitioningScheme::createPartitionsFromGrid()
{
	Messenger::enter("PartitioningScheme::createPartitionsFromGrid");
	
	// Remove old partition data and add default (cell) partition
	partitions_.clear();
	PartitionData* pd = partitions_.add();
	pd->setId(0);
	pd->setName("Excluded Space");
	pd->setParent(this);
	
	// Loop over grid elements - we will add new partition nodes as we go...
	int i, j, k, pid;
	Vec3<int> npoints = grid_.nXYZ();
	double** *data = grid_.data3d();
	for (i=0; i<npoints.x; ++i)
	{
		for (j=0; j<npoints.y; ++j)
		{
			for (k=0; k<npoints.z; ++k)
			{
				// Get partition id from grid
				pid = floor(data[i][j][k] + 0.5);
				if (pid < 0)
				{
					printf("Developer Oversight : Found a negative number in this scheme's grid.\n");
					continue;
				}
				
				// Check pid against number of partitions currently in list
				while (pid >= partitions_.nItems())
				{
					pd = partitions_.add();
					pd->setId(pid);
					pd->setName("Generated partition " + QString::number(pid));
				}
				
				// Add cell to list
				partitions_[pid]->addCell(i, j, k);
			}
		}
	}

	Messenger::exit("PartitioningScheme::createPartitionsFromGrid");
}

// Recalculate partition information (after load or change in options)
void PartitioningScheme::recalculatePartitions()
{
	Messenger::enter("PartitioningScheme::recalculatePartitions");
	
	// If this scheme contains static data then there's nothingn to be done
	if (staticData_)
	{
		Messenger::print(Messenger::Verbose, "Scheme '%s' contains static data, so nothing to recalculate.", qPrintable(name_));
		Messenger::exit("PartitioningScheme::recalculatePartitions");
		return;
	}
	
	// If log point has not changed, do nothing
	if (changeLog_ == partitionLogPoint_)
	{
		Messenger::print(Messenger::Verbose, "Log point for partitions in scheme '%s' is up to date.", qPrintable(name_));
		Messenger::exit("PartitioningScheme::recalculatePartitions");
		return;
	}

	// Recalculate grid points
	double*** data = grid_.data3d();
	
	// Clear partition data
	for (PartitionData* pd = partitions_.first(); pd != NULL; pd = pd->next) pd->clear();

	ReturnValue rv;
	bool success;
	int i, j, k, pid;
	double dx = 1.0/gridSize_.x, dy = 1.0/gridSize_.y, dz = 1.0/gridSize_.z;
	// Coordinates {xyz} will be in the centre of the grid 'cells'
	double x, y, z;

	// Okay, do the calculation
	QString text = "Generating partition data for scheme '" + name_ + "'";
	Task* task = Messenger::initialiseTask(text, gridSize_.x);
	x = 0.5*dx;
	for (i=0; i<gridSize_.x; ++i)
	{
		xVariable_.setFromDouble(x);
		y = 0.5*dy;
		for (j=0; j<gridSize_.y; ++j)
		{
			yVariable_.setFromDouble(y);
			z = 0.5*dz;
			for (k=0; k<gridSize_.z; ++k)
			{
				zVariable_.setFromDouble(z);
				// Get integer id of the partition at this location
				success = partitionFunctionNode_.execute(rv);
				pid = rv.asInteger();
				
				data[i][j][k] = pid;
				partitions_[pid]->addCell(i,j,k);
				z += dz;
			}
			y += dy;
		}
		x += dx;
		Messenger::incrementTaskProgress(task);
	}
	Messenger::terminateTask(task);

	partitionLogPoint_ = changeLog_;

	Messenger::exit("PartitioningScheme::recalculatePartitions");
}

// Return number of partitions now recognised in grid
int PartitioningScheme::nPartitions()
{
	return partitions_.nItems();
}

// Clear partition component lists
void PartitioningScheme::clearComponentLists()
{
	for (PartitionData* pd = partitions_.first(); pd != NULL; pd = pd->next) pd->clearComponents();
}

// Return list object containing partition information
PartitionData* PartitioningScheme::partitions()
{
	return partitions_.first();
}

// Return nth partition in list
PartitionData* PartitioningScheme::partition(int id)
{
	return partitions_[id];
}

// Return name of nth partition in list
QString PartitioningScheme::partitionName(int id)
{
	static ReturnValue rv;
	rv.set(id);
	idVariable_.set(rv);
	partitionNameNode_.execute(rv);
	return rv.asString();
}

// Return whether scheme has any defined options
bool PartitioningScheme::hasOptions()
{
	return hasOptions_;
}

// Show (execute) options dialog
bool PartitioningScheme::showOptions()
{
	if (!hasOptions_) return true;
	ReturnValue rv;
	partitionOptionsNode_.execute(rv);
	bool result = rv.asBool();
	if (result) ++changeLog_;
	return result;
}

// Return partition in which simple (unit) coordinate falls
int PartitioningScheme::partitionId(double x, double y, double z)
{
	if (staticData_)
	{
		// Determine integer cell identity
		int ix = x * gridSize_.x, iy = y * gridSize_.y, iz = z * gridSize_.z;
		// Now search for cell in partition lists, starting at second partition (first proper one)
		int id = 0;
		for (PartitionData* pd = partitions_.second(); pd != NULL; pd = pd->next)
		{
			if (pd->contains(ix, iy, iz))
			{
				id = pd->id();
				break;
			}
		}
		return id;
	}
	else
	{
		ReturnValue rv;
		xVariable_.setFromDouble(x);
		yVariable_.setFromDouble(y);
		zVariable_.setFromDouble(z);
		partitionFunctionNode_.execute(rv);
		return rv.asInteger();
	}
}

// Return the grid structure
Grid& PartitioningScheme::grid()
{
	return grid_;
}

// Return icon containing illustrative partitions
QIcon& PartitioningScheme::icon()
{
	return icon_;
}

// Set gridsize to use for calculation
void PartitioningScheme::setGridSize(Vec3<int> newSize)
{
	// Is this different from the stored values?
	if ((newSize.x != gridSize_.x) || (newSize.y != gridSize_.y) || (newSize.z != gridSize_.z))
	{
		++changeLog_;
		gridSize_ = newSize;
		grid_.initialise(Grid::RegularXYZData, gridSize_);
		grid_.setAxes( Vec3<double>(newSize.x, newSize.y, newSize.z) );
	}
}

// Return last grid size used to calculated data
Vec3<int> PartitioningScheme::gridSize()
{
	return gridSize_;
}

// Copy data from specified partition
void PartitioningScheme::copy(PartitioningScheme &source)
{
	// Clear some things which we cannot copy
	schemeDefinition_.clear();
	partitionFunction_ = NULL;
	partitionNameFunction_ = NULL;
	partitionOptionsFunction_ = NULL;
	
	// Copied data will now be absolute...
	staticData_ = true;
	hasOptions_ = false;

	// Copy basic data
	name_ = source.name_;
	description_ = source.description_;
	gridSize_ = source.gridSize_;
	
	// Copy partition data
	partitions_.clear();
	PartitionData* newPartition;
	int *data;
	for (PartitionData* pd = source.partitions_.first(); pd != NULL; pd = pd->next)
	{
		newPartition = partitions_.add();
		newPartition->copy(pd);
	}
}
