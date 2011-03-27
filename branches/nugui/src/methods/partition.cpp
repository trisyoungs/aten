/*
	*** Monte Carlo methods
	*** src/methods/mc.cpp
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

#include "methods/partition.h"
#include "parser/double.h"
#include "parser/usercommandnode.h"
#include "model/model.h"
#include "base/mathfunc.h"

/*
// Partition Cell Data
*/

// Constructor
PartitionCellData::PartitionCellData()
{
	next = NULL;
	prev = NULL;
	dataPos = 0;
}

/*
// Partition Data
*/

// Constructor
PartitionData::PartitionData()
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Private variables
	id_ = -1;
	nCells_ = 0;
	currentCellChunk_ = NULL;
}

// Set id of partition
void PartitionData::setId(int id)
{
	id_ = id;
}

// Return id of partition
int PartitionData::id()
{
	return id_;
}

// Clear cells list
void PartitionData::clear()
{
	cells_.clear();
	nCells_ = 0;
	volume_ = 0.0;
	reducedMass_ = 0.0;
}

// Add cell to list
void PartitionData::addCell(int ix, int iy, int iz)
{
	if (currentCellChunk_ == NULL) currentCellChunk_ = cells_.add();
	else if (currentCellChunk_->dataPos == CELLCHUNKSIZE*3) currentCellChunk_ = cells_.add();
	currentCellChunk_->data[currentCellChunk_->dataPos] = ix;
	currentCellChunk_->data[currentCellChunk_->dataPos+1] = iy;
	currentCellChunk_->data[currentCellChunk_->dataPos+2] = iz;
	currentCellChunk_->dataPos += 3;
	++nCells_;
}

// Return random cell from list
int *PartitionData::randomCell()
{
	// Generate random number between 0 and nCells_-1
	int id = AtenMath::randomi(nCells_);
	int listid = id/CELLCHUNKSIZE;
	return &cells_[id/CELLCHUNKSIZE]->data[(id%CELLCHUNKSIZE)*3];
}

// Calculate volume based on supplied volume element
void PartitionData::calculateVolume(double velement)
{
	volume_ = nCells_ * velement;
}

// Return volume of partition
double PartitionData::volume()
{
	return volume_;
}

// Adjust partition density based on supplied model
void PartitionData::adjustReducedMass(Model *source, bool subtract)
{
	if (subtract) reducedMass_ -= source->mass() / AVOGADRO;
	else reducedMass_ += source->mass() / AVOGADRO;
}

// Return current density of partition
double PartitionData::density()
{
	return reducedMass_ / (volume_ * 1.0E-24);
}

/*
// Partitioning Scheme
*/

// Constructor
PartitioningScheme::PartitioningScheme()
{
	// Public variables
	prev = NULL;
	next = NULL;
	
	// Private variables
	partitionFunction_ = NULL;
	checkFunction_ = NULL;
	partitionNameFunction_ = NULL;
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
bool PartitioningScheme::initialise()
{
	msg.enter("PartitioningScheme::initialise");

	Tree *func;
	ReturnValue rv;
	
	// Retrieve name
	func = schemeDefinition_.findGlobalFunction("name");
	if (func != NULL)
	{
		func->execute(rv);
		name_ = rv.asString();
	}
	else
	{
		msg.print("No 'name' function defined in this partitioning scheme.\n");
		name_ = "???";
	}

	// Retrieve description
	func = schemeDefinition_.findGlobalFunction("description");
	if (func != NULL)
	{
		func->execute(rv);
		description_ = rv.asString();
	}
	else
	{
		msg.print("No 'description' function defined in this partitioning scheme.\n");
		description_ = "???";
	}

	// Was a 'checkvalue' function provided?
	checkFunction_ = schemeDefinition_.findGlobalFunction("checkvalues");
	if (checkFunction_) msg.print(Messenger::Verbose, "  --> Found 'checkvalue' function in partitioning scheme '%s'.\n", name_.get());
	
	// Locate 'partition' function
	partitionFunction_ = schemeDefinition_.findGlobalFunction("partition");
	if (partitionFunction_) msg.print(Messenger::Verbose, "  --> Found 'partition' function in partitioning scheme '%s'.\n", name_.get());
	else
	{
		msg.print("Error: No 'partition' function defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}

	// Locate 'partitionname' function
	partitionNameFunction_ = schemeDefinition_.findGlobalFunction("partitionname");
	if (partitionNameFunction_) msg.print(Messenger::Verbose, "  --> Found 'partitionname' function in partitioning scheme '%s'.\n", name_.get());
	else
	{
		msg.print("Error: No 'partitionname' function defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}

	// Locate 'maxpartitions' function
	func = schemeDefinition_.findGlobalFunction("maxpartitions");
	if (func) msg.print(Messenger::Verbose, "  --> Found 'maxpartitions' function in partitioning scheme '%s'.\n", name_.get());
	else
	{
		msg.print("Error: No 'maxpartitions' function defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}
	bool success = func->execute(rv);
	int maxparts = rv.asInteger();
	if (maxparts < 1)
	{
		msg.print("Error: Invalid 'maxpartitions' (%i) returned by partitioning scheme '%s'\n", maxparts, name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}
	partitions_.clear();
	
	msg.exit("PartitioningScheme::initialise");
	return TRUE;
}

// Return name of partitioning scheme
const char *PartitioningScheme::name()
{
	return name_.get();
}

// Return description of partitioning scheme
const char *PartitioningScheme::description()
{
	return description_.get();
}

// Return whether the partition function has any user-definable options
bool PartitioningScheme::hasOptions()
{
	if (partitionFunction_ == NULL) return FALSE;
	return (partitionFunction_->widgets() ? TRUE : FALSE);
}

// Execute dialog for user-definable options in partition function
bool PartitioningScheme::runOptions()
{
	if (partitionFunction_ == NULL) return FALSE;
	return partitionFunction_->executeCustomDialog();
}

// Update partition information (after load or change in options)
void PartitioningScheme::updatePartitions(int nx, int ny, int nz, bool createGrid)
{
	msg.enter("PartitioningScheme::updatePartitions");
	
	// Recalculate grid points (and icon if requested)
	Vec3<int> npoints(nx,ny,nz);
	double ***data;
	if (createGrid)
	{
		grid_.initialise(Grid::RegularXYZData, npoints);
		data = grid_.data3d();
	}
	// Clear partition data
	for (PartitionData *pd = partitions_.first(); pd != NULL; pd = pd->next) pd->clear();

	ReturnValue rv;
	bool success;
	int i, j, k, pid;
	double dx = 1.0/npoints.x, dy = 1.0/npoints.y, dz = 1.0/npoints.z;
	// Coordinates {xyz} will be in the centre of the grid 'cells'
	double x, y, z;

	// Setup a local UserCommandNode (for speed)
	Tree tree;
	UserCommandNode pf(partitionFunction_);
	pf.setParent(&tree);
	List<TreeNode> args;
	DoubleVariable *xvar = new DoubleVariable(x, FALSE);
	DoubleVariable *yvar = new DoubleVariable(y, FALSE);
	DoubleVariable *zvar = new DoubleVariable(z, FALSE);
	args.own(xvar);
	args.own(yvar);
	args.own(zvar);
	pf.addListArguments(args.first());

	// Okay, do the calculation
	x = 0.5*dx;
	for (i=0; i<npoints.x; ++i)
	{
		xvar->setFromDouble(x);
		y = 0.5*dy;
		for (j=0; j<npoints.y; ++j)
		{
			yvar->setFromDouble(y);
			z = 0.5*dz;
			for (k=0; k<npoints.z; ++k)
			{
				zvar->setFromDouble(z);
				// Get integer id of the partition at this location
				success = pf.execute(rv);
				pid = rv.asInteger();
				
				if (createGrid) data[i][j][k] = pid;
				else partitions_[pid]->addCell(i,j,k);
				z += dz;
			}
			y += dy;
		}
		x += dx;
	}

	msg.exit("PartitioningScheme::updatePartitions");
}

// Return number of partitions now recognised in grid
int PartitioningScheme::nPartitions()
{
	return partitions_.nItems();
}

// Return list object containing partition information
List<PartitionData> &PartitioningScheme::partitions()
{
	return partitions_;
}

// Return the grid structure
Grid &PartitioningScheme::grid()
{
	return grid_;
}

// Return icon containing illustrative partitions
QIcon &PartitioningScheme::icon()
{
	return icon_;
}
