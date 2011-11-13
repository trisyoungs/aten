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
#include "base/progress.h"

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

// Set name of partition
void PartitionData::setName(const char *s)
{
	name_ = s;
}

// Return name of partition
const char *PartitionData::name()
{
	return name_.get();
}

// Clear cells list
void PartitionData::clear()
{
	cells_.clear();
	nCells_ = 0;
	volume_ = 0.0;
	reducedMass_ = 0.0;
	currentCellChunk_ = NULL;
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
void PartitionData::adjustReducedMass(Atom *i, bool subtract)
{
	if (subtract) reducedMass_ -= elements().atomicMass(i) / AVOGADRO;
	else reducedMass_ += elements().atomicMass(i) / AVOGADRO;
}

// Adjust partition density based on supplied model
void PartitionData::adjustReducedMass(Model *m, bool subtract)
{
	for (Atom *i = m->atoms(); i != NULL; i = i->next) adjustReducedMass(i, subtract);
}

// Return current density of partition
double PartitionData::density()
{
	return reducedMass_ / (volume_ * 1.0E-24);
}

// Clear component list
void PartitionData::clearComponents()
{
	components_.clear();
}

// Add specified component to list
void PartitionData::addComponent(DisorderData *component)
{
	components_.add(component);
}

// Return number of components in list
int PartitionData::nComponents()
{
	return components_.nItems();
}

// Return nth component in list
DisorderData *PartitionData::component(int id)
{
	if ((id < 0) || (id >= components_.nItems())) return NULL;
	return components_[id]->item;
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
	partitionNameFunction_ = NULL;
	partitionOptionsFunction_ = NULL;
	absolute_ = FALSE;

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
bool PartitioningScheme::initialise()
{
	msg.enter("PartitioningScheme::initialise");

	ReturnValue rv;
	bool success;
	Variable *v;
	int scopelevel, nparts;
	
	// Retrieve name
	v = schemeDefinition_.mainProgram()->findVariableInScope("name", scopelevel);
	if (v != NULL)
	{
		v->initialise();
		v->execute(rv);
		name_ = rv.asString();
	}
	else
	{
		msg.print("No 'name' variable defined in this partitioning scheme.\n");
		name_ = "???";
	}

	// Retrieve description
	v = schemeDefinition_.mainProgram()->findVariableInScope("description", scopelevel);
	if (v != NULL)
	{
		v->initialise();
		v->execute(rv);
		description_ = rv.asString();
	}
	else
	{
		msg.print("No 'description' variable defined in this partitioning scheme.\n");
		description_ = "???";
	}

	// Locate 'npartitions' variable
	v = schemeDefinition_.mainProgram()->findVariableInScope("npartitions", scopelevel);
	if (v == NULL)
	{
		msg.print("Error: No 'npartitions' variable defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}
	msg.print(Messenger::Verbose, "  --> Found 'npartitions' variable in partitioning scheme '%s'.\n", name_.get());
	v->initialise();
	success = v->execute(rv);
	nparts  = rv.asInteger();
	if (nparts < 1)
	{
		msg.print("Error: Invalid 'npartitions' (%i) found in partitioning scheme '%s'\n", nparts, name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}
	
	// Retrieve grid information
	v = schemeDefinition_.mainProgram()->findVariableInScope("roughgrid", scopelevel);
	if (v != NULL)
	{
		v->initialise();
		v->execute(rv);
		roughGridSize_.set(rv.asInteger(0,success), rv.asInteger(1,success), rv.asInteger(2,success));
		msg.print(Messenger::Verbose, "  --> Rough grid size for scheme '%s' is %i %i %i\n", name_.get(), roughGridSize_.x, roughGridSize_.y, roughGridSize_.z);
	}
	else
	{
		msg.print("Error: No 'roughgrid' variable defined in partitioning scheme '%s'.\n", name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}
	v = schemeDefinition_.mainProgram()->findVariableInScope("finegrid", scopelevel);
	if (v != NULL)
	{
		v->initialise();
		v->execute(rv);
		fineGridSize_.set(rv.asInteger(0,success), rv.asInteger(1,success), rv.asInteger(2,success));
		msg.print(Messenger::Verbose, "  --> Fine grid size for scheme '%s' is %i %i %i\n", name_.get(), fineGridSize_.x, fineGridSize_.y, fineGridSize_.z);
	}
	else
	{
		msg.print("Error: No 'finegrid' variable defined in partitioning scheme '%s'.\n", name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}
	
	// Locate 'partition' function
	partitionFunction_ = schemeDefinition_.mainProgram()->findLocalFunction("partition");
	if (partitionFunction_) msg.print(Messenger::Verbose, "  --> Found 'partition' function in partitioning scheme '%s'.\n", name_.get());
	else
	{
		msg.print("Error: No 'partition' function defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}
	partitionFunctionNode_.setFunction(partitionFunction_);

	// Locate 'partitionName' function
	partitionNameFunction_ = schemeDefinition_.mainProgram()->findLocalFunction("partitionName");
	if (partitionNameFunction_) msg.print(Messenger::Verbose, "  --> Found 'partitionName' function in partitioning scheme '%s'.\n", name_.get());
	else
	{
		msg.print("Error: No 'partitionName' function defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialise");
		return FALSE;
	}
	partitionNameNode_.setFunction(partitionNameFunction_);

	// Locate 'partitionOptions' function (if one exists)
	partitionOptionsFunction_ = schemeDefinition_.mainProgram()->findLocalFunction("partitionOptions");
	if (partitionOptionsFunction_)
	{
		msg.print(Messenger::Verbose, "  --> Found 'partitionOptions' function in partitioning scheme '%s'.\n", name_.get());
		partitionOptionsNode_.setFunction(partitionOptionsFunction_);
		hasOptions_ = TRUE;
	}
	else hasOptions_ = FALSE;

	// Run main partition tree so all variables / globals are set
	if (!schemeDefinition_.mainProgram()->execute(rv))
	{
		msg.print("Error: Failed to run through partitioning scheme code.\n");
		return FALSE;
	}

	// Now can set up basic partition list
	partitions_.clear();
	for (int n = 0; n<nparts; ++n)
	{
		PartitionData *pd = partitions_.add();
		pd->setId(n);
		pd->setName(partitionName(n));
	}
	
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

// Find and set named variable in partitionFunction_
bool PartitioningScheme::setVariable(const char *name, const char *value)
{	
	msg.enter("PartitioningScheme::setVariable");
	if (partitionFunction_ == NULL)
	{
		msg.print("Internal Error: No partitionFunction_ defined for variable search.\n");
		msg.exit("PartitioningScheme::setVariable");
		return FALSE;
	}
	return partitionFunction_->defaultDialog().setWidgetValue(name, value);
	msg.exit("PartitioningScheme::setVariable");
}

// Set whether scheme is absolute
void PartitioningScheme::setAbsolute()
{
	absolute_ = TRUE;
}


// Update partition information (after load or change in options)
void PartitioningScheme::updatePartitions(bool useRoughGrid)
{
	msg.enter("PartitioningScheme::updatePartitions");
	
	// Recalculate grid points (and icon if requested)
	Vec3<int> npoints;
	double ***data;
	
	// If we're using the rough grid, we'll assume that we're also supposed to be filling a Grid structure as well
	if (useRoughGrid)
	{
		npoints = roughGridSize_;
		grid_.initialise(Grid::RegularXYZData, npoints);
		data = grid_.data3d();
	}
	else npoints = fineGridSize_;
	
	// Clear partition data
	for (PartitionData *pd = partitions_.first(); pd != NULL; pd = pd->next) pd->clear();

	ReturnValue rv;
	bool success;
	int i, j, k, pid;
	double dx = 1.0/npoints.x, dy = 1.0/npoints.y, dz = 1.0/npoints.z;
	// Coordinates {xyz} will be in the centre of the grid 'cells'
	double x, y, z;

	// Okay, do the calculation
	Dnchar text(-1, "Generating partition data for scheme '%s'", name_.get());
	int progid = progress.initialise(text.get(), npoints.x, FALSE);
	x = 0.5*dx;
	for (i=0; i<npoints.x; ++i)
	{
		xVariable_.setFromDouble(x);
		y = 0.5*dy;
		for (j=0; j<npoints.y; ++j)
		{
			yVariable_.setFromDouble(y);
			z = 0.5*dz;
			for (k=0; k<npoints.z; ++k)
			{
				zVariable_.setFromDouble(z);
				// Get integer id of the partition at this location
				success = partitionFunctionNode_.execute(rv);
				pid = rv.asInteger();
				
				if (useRoughGrid) data[i][j][k] = pid;
				else partitions_[pid]->addCell(i,j,k);
				z += dz;
			}
			y += dy;
		}
		x += dx;
		progress.update(progid);
	}
	progress.terminate(progid);

	msg.exit("PartitioningScheme::updatePartitions");
}

// Return number of partitions now recognised in grid
int PartitioningScheme::nPartitions()
{
	return partitions_.nItems();
}

// Clear partition component lists
void PartitioningScheme::clearComponentLists()
{
	for (PartitionData *pd = partitions_.first(); pd != NULL; pd = pd->next) pd->clearComponents();
}

// Return list object containing partition information
PartitionData *PartitioningScheme::partitions()
{
	return partitions_.first();
}

// Return nth partition in list
PartitionData *PartitioningScheme::partition(int id)
{
	return partitions_[id];
}

// Return name of nth partition in list
const char *PartitioningScheme::partitionName(int id)
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
	if (!hasOptions_) return TRUE;
	ReturnValue rv;
	partitionOptionsNode_.execute(rv);
	return rv.asBool();
}

// Return partition in which simple (unit) coordinate falls
int PartitioningScheme::partitionId(double x, double y, double z)
{
	ReturnValue rv;
	xVariable_.setFromDouble(x);
	yVariable_.setFromDouble(y);
	zVariable_.setFromDouble(z);
	partitionFunctionNode_.execute(rv);
	return rv.asInteger();
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

// Return fine grid size
Vec3<int> PartitioningScheme::fineGridSize()
{
	return fineGridSize_;
}