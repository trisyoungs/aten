/*
	*** Partitioning Scheme
	*** src/methods/partition.cpp
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

#include "methods/partition.h"
#include "parser/double.h"
#include "parser/usercommandnode.h"
#include "model/model.h"
#include "base/mathfunc.h"
#include "base/progress.h"
#include "classes/prefs.h"

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

// Copy data from specified PartitionData
void PartitionData::copy(PartitionData *source)
{
	// Clear any old data
	clear();
	
	name_ = source->name_;
	id_ = source->id_;

	// Step through cells list in source
	for (PartitionCellData *pcd = source->cells_.first(); pcd != NULL; pcd = pcd ->next)
	{
		int *data = pcd->data;
		for (int n=0; n<pcd->dataPos; n +=3) addCell(data[n], data[n+1], data[n+2]);
	}
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

// Return whether specified cell is contained in the list
bool PartitionData::contains(int ix, int iy, int iz)
{
	int n, div, mod;
	for (n=0; n < nCells_; ++n)
	{
		div = n/CELLCHUNKSIZE;
		mod = n%CELLCHUNKSIZE;
		if (cells_[div]->data[mod*3] != ix) continue;
		if (cells_[div]->data[mod*3+1] != iy) continue;
		if (cells_[div]->data[mod*3+2] != iz) continue;
		return TRUE;
	}
	return FALSE;
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

// Reset reduced mass of partition
void PartitionData::resetReducedMass()
{
	reducedMass_ = 0.0;
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

// Return grid primitive instance for this partition
GridPrimitive &PartitionData::gridPrimitive()
{
	return gridPrimitive_;
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
	staticData_ = FALSE;
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
	msg.enter("PartitioningScheme::initialiseFromProgram");

	ReturnValue rv;
	bool success;
	Variable *v;
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
		msg.print("No 'name' variable defined in this partitioning scheme.\n");
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
		msg.print("No 'description' variable defined in this partitioning scheme.\n");
		description_ = "???";
	}

	// Locate 'nPartitions' variable
	v = schemeDefinition_.mainProgram()->findLocalVariable("nPartitions", scopelevel);
	if (v == NULL)
	{
		msg.print("Error: No 'nPartitions' variable defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialiseFromProgram");
		return FALSE;
	}
	msg.print(Messenger::Verbose, "  --> Found 'nPartitions' variable in partitioning scheme '%s'.\n", name_.get());
	v->initialise();
	success = v->execute(rv);
	nparts  = rv.asInteger();
	if (nparts < 1)
	{
		msg.print("Error: Invalid 'nPartitions' (%i) found in partitioning scheme '%s'\n", nparts, name_.get());
		msg.exit("PartitioningScheme::initialiseFromProgram");
		return FALSE;
	}
	
	// Set initial grid size (from prefs)
	setGridSize(prefs.partitionGridSize());
	
	// Locate 'partition' function
	partitionFunction_ = schemeDefinition_.mainProgram()->findLocalFunction("partition");
	if (partitionFunction_) msg.print(Messenger::Verbose, "  --> Found 'partition' function in partitioning scheme '%s'.\n", name_.get());
	else
	{
		msg.print("Error: No 'partition' function defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialiseFromProgram");
		return FALSE;
	}
	partitionFunctionNode_.setFunction(partitionFunction_);

	// Locate 'partitionName' function
	partitionNameFunction_ = schemeDefinition_.mainProgram()->findLocalFunction("partitionName");
	if (partitionNameFunction_) msg.print(Messenger::Verbose, "  --> Found 'partitionName' function in partitioning scheme '%s'.\n", name_.get());
	else
	{
		msg.print("Error: No 'partitionName' function defined in partitioning scheme '%s'\n", name_.get());
		msg.exit("PartitioningScheme::initialiseFromProgram");
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
		msg.exit("PartitioningScheme::initialiseFromProgram");
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
	
	msg.exit("PartitioningScheme::initialiseFromProgram");
	return TRUE;
}

// Setup scheme information manually (for absolute grid data)
void PartitioningScheme::initialiseAbsolute(const char *name, const char *description)
{
	name_ = name;
	description_ = description;
	staticData_ = TRUE;
}

// Set name and description of scheme manually
void PartitioningScheme::setName(const char *name, const char *description)
{
	name_ = name;
	if (description != NULL) description_ = description;
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
	// Search for global variable by that name...
	bool result;
	Variable *var = schemeDefinition_.mainProgram()->globalVariables().find(name);
	if (var != NULL)
	{
		msg.print(Messenger::Verbose, "Found global variable '%s' in partitioning scheme '%s' - setting value to '%s'\n", name, name_.get(), value);
		ReturnValue rv(value);
		var->set(rv);
		result = TRUE;
	}
	else
	{
		result = partitionFunction_->defaultDialog().setWidgetValue(name, value);
	}

	if (result) ++changeLog_;
	msg.exit("PartitioningScheme::setVariable");
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
	msg.enter("PartitioningScheme::createPartitionsFromGrid");
	
	// Remove old partition data and add default (cell) partition
	partitions_.clear();
	PartitionData *pd = partitions_.add();
	pd->setId(0);
	pd->setName("Excluded Space");
	
	// Loop over grid elements - we will add new partition nodes as we go...
	int i, j, k, pid;
	Vec3<int> npoints = grid_.nXYZ();
	double ***data = grid_.data3d();
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
					Dnchar name(-1, "Generated partition %i", pid);
					pd->setName(name);
				}
				
				// Add cell to list
				partitions_[pid]->addCell(i, j, k);
			}
		}
	}
	
	// Generate GridPrimitives for each partition
	for (PartitionData *pd = partitions_.first(); pd != NULL; pd = pd->next)
	{
		GridPrimitive &prim = pd->gridPrimitive();
		prim.setSource(&grid_);
		grid_.setLowerPrimaryCutoff(pd->id()-0.5);
		grid_.setUpperPrimaryCutoff(pd->id()+0.5);
		prim.createSurfaceMarchingCubes();
	}
	msg.exit("PartitioningScheme::createPartitionsFromGrid");
}

// Recalculate partition information (after load or change in options)
void PartitioningScheme::recalculatePartitions()
{
	msg.enter("PartitioningScheme::updatePartitions");
	
	// If this scheme contains static data then there's nothingn to be done
	if (staticData_)
	{
		msg.print(Messenger::Verbose, "Scheme '%s' contains static data, so nothing to recalculate.\n", name_.get());
		msg.exit("PartitioningScheme::updatePartitions");
		return;
	}
	
	// If log point has not changed, do nothing
	if (changeLog_ == partitionLogPoint_)
	{
		msg.print(Messenger::Verbose, "Log point for partitions in scheme '%s' is up to date.\n", name_.get());
		msg.exit("PartitioningScheme::updatePartitions");
		return;
	}

	// Recalculate grid points
	double ***data = data = grid_.data3d();
	
	// Clear partition data
	for (PartitionData *pd = partitions_.first(); pd != NULL; pd = pd->next) pd->clear();

	ReturnValue rv;
	bool success;
	int i, j, k, pid;
	double dx = 1.0/gridSize_.x, dy = 1.0/gridSize_.y, dz = 1.0/gridSize_.z;
	// Coordinates {xyz} will be in the centre of the grid 'cells'
	double x, y, z;

	// Okay, do the calculation
	Dnchar text(-1, "Generating partition data for scheme '%s'", name_.get());
	int progid = progress.initialise(text.get(), gridSize_.x, FALSE);
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
		progress.update(progid);
	}
	progress.terminate(progid);
	
	// Generate GridPrimitives for each partition
	for (PartitionData *pd = partitions_.first(); pd != NULL; pd = pd->next)
	{
		GridPrimitive &prim = pd->gridPrimitive();
		prim.setSource(&grid_);
		grid_.setLowerPrimaryCutoff(pd->id()-0.5);
		grid_.setUpperPrimaryCutoff(pd->id()+0.5);
		prim.createSurfaceMarchingCubes();
	}
	
	partitionLogPoint_ = changeLog_;

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
		for (PartitionData *pd = partitions_.second(); pd != NULL; pd = pd->next)
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
Grid &PartitioningScheme::grid()
{
	return grid_;
}

// Return icon containing illustrative partitions
QIcon &PartitioningScheme::icon()
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
	staticData_ = TRUE;
	hasOptions_ = FALSE;

	// Copy basic data
	name_ = source.name_;
	description_ = source.description_;
	gridSize_ = source.gridSize_;
	
	// Copy partition data
	partitions_.clear();
	PartitionData *newPartition;
	int *data;
	for (PartitionData *pd = source.partitions_.first(); pd != NULL; pd = pd->next)
	{
		newPartition = partitions_.add();
		newPartition->copy(pd);
	}
}
