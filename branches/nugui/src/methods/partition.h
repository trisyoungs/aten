/*
	*** Partitioning Scheme
	*** src/methods/partition.h
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

#ifndef ATEN_PARTITION_H
#define ATEN_PARTITION_H

#include "base/dnchar.h"
#include "classes/grid.h"
#include "render/primitive.h"
#include "parser/program.h"
#include "parser/usercommandnode.h"
#include "parser/double.h"
#include "parser/integer.h"
#include "methods/disorderdata.h"

#define CELLCHUNKSIZE 1000

// Partition Cell Data
class PartitionCellData
{
	public:
	// Constructor
	PartitionCellData();
	// List pointers
	PartitionCellData *next, *prev;

	public:
	// Data
	int data[CELLCHUNKSIZE*3];
	// Position of next data to be added
	int dataPos;
};

// Partition Data
class PartitionData
{
	public:
	// Constructor
	PartitionData();
	// List pointers
	PartitionData *prev, *next;

	/*
	// Data
	*/
	private:
	// Integer id of partition
	int id_;
	// Name of the partition
	Dnchar name_;
	// Number of cells in partition
	int nCells_;
	// List of basic coordinates of cells
	List<PartitionCellData> cells_;
	// Pointer to current PartitionCellData
	PartitionCellData *currentCellChunk_;
	// Volume of partition
	double volume_;
	// Current reduced atomic mass present in partition
	double reducedMass_;
	// Reflist of components targeting this partition
	Reflist<DisorderData,int> components_;
	
	public:
	// Set id of partition
	void setId(int id);
	// Return id of partition
	int id();
	// Set name of partition
	void setName(const char *s);
	// Return name of partition
	const char *name();
	// Clear cells list
	void clear();
	// Add cell to list
	void addCell(int ix, int iy, int iz);
	// Return random cell from list
	int *randomCell();
	// Calculate volume based on supplied volume element
	void calculateVolume(double velement);
	// Return volume of partition
	double volume();
	// Adjust partition reduced mass contents
	void adjustReducedMass(Atom *i, bool subtract = FALSE);
	// Adjust partition reduced mass contents (from Model)
	void adjustReducedMass(Model *m, bool subtract = FALSE);
	// Return current density of partition
	double density();
	// Clear component list
	void clearComponents();
	// Add specified component to list
	void addComponent(DisorderData *component);
	// Return number of components in list
	int nComponents();
	// Return nth component in list
	DisorderData *component(int id);
};

// Partitioning Scheme for Disordered Builder
class PartitioningScheme
{
	public:
	// Constructor / Destructor
	PartitioningScheme();
	~PartitioningScheme();
	// List pointers
	PartitioningScheme *prev, *next;

	/*
	// Function Data
	*/
	private:
	// Program defining scheme, variables and functions
	Program schemeDefinition_;
	// Name of scheme (retrieved fron name() function)
	Dnchar name_;
	// Description of scheme (retrieved from description() function)
	Dnchar description_;
	// Pointer to partition() function
	Tree *partitionFunction_;
	// Pointer to partitionname() function
	Tree *partitionNameFunction_;
	
	public:
	// Return Program structure
	Program &schemeDefinition();
	// Setup scheme information from generated program structure
	bool initialise();
	// Return name of partitioning scheme
	const char *name();
	// Return description of partitioning scheme
	const char *description();
	// Return whether the partition function has any user-definable options
	bool hasOptions();
	// Execute dialog for user-definable options in partition function
	bool runOptions();


	/*
	// Partition Data
	*/
	private:
	// List of all possible partitions in scheme
	List<PartitionData> partitions_;
	// Grid structure holding illustrative partition data
	Grid grid_;
	// Icon of illustrative grid
	QIcon icon_;
	// Tree parent for UserCommandNode holding 'partition()' function
	Tree tree_;
	// User command nodes for varions functions
	UserCommandNode partitionFunctionNode_, partitionNameNode_;
	// Variables to hold passed coordinates
	DoubleVariable xVariable_, yVariable_, zVariable_;
	IntegerVariable idVariable_;
	// Grid size for rough approximation
	Vec3<int> roughGridSize_;
	// Fine grid size (used for actual calculation)
	Vec3<int> fineGridSize_;
	
	public:
	// Update partition information (after load or change in options)
	void updatePartitions(bool useRoughGrid);
	// Return number of partitions now recognised in grid
	int nPartitions();
	// Clear partition component lists
	void clearComponentLists();
	// Return first partition in list
	PartitionData *partitions();
	// Return nth partition in list
	PartitionData *partition(int id);
	// Return name of nth partition in list
	const char *partitionName(int id);
	// Return partition in which simple (unit) coordinate falls
	int partitionId(double x, double y, double z);
	// Return the grid structure
	Grid &grid();
	// Return icon containing illustrative partitions
	QIcon &icon();
	// Return fine grid size
	Vec3<int> fineGridSize();
};

#endif
