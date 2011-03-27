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

#define CELLCHUNKSIZE 100

// Partition Cell Data
class PartitionCellData
{
	// Constructor
	PartitionCellData();
	
	public:
	// List pointers
	PartitionCellData *next, *prev;
	// Data
	int data[CELLCHUNKSIZE*3];
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
	// Number of cells in partition
	int nCells_;
	// List of basic coordinates of cells
	List<PartitionCellData> cells_;
	// Primitive containing parition data for rendering
	GridPrimitive partitionPrimitive_;

	public:
	// Set id of partition
	void setId(int id);
	// Return id of partition
	int id();
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
	// Pointer to checkvalues() function (if defined)
	Tree *checkFunction_;
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
	// List of partitions (generated from last call to updatePartitions)
	List<PartitionData> partitions_;
	// Grid structure holding illustrative partition data
	Grid grid_;
	// Icon of illustrative grid
	QIcon icon_;
	
	public:
	// Update partition information (after load or change in options)
	void updatePartitions(bool generateIcon);
	// Return number of partitions now recognised in grid
	int nPartitions();
	// Return list object containing partition information
	List<PartitionData> &partitions();
	// Return the grid structure
	Grid &grid();
	// Return icon containing illustrative partitions
	QIcon &icon();
};

#endif
