/*
	*** Partition Data
	*** src/methods/partitiondata.h
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

#ifndef ATEN_PARTITIONDATA_H
#define ATEN_PARTITIONDATA_H

#include "methods/disorderdata.h"
#include "methods/partitioncelldata.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Partition Data
class PartitionData : public ListItem<PartitionData>
{
	public:
	// Constructor
	PartitionData();


	/*
	 * Data
	 */
	private:
	// Parent partitioning scheme
	PartitioningScheme* parent_;
	// Integer id of partition
	int id_;
	// Name of the partition
	QString name_;
	// Number of cells in partition
	int nCells_;
	// List of basic coordinates of cells
	List<PartitionCellData> cells_;
	// Pointer to current PartitionCellData
	PartitionCellData* currentCellChunk_;
	// Volume of partition
	double volume_;
	// Current reduced atomic mass present in partition
	double reducedMass_;
	// RefList of components targeting this partition
	RefList<DisorderData,int> components_;
	// Primitive containing surface data
	Primitive primitive_;
	
	public:
	// Set parent partitioning scheme
	void setParent(PartitioningScheme* scheme);
	// Copy data from specified PartitionData
	void copy(PartitionData* source);
	// Set id of partition
	void setId(int id);
	// Return id of partition
	int id();
	// Set name of partition
	void setName(QString s);
	// Return name of partition
	QString name();
	// Clear cells list
	void clear();
	// Add cell to list
	void addCell(int ix, int iy, int iz);
	// Return whether specified cell is contained in the list
	bool contains(int ix, int iy, int iz);
	// Return random cell from list
	int* randomCell();
	// Calculate volume based on supplied volume element
	void calculateVolume(double velement);
	// Return volume of partition
	double volume();
	// Reset reduced mass of partition
	void resetReducedMass();
	// Adjust partition reduced mass contents
	void adjustReducedMass(Atom* i, bool subtract = false);
	// Adjust partition reduced mass contents (from Model)
	void adjustReducedMass(Model* m, bool subtract = false);
	// Return current density of partition
	double density();
	// Clear component list
	void clearComponents();
	// Add specified component to list
	void addComponent(DisorderData* component);
	// Return number of components in list
	int nComponents();
	// Return nth component in list
	DisorderData* component(int id);
	// Return primitive, generating if necessary
	Primitive& primitive();
	// Send primitive for surface data, generating if necessary
	void sendPrimitive();
};

ATEN_END_NAMESPACE

#endif
