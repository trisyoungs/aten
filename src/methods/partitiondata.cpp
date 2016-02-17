/*
	*** Partition Data
	*** src/methods/partitiondata.cpp
	Copyright T. Youngs 2007-2016

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

#include "methods/partitiondata.h"
#include "partitioningscheme.h"
#include "render/primitiveset.h"
#include <QOpenGLContext>

ATEN_USING_NAMESPACE

// Constructor
PartitionData::PartitionData() : ListItem<PartitionData>()
{
	// Private variables
	id_ = -1;
	parent_ = NULL;
	nCells_ = 0;
	currentCellChunk_ = NULL;
}

// Set parent partitioning scheme
void PartitionData::setParent(PartitioningScheme* scheme)
{
	parent_ = scheme;
}

// Copy data from specified PartitionData
void PartitionData::copy(PartitionData* source)
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
void PartitionData::setName(QString name)
{
	name_ = name;
}

// Return name of partition
QString PartitionData::name()
{
	return name_;
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
		return true;
	}
	return false;
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
void PartitionData::adjustReducedMass(Atom* i, bool subtract)
{
	if (subtract) reducedMass_ -= Elements().atomicMass(i) / AVOGADRO;
	else reducedMass_ += Elements().atomicMass(i) / AVOGADRO;
}

// Adjust partition density based on supplied model
void PartitionData::adjustReducedMass(Model* m, bool subtract)
{
	for (Atom* i = m->atoms(); i != NULL; i = i->next) adjustReducedMass(i, subtract);
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
void PartitionData::addComponent(DisorderData* component)
{
	components_.add(component);
}

// Return number of components in list
int PartitionData::nComponents()
{
	return components_.nItems();
}

// Return nth component in list
DisorderData* PartitionData::component(int id)
{
	if ((id < 0) || (id >= components_.nItems())) return NULL;
	return components_[id]->item;
}

// Return primitive for this partition, generating first if necessary
Primitive& PartitionData::primitive()
{
	// Create primitive if we don't already have one
	if (!primitive_.registeredAsDynamic()) PrimitiveSet::registerDynamicPrimitive(&primitive_);

	primitive_.marchingCubes(&parent_->grid(), id_-0.5, id_+0.5, -1);

	return primitive_;
}

// Send primitive for this partition, generating first if necessary
void PartitionData::sendPrimitive()
{
	// Grab primitive
	Primitive& prim = primitive();

	// Render it
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_LIGHTING);
	double colour[4] = { 0.0,0.0,0.0,0.7 };
	prim.sendToGL(QOpenGLContext::currentContext(), GL_FILL, true, true, colour);
}
