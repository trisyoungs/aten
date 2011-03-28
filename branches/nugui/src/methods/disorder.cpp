/*
	*** Disorder Builder
	*** src/methods/disorder.cpp
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

#include "main/aten.h"
#include "methods/mc.h"
#include "model/model.h"
#include "methods/disorderdata.h"
#include "model/clipboard.h"
#include "gui/gui.h"
#include "base/sysfunc.h"

// Disorder Builder
bool MonteCarlo::disorder(Model *destmodel, PartitioningScheme *scheme, bool fixedCell)
{
	msg.enter("MonteCarlo::disorder");
	DisorderData *component;
	PartitionData *pd;
	Atom *i;
	int n, id, cycle, nSatisfied;
	Vec3<double> r;
	UnitCell *cell;
	double accuracy = 0.02, delta;

	// Step 1 - Construct cell lists in scheme
	if (scheme == NULL)
	{
		msg.print("Error: NULL scheme pointer given to disorder builder.\n");
		msg.exit("MonteCarlo::disorder");
		return FALSE;
	}
	// If we are using the default 'cell' scheme, then no need to generate a fine mesh
	Vec3<int> npoints = scheme->fineGridSize();
	msg.print("Generating fine-grained partition data...\n");
	scheme->updatePartitions(FALSE);

	// Step 2 - Construct list of components with partition references, taking copies of all component models
	Model *targetModel_ = destmodel;
	cell = targetModel_->cell();
	List<DisorderData> components_;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		m->print();
		if (m->componentInsertionPolicy() == Model::NoPolicy) continue;
		// Check that we can get the requested partition
		int id = m->componentPartition();
		if ((id < -1) || (id >= scheme->nPartitions()))
		{
			msg.print("Error: Model '%s' targets partition id %i, but it does not exist in the scheme '%s'.\n", m->name(), id, scheme->name());
			msg.exit("MonteCarlo::disorder");
			return FALSE;
		}
		component = components_.add();
		msg.print("Initialising component model '%s'...\n", m->name());
		if (!component->initialise(m, scheme->partition(id)))
		{
			msg.exit("MonteCarlo::disorder");
			return FALSE;
		}
	}
	if (components_.nItems() == 0)
	{
		msg.print("Error: No component models selected for disorder builder.\n");
		msg.exit("MonteCarlo::disorder");
		return FALSE;
	}

	// Step 3 - Determine cell size (if applicable) and perform some sanity checks
	if (!fixedCell)
	{
		msg.print("Absolute cell volume is not defined.\n");
		// If we have a free or as-yet-undefined cell, the existing model MUST NOT contain any existing atoms
		if (targetModel_->nAtoms() != 0)
		{
			msg.print("Error: For disorder building with an unspecified cell size the cell cannot contain any existing atoms.\n");
			msg.exit("MonteCarlo::disorder");
			return FALSE;
		}
		// So, we must decide on the final volume of the cell - this means that both a population and a density for each 
		// component must have been specified - i.e. no 'bulk' components and no 'free' densities
		msg.print("Calculating cell volume based on component information...\n");
		double totalVolume = 0.0;
		for (component = components_.first(); component != NULL; component = component->next)
		{
			if (component->insertionPolicy() == Model::NoPolicy) continue;
			if (component->insertionPolicy() != Model::NumberAndDensityPolicy)
			{
				msg.print("Error: For disorder building with an unspecified cell size every component must have a population and density specified with policy 'numberanddensity' (component '%s' does not).\n", component->modelName());
				msg.exit("MonteCarlo::disorder");
				return FALSE;
			}
			// All ok, so add component to volume
			totalVolume += (component->requestedPopulation() * component->modelMass() / AVOGADRO) / (component->requestedDensity() * 1.0E-24);
		}
		msg.print("From the components specified, the new cell will have a volume of %f cubic Angstroms.\n", totalVolume);
		double factor = pow(totalVolume,1.0/3.0) / pow(targetModel_->cell()->volume(),1.0/3.0);
		Matrix axes = targetModel_->cell()->axes();
		axes.applyScaling(factor,factor,factor);
		targetModel_->cell()->set(axes);
		msg.print("Based on original cell, scaling factor is %f, giving new a cell specification of:\n", factor);
		targetModel_->cell()->print();
	}

	// Step 4 - Determine partition volumes and current partition densities (arising from existing model contents)
	msg.print("Determining partition volumes and starting densities...\n");
	Matrix volumeElement = targetModel_->cell()->axes();
	volumeElement.applyScaling(1.0/npoints.x, 1.0/npoints.y, 1.0/npoints.z);
	double elementVolume = volumeElement.determinant();

	for (pd = scheme->partitions(); pd != NULL; pd = pd->next) pd->calculateVolume(elementVolume);
	// The target model may contain atoms already, so this must be subtracted from the relevant partitions
	for (i = targetModel_->atoms(); i != NULL; i = i->next)
	{
		r = i->r();
		targetModel_->cell()->fold(r, NULL, NULL);
		r = targetModel_->cell()->realToFrac(r);
		id = scheme->partitionId(r.x, r.y, r.z);
		scheme->partition(id)->adjustReducedMass(i);
	}
	msg.print("Partition\t\tVolume\tDensity\n");
	for (pd = scheme->partitions(); pd != NULL; pd = pd->next)
	{
		msg.print("%2i %8s\t%10.2f\t%8.5f\n", pd->id(), scheme->partitionName(pd->id()), pd->volume(), pd->density()); 
	}
	
	// All set up and ready - do the build
	msg.print("Cycle  Component    Region    Population     Density\n");
	for (cycle = 1; cycle <= 500; ++cycle)
	{
		// Each cycle will consist of one round of insertions, one round of adjustments, and one round of shaking
		
		/*
		// Insertions
		*/
		nSatisfied = 0;
		for (component = components_.first(); component != NULL; component = component->next)
		{
			switch (component->insertionPolicy())
			{
				// For NumberPolicy, just see if there are any molecules left to add in
				case (Model::NumberPolicy):
					if (component->requestedPopulation() == component->nAdded())
					{
						nSatisfied++;
						continue;
					}
					break;
				// For density policy, check the current density of the component's target partition
				case (Model::DensityPolicy):
					delta = fabs(1.0 - component->requestedDensity() / component->partitionDensity());
					if (delta < accuracy)
					{
						nSatisfied++;
						continue;
					}
					break;
			}
			
			// Termination condition for this component not satisfied, so more molecules needed!
			// Prepare a new candidate and do a test insertion
			component->prepareCandidate(volumeElement);
			// Test component against its own population...
			if (component->selfOverlapPenalty(cell) > 0.0) component->rejectCandidate();
			else if (component->modelOverlapPenalty(targetModel_, cell)) component->rejectCandidate();
			else if (component->otherOverlapPenalty(components_.first(), cell)) component->rejectCandidate();
			else component->acceptCandidate();
		}

		// Print summary of cycle
		for (component = components_.first(); component != NULL; component = component->next)
		{
			switch (component->insertionPolicy())
			{
				case (Model::NumberPolicy):
					msg.print("%5i %-15s %-10s %-6i           --   %5.3f\n", cycle, component->modelName(), component->partitionName(), component->nAdded(), component->scaleFactor());
					break;
			}
		}
	}
	
	// Done - Copy component model contents across to targetModel_
	for (component = components_.first(); component != NULL; component = component->next) component->copyTo(targetModel_);
	
}

