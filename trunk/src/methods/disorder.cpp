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
#include "base/progress.h"

// Disorder Builder
bool MonteCarlo::disorder(Model *destmodel, PartitioningScheme *scheme, bool fixedCell)
{
	msg.enter("MonteCarlo::disorder");
	DisorderData *component, *other;
	Refitem<DisorderData,int> *ri;
	PartitionData *pd;
	Atom *i;
	Dnchar cycleText;
	int n, id, cycle, nSatisfied, nInsertions, nRelative = 0;
	Vec3<double> r;
	UnitCell *cell;
	double delta, firstRelative, firstActual, expectedPop;
	bool isRelative;
	
	// The accuracy parameter determines what percentage error we will allow in actual vs requested densities
	double accuracy = 0.01;
	// The maxfailures limit is the number of successive failed insertions we allow before we reduce the scale factor
	int maxFailures = 5;
	// The reductionfactor is the factor by which we multiply scale factors after reaching the limit of unsuccessful insertions
	double reductionFactor = 0.95;
	// Number of tweaks to attempt, per component, per cycle
	int nTweaks = 0;
	// Maximum distance to translate molecule in tweak
	double deltaDistance = 1.0;
	// Maximum angle (each around X and Y) to rotate molecule in tweak
	double deltaAngle = 20.0;
	
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
	scheme->clearComponentLists();

	// Step 2 - Construct list of components with partition references, taking copies of all component models
	// List all models with RelativePolicy at the start of the list, and store a reflist of the components in the original model order
	// Also, build up reverse list of components associated to partitions
	Model *targetModel_ = destmodel;
	cell = targetModel_->cell();
	List<DisorderData> components_;
	Reflist<DisorderData, int> componentsOrder_;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		if (m->componentInsertionPolicy() == Model::NoPolicy) continue;
		// Check that we can get the requested partition
		int id = m->componentPartition();
		if ((id < 0) || (id >= scheme->nPartitions()))
		{
			msg.print("Error: Model '%s' targets partition id %i, but it does not exist in the scheme '%s'.\n", m->name(), id+1, scheme->name());
			msg.exit("MonteCarlo::disorder");
			return FALSE;
		}
		// Do some quick checks on the component to see if its valid
		switch (m->componentInsertionPolicy())
		{
			case (Model::NumberPolicy):
				if (m->componentPopulation() == 0) msg.print("Warning: Population for component '%s' is set to zero.\n", m->name());
				break;
			case (Model::DensityPolicy):
				if (m->componentDensity() < 0.01) msg.print("Warning: Density for component '%s' is too low (or zero) (%f).\n", m->name(), m->componentDensity());
				break;
			case (Model::NumberAndDensityPolicy):
				if (m->componentPopulation() == 0) msg.print("Warning: Population for component '%s' is set to zero.\n", m->name());
				if (m->componentDensity() < 0.01)
				{
					msg.print("Error: Density for component '%s' is too low (or zero) (%f).\n", m->name(), m->componentDensity());
					msg.exit("MonteCarlo::disorder");
					return FALSE;
				}
				break;
			case (Model::RelativePolicy):
				if (m->componentPopulation() == 0)
				{
					msg.print("Error: Population must be specified for component '%s' since it's policy is 'relative'.\n", m->name());
					msg.exit("MonteCarlo::disorder");
					return FALSE;
				}
				if (m->componentDensity() < 0.01)
				{
					msg.print("Error: Density for component '%s' is too low (or zero) (%f).\n", m->name(), m->componentDensity());
					msg.exit("MonteCarlo::disorder");
					return FALSE;
				}
				break;
		}
		component = (m->componentInsertionPolicy() == Model::RelativePolicy ? components_.insert(NULL) : components_.add());
		msg.print("Initialising component model '%s' for partition '%s'...\n", m->name(), scheme->partitionName(id));
		if (!component->initialise(m, scheme->partition(id)))
		{
			msg.exit("MonteCarlo::disorder");
			return FALSE;
		}
		componentsOrder_.add(component);
		// Add component to partition's own list
		scheme->partition(id)->addComponent(component);
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
		msg.print("%2i %8s\t%10.2f\t%8.5f\n", pd->id(), pd->name(), pd->volume(), pd->density()); 
	}
	
	// All set up and ready - do the build
	int pid = progress.initialise("Performing Disorder build", -1, !gui.exists());
	msg.print("Cycle  Component    Region    Population (Requested)  Density (Requested)  RSF\n");
	for (cycle = 1; cycle <= 100000; ++cycle)
	{
		// Each cycle will consist of one round of insertions and deletions, and one round of MC shaking (tweaking)
		
		/*
		// Insertions / Deletions
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
				// For density (and relative) policy, check the current density of the component's target partition
				case (Model::DensityPolicy):
					delta = fabs(1.0 - component->partitionDensity() / component->requestedDensity());
					if (delta < accuracy)
					{
						nSatisfied++;
						continue;
					}
					break;
				// For number and density, we obviously require both to be satisfied
				case (Model::NumberAndDensityPolicy):
// 					delta = fabs(1.0 - component->partitionDensity() / component->requestedDensity());
					if (component->requestedPopulation() == component->nAdded())
					{
						nSatisfied++;
						continue;
					}
					break;
				// For the relative policy, the requested densities and the relative populations are important
				case (Model::RelativePolicy):
// 					if (component->nAdded() == 0) break;
					delta = fabs(1.0 - component->partitionDensity() / component->requestedDensity());
					// Calculate raltive population ratio with the first relative component
					firstRelative = components_.first()->requestedPopulation();
					firstActual = components_.first()->nAdded();
					expectedPop = (component->requestedPopulation() / firstRelative) * firstActual;
					if (component->nAdded() == 0) isRelative = TRUE;
					else isRelative = fabs(1.0 - expectedPop / component->nAdded()) < accuracy;
// 					printf("Population relative expected = %f, actual = %i, delta = %f\n", expectedPop, component->nAdded(), fabs(1.0 - expectedPop / component->nAdded()));
					if ((delta < accuracy) && isRelative)
					{
						nSatisfied++;
						continue;
					}
					break;
			}
			
			// Termination condition for this component not satisfied, so more molecules needed!
			if (component->insertionPolicy() == Model::RelativePolicy)
			{
				// If this relative component is already relative to the first in the list, the it is just a case of adding some more
				if (isRelative) nInsertions = component->requestedPopulation();
				else
				{
					// Depending on the expected population, we either insert or delete
					nInsertions = expectedPop - component->nAdded();
				}
			}
			else nInsertions = 1;
// 			printf("nInsertions for component %s is %i\n", component->modelName(), nInsertions);

			// Prepare/select nTrial candidates and do some test insertions or deletions
			if (nInsertions == 0) continue;
			else if (nInsertions > 0) for (n=0; n<nInsertions; ++n)
			{
				/*
				// Insertion
				*/
// 				printf("Insertion %i on component %p\n", n, component);
				component->prepareCandidate(volumeElement);
				// Test component against its own population...
				if (component->selfOverlaps(cell)) component->rejectCandidate();
				else if (component->modelOverlaps(targetModel_, cell)) component->rejectCandidate();
				else if (component->otherOverlaps(components_.first(), cell)) component->rejectCandidate();
				else
				{
					// Happy days! This position is fine, so insert the molecule and continue the loop
					component->acceptCandidate();
					continue;
				}
				
				// We are here, so we failed again. Have we reached our successive failures limit?
				if (component->nFailed() >= maxFailures) component->adjustScaleFactor(reductionFactor, 1.0);
			}
			else for (n=nInsertions; n < 0; ++n)
			{
				/*
				// Deletions
				*/
// 				printf("Deletion %i on component %p\n", n, component);
// 				if (AtenMath::random() < 0.90) continue;
				if (!component->selectCandidate()) break;
				component->deleteCandidate();
			}
		}

		/*
		// Tweaks
		*/
		for (component = components_.first(); component != NULL; component = component->next)
		{
			for (n = 0; n < nTweaks; ++n)
			{
				// Select a candidate molecule, tweak it, and do a test insertion
// 				printf("Tweak %i on component %s\n", n, component->modelName());
				if (!component->selectCandidate()) break;
				component->tweakCandidate(deltaDistance, deltaAngle);
				// Test component against its own population...
				if (component->selfOverlaps(cell) > 0.0) continue;
				else if (component->modelOverlaps(targetModel_, cell)) continue;
				else if (component->otherOverlaps(components_.first(), cell)) continue;
				else
				{
					// Happy days! This position is fine, so store new coordinates and continue the loop
					component->acceptCandidate();
					continue;
				}
			}
		}
		
		// Check densities of all components - if higher than requested, delete some molecules randomly from all components targetting the same partition
		for (component = components_.first(); component != NULL; component = component->next)
		{
			if (component->requestedDensity() > component->partitionDensity()) continue;
			if (component->insertionPolicy() == Model::NumberPolicy) continue;
			// Uh-oh - density is higher. Let's delete stuff...
// 			printf("Density in region '%s' is higher than requested...\n", component->partitionName());
// 			while (fabs(1.0 - component->partitionDensity() / component->requestedDensity()) > accuracy)
			while (component->partitionDensity() > component->requestedDensity())
			{
				// Pick a random component from the partition's list
				pd = component->partition();
				id = AtenMath::randomi(pd->nComponents());
				other = pd->component(id);
				if (other == NULL) printf("Baaaaaad error.\n");
				else
				{
					if (!other->selectCandidate()) break;
					other->deleteCandidate();
				}
			}
		}
		
		/*
		// Cycle Summary
		*/
		if ((cycle-1)%100 == 0)
		{
			cycleText.sprintf("%-5i", cycle);
			for (component = components_.first(); component != NULL; component = component->next)
			{
				switch (component->insertionPolicy())
				{
					case (Model::NumberPolicy):
						msg.print("%-5s   %-15s %-10s  %-5i (%-5i)  %8.5f  (   N/A  )  %5.3f\n", cycleText.get(), component->modelName(), component->partitionName(), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->scaleFactor());
						break;
					case (Model::DensityPolicy):
						msg.print("%-5s   %-15s %-10s  %-5i ( N/A )  %8.5f  (%8.5f)  %5.3f\n", cycleText.get(), component->modelName(), component->partitionName(), component->nAdded(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor());
						break;
					case (Model::NumberAndDensityPolicy):
						msg.print("%-5s   %-15s %-10s  %-5i (%-5i)  %8.5f  (%8.5f)  %5.3f\n", cycleText.get(), component->modelName(), component->partitionName(), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor());
						break;
					case (Model::RelativePolicy):
						msg.print("%-5s   %-15s %-10s  %-5i (R %-3i)  %8.5f  (%8.5f)  %5.3f\n", cycleText.get(), component->modelName(), component->partitionName(), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor());
						break;
				}
				cycleText.clear();
			}
		}
		
		// Finished?
		if (nSatisfied == components_.nItems())
		{
			msg.print("Done.\n");
			break;
		}

		gui.processMessages();
		progress.update(pid);
	}
	progress.terminate(pid);

	// Copy component model contents across to targetModel_, in the order they were originally listed
	targetModel_->beginUndoState("Disorder build");
	for (ri = componentsOrder_.first(); ri != NULL; ri = ri->next) ri->item->copyTo(targetModel_);
	targetModel_->endUndoState();

	msg.enter("MonteCarlo::disorder");
	return TRUE;
}

