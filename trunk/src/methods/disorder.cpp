/*
	*** Disorder Builder
	*** src/methods/disorder.cpp
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

#include "main/aten.h"
#include "methods/mc.h"
#include "model/model.h"
#include "methods/disorderdata.h"
#include "methods/partitiondata.h"
#include "model/clipboard.h"
#include "base/sysfunc.h"
#include "base/progress.h"
#include "base/pattern.h"

ATEN_USING_NAMESPACE

// Disorder Builder
bool MonteCarlo::disorder(Model* allModels, Model* destmodel, PartitioningScheme* scheme, bool fixedCell)
{
	Messenger::enter("MonteCarlo::disorder");
	DisorderData *component, *other;
	Refitem<DisorderData,int>* ri;
	PartitionData* pd;
	Atom* i;
	int n, m, id, cycle, nSatisfied, nInsertions, nRelative = 0, totalToAdd = 0;
	Vec3<double> r;
	double delta, firstRelative, firstActual, expectedPop;
	bool isRelative;
	
	// Step 1 - Update cell lists in scheme (if required)
	if (scheme == NULL)
	{
		Messenger::print("Error: NULL scheme pointer given to disorder builder.");
		Messenger::exit("MonteCarlo::disorder");
		return false;
	}
	// If we are using the default 'cell' scheme, then no need to generate a fine mesh
	if (!scheme->staticData())
	{
		scheme->setGridSize(prefs.partitionGridSize());
		scheme->recalculatePartitions();
	}
	scheme->clearComponentLists();

	// Step 2 - Construct list of components with partition references, taking copies of all component models
	// List all models with RelativePolicy at the start of the list, and store a reflist of the components in the original model order
	// Also, build up reverse list of components associated to partitions
	Model* targetModel_ = destmodel;
	UnitCell& cell = targetModel_->cell();
	List<DisorderData> components_;
	Reflist<DisorderData, int> componentsOrder_;
	for (Model* m = allModels; m != NULL; m = m->next)
	{
		if (m->componentInsertionPolicy() == Model::NoPolicy) continue;
		// Check that we can get the requested partition
		int id = m->componentPartition();

		if ((id < 0) || (id >= scheme->nPartitions()))
		{
			Messenger::print("Error: Model '%s' targets partition id %i, but it does not exist in the scheme '%s'.", qPrintable(m->name()), id+1, qPrintable(scheme->name()));
			Messenger::exit("MonteCarlo::disorder");
			return false;
		}
		// Do some quick checks on the component to see if its valid
		switch (m->componentInsertionPolicy())
		{
			case (Model::NumberPolicy):
				if (m->componentPopulation() == 0) Messenger::print("Warning: Population for component '%s' is set to zero.", qPrintable(m->name()));
				totalToAdd += m->componentPopulation();
				break;
			case (Model::DensityPolicy):
				if (m->componentDensity() < 0.01) Messenger::print("Warning: Density for component '%s' is too low (or zero) (%f).", qPrintable(m->name()), m->componentDensity());
				break;
			case (Model::NumberAndDensityPolicy):
				if (m->componentPopulation() == 0) Messenger::print("Warning: Population for component '%s' is set to zero.", qPrintable(m->name()));
				if (m->componentDensity() < 0.01)
				{
					Messenger::print("Error: Density for component '%s' is too low (or zero) (%f).", qPrintable(m->name()), m->componentDensity());
					Messenger::exit("MonteCarlo::disorder");
					return false;
				}
				totalToAdd += m->componentPopulation();
				break;
			case (Model::RelativePolicy):
				if (m->componentPopulation() == 0)
				{
					Messenger::print("Error: Population must be specified for component '%s' since it's policy is 'relative'.", qPrintable(m->name()));
					Messenger::exit("MonteCarlo::disorder");
					return false;
				}
				if (m->componentDensity() < 0.01)
				{
					Messenger::print("Error: Density for component '%s' is too low (or zero) (%f).", qPrintable(m->name()), m->componentDensity());
					Messenger::exit("MonteCarlo::disorder");
					return false;
				}
				break;
		}
		component = (m->componentInsertionPolicy() == Model::RelativePolicy ? components_.prepend() : components_.add());
		Messenger::print("Initialising component model '%s' for partition '%s'...", qPrintable(m->name()), qPrintable(scheme->partitionName(id)));
		if (!component->initialise(m, scheme->partition(id)))
		{
			Messenger::exit("MonteCarlo::disorder");
			return false;
		}
		componentsOrder_.add(component);
		// Add component to partition's own list
		scheme->partition(id)->addComponent(component);
	}
	if (components_.nItems() == 0)
	{
		Messenger::print("Error: No component models selected for disorder builder.");
		Messenger::exit("MonteCarlo::disorder");
		return false;
	}
	
	// Step 3 - Determine cell size (if applicable) and perform some sanity checks
	if (!fixedCell)
	{
		Messenger::print("Absolute cell volume is not defined.");
		// If we have a free or as-yet-undefined cell, the existing model MUST NOT contain any existing atoms
		if (targetModel_->nAtoms() != 0)
		{
			Messenger::print("Error: For disorder building with an unspecified cell size the cell cannot contain any existing atoms.");
			Messenger::exit("MonteCarlo::disorder");
			return false;
		}
		
		// Make double sure that we actually have a unit cell specified at this point
		if (targetModel_->cell().type() == UnitCell::NoCell)
		{
			Messenger::print("Error: No unit cell defined. Create a simple unit cell - the lengths will be adjusted automatically by the builder.");
			Messenger::exit("MonteCarlo::disorder");
			return false;
		}
		
		// So, we must decide on the final volume of the cell - this means that both a population and a density for each 
		// component must have been specified - i.e. no 'bulk' components and no 'free' densities
		Messenger::print("Calculating cell volume based on component information...");
		double totalVolume = 0.0;
		for (component = components_.first(); component != NULL; component = component->next)
		{
			if (component->insertionPolicy() == Model::NoPolicy) continue;
			if (component->insertionPolicy() != Model::NumberAndDensityPolicy)
			{
				Messenger::print("Error: For disorder building with an unspecified cell size every component must have a population and density specified with policy 'both' (component '%s' does not).", qPrintable(component->modelName()));
				Messenger::exit("MonteCarlo::disorder");
				return false;
			}
			// All ok, so add component to volume
			totalVolume += (component->requestedPopulation() * component->sourceModel().mass() / AVOGADRO) / (component->requestedDensity() * 1.0E-24);
		}
		Messenger::print("From the components specified, the new cell will have a volume of %f cubic Angstroms.", totalVolume);
		double factor = pow(totalVolume,1.0/3.0) / pow(targetModel_->cell().volume(),1.0/3.0);
		Matrix axes = targetModel_->cell().axes();
		axes.applyScaling(factor,factor,factor);
		targetModel_->cell().set(axes);
		Messenger::print("Based on original cell, scaling factor is %f, giving new a cell specification of:", factor);
		targetModel_->cell().print();
	}
	
	// Step 4 - Determine partition volumes and current partition densities (arising from existing model contents)
	Messenger::print("Determining partition volumes and starting densities...");
	Matrix volumeElement = targetModel_->cell().axes();
	Vec3<int> gridSize = scheme->gridSize();
	volumeElement.applyScaling(1.0/gridSize.x, 1.0/gridSize.y, 1.0/gridSize.z);
	double elementVolume = volumeElement.determinant();

	// Clear old mass data and reset volume
	for (pd = scheme->partitions(); pd != NULL; pd = pd->next)
	{
		pd->calculateVolume(elementVolume);
		pd->resetReducedMass();
	}
	
	// The target model may contain atoms already, so this must be subtracted from the relevant partitions
	Messenger::print("Determining current partition densities...");
	for (i = targetModel_->atoms(); i != NULL; i = i->next)
	{
		r = targetModel_->cell().fold(i->r());
		r = targetModel_->cell().realToFrac(r);
		id = scheme->partitionId(r.x, r.y, r.z);
		scheme->partition(id)->adjustReducedMass(i);
	}
	Messenger::print("Partition\t\tVolume\tDensity");
	for (pd = scheme->partitions(); pd != NULL; pd = pd->next)
	{
		Messenger::print("%2i %8s\t%10.2f\t%8.5f", pd->id()+1, qPrintable(pd->name()), pd->volume(), pd->density()); 
	}
	
	// All set up and ready - do the build
	int pid = progress.initialise("Performing Disorder build", disorderMaxCycles_);
	Messenger::print("Cycle   Component       Region      Pop   (Req)     Density  (Req)        RSF");
	for (cycle = 1; cycle <= disorderMaxCycles_; ++cycle)
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
					if (delta < disorderAccuracy_)
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
					if (component->nAdded() == 0) isRelative = true;
					else isRelative = fabs(1.0 - expectedPop / component->nAdded()) < disorderAccuracy_;
// 					printf("Population relative expected = %f, actual = %i, delta = %f\n", expectedPop, component->nAdded(), fabs(1.0 - expectedPop / component->nAdded()));
					if ((delta < disorderAccuracy_) && isRelative)
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
				if (component->nFailed() >= disorderMaxFailures_) component->adjustScaleFactor(disorderReductionFactor_, disorderMinimumScaleFactor_);
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
			for (n = 0; n < disorderNTweaks_; ++n)
			{
				// Select a candidate molecule, tweak it, and do a test insertion
// 				printf("Tweak %i on component %s\n", n, component->modelName());
				if (!component->selectCandidate()) break;
				component->tweakCandidate(disorderDeltaDistance_, disorderDeltaAngle_, scheme);
				// Test component against its own population...
				if (component->selfOverlaps(cell)) continue;
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
			if (component->insertionPolicy() == Model::NumberPolicy) continue;
			// Allow the actual density to vary by 'accuracy'
			if ((component->partitionDensity() / component->requestedDensity() - 1.0) < disorderAccuracy_) continue;
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
		if ((cycle%100 == 0) || (nSatisfied == components_.nItems()))
		{
			for (component = components_.first(); component != NULL; component = component->next)
			{
				switch (component->insertionPolicy())
				{
					case (Model::NumberPolicy):
						Messenger::print("%-5i   %-15s %-10s  %-5i (%-5i)  %8.5f  (   N/A  )  %5.3f", cycle, qPrintable(component->modelName()), qPrintable(component->partitionName()), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->scaleFactor());
						break;
					case (Model::DensityPolicy):
						Messenger::print("%-5i   %-15s %-10s  %-5i ( N/A )  %8.5f  (%8.5f)  %5.3f", cycle, qPrintable(component->modelName()), qPrintable(component->partitionName()), component->nAdded(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor());
						break;
					case (Model::NumberAndDensityPolicy):
						Messenger::print("%-5i   %-15s %-10s  %-5i (%-5i)  %8.5f  (%8.5f)  %5.3f", cycle, qPrintable(component->modelName()), qPrintable(component->partitionName()), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor());
						break;
					case (Model::RelativePolicy):
						Messenger::print("%-5i   %-15s %-10s  %-5i (R %-3i)  %8.5f  (%8.5f)  %5.3f", cycle, qPrintable(component->modelName()), qPrintable(component->partitionName()), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor());
						break;
				}
			}
		}
		
		// Finished?
		if (nSatisfied == components_.nItems())
		{
			Messenger::print("Done.");
			break;
		}

// 		gui.processMessages(); // ATEN2 TODO
		if (!progress.update(pid))
		{
			Messenger::print("Canceled.");
			break;
		}
	}
	progress.terminate(pid);
	
	// Perform a relaxation on the system by attempting to get the component scale ratios back as high as possible
	pid = progress.initialise("Performing Recovery", disorderRecoveryMaxCycles_);
	Messenger::print("Target scale factor is %f", disorderMaximumScaleFactor_);
	
	Messenger::print("Cycle   Component       Region      Pop   (Req)     Density  (Req)        RSF    Frac");
	for (cycle=1; cycle<=disorderRecoveryMaxCycles_; ++cycle)
	{
		nSatisfied = 0;
		for (component = components_.first(); component != NULL; component = component->next)
		{
			component->resetCount();
			// Check each molecule in turn for overlaps - if it does, tweak it...
			for (n = 0; n < component->nAdded(); ++n)
			{
				if (!component->selectCandidate(n)) continue;
				if (component->selfOverlaps(cell) || component->modelOverlaps(targetModel_, cell) || component->otherOverlaps(components_.first(), cell))
				{
					// Run a number of tweaks to try and remove overlaps
					for (m=0; m<disorderRecoveryMaxTweaks_; ++m)
					{
						component->tweakCandidate(disorderDeltaDistance_, disorderDeltaAngle_, scheme);
						// Test candidate molecule for overlaps
						if (component->selfOverlaps(cell)) continue;
						else if (component->modelOverlaps(targetModel_, cell)) continue;
						else if (component->otherOverlaps(components_.first(), cell)) continue;
						else
						{
							// Happy days! This position is fine, so store new coordinates and continue the loop
							component->acceptCandidate();
							component->increaseCount();
							break;
						}
					}
				}
				else component->increaseCount();
			}
// 			printf("Success count for component %s is %i\n", component->modelName(), count);
			if (double(component->count())/component->nAdded() >= disorderRecoveryThreshold_)
			{
				component->adjustScaleFactor(1.0+(1-disorderReductionFactor_), disorderMinimumScaleFactor_, disorderMaximumScaleFactor_);
				// Check for achievement of required scale factor.
				if (fabs(component->scaleFactor()-disorderMaximumScaleFactor_) < disorderAccuracy_) ++nSatisfied;
			}
		}
		
		/*
		// Cycle Summary
		*/
		if ((cycle%5 == 0) || (nSatisfied == components_.nItems()))
		{
			for (component = components_.first(); component != NULL; component = component->next)
			{
				switch (component->insertionPolicy())
				{
					case (Model::NumberPolicy):
						Messenger::print("%-5i   %-15s %-10s  %-5i (%-5i)  %8.5f  (   N/A  )  %5.3f  %5.3f", cycle, qPrintable(component->modelName()), qPrintable(component->partitionName()), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->scaleFactor(), double(component->count())/component->nAdded());
						break;
					case (Model::DensityPolicy):
						Messenger::print("%-5i   %-15s %-10s  %-5i ( N/A )  %8.5f  (%8.5f)  %5.3f  %5.3f", cycle, qPrintable(component->modelName()), qPrintable(component->partitionName()), component->nAdded(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor(), double(component->count())/component->nAdded());
						break;
					case (Model::NumberAndDensityPolicy):
						Messenger::print("%-5i   %-15s %-10s  %-5i (%-5i)  %8.5f  (%8.5f)  %5.3f  %5.3f", cycle, qPrintable(component->modelName()), qPrintable(component->partitionName()), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor(), double(component->count())/component->nAdded());
						break;
					case (Model::RelativePolicy):
						Messenger::print("%-5i   %-15s %-10s  %-5i (R %-3i)  %8.5f  (%8.5f)  %5.3f  %5.3f", cycle, qPrintable(component->modelName()), qPrintable(component->partitionName()), component->nAdded(), component->requestedPopulation(), component->partitionDensity(), component->requestedDensity(), component->scaleFactor(), double(component->count())/component->nAdded());
						break;
				}
			}
		}
		
		// Finished?
		if (nSatisfied == components_.nItems())
		{
			Messenger::print("Done.");
			break;
		}
		
// 		gui.processMessages(); ATEN2 TODO
		if (!progress.update(pid))
		{
			Messenger::print("Canceled.");
			break;
		}
	}
	progress.terminate(pid);
	
	// Copy component model contents across to targetModel_, in the order they were originally listed
	targetModel_->beginUndoState("Disorder build");
	for (ri = componentsOrder_.first(); ri != NULL; ri = ri->next) ri->item->copyTo(targetModel_);
	targetModel_->endUndoState();
	
	// Apply a suitable pattern definition to the system, retaining forcefield information from original models
	Messenger::print("Applying pattern definition to the new system...");
	targetModel_->clearPatterns();
	for (ri = componentsOrder_.first(); ri != NULL; ri = ri->next)
	{
		if (ri->item->nAdded() == 0)
		{
			Messenger::print(" -- No pattern added for insertion model '%s' since zero molecules were added.", qPrintable(ri->item->modelName()));
			continue;
		}
		Pattern* p = targetModel_->addPattern(ri->item->modelName(), ri->item->nAdded(), ri->item->sourceModel().nAtoms());
		p->setForcefield(ri->item->sourceModel().forcefield());
	}
	targetModel_->printPatterns();
	targetModel_->describeAtoms();

	Messenger::enter("MonteCarlo::disorder");
	return true;
}

