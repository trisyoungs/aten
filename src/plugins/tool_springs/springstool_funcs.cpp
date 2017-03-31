/*
        *** SPRings Tool Functions
        *** src/plugins/tool_springs/springstool_funcs.cpp
        Copyright T. Youngs 2016-2017

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

#include "plugins/tool_springs/springstool.hui"
#include "plugins/tool_springs/springstooldialog.h"
#include "gui/qcustomplot/qcustomplot.hui"
#include "model/model.h"
#include "model/clipboard.h"
#include "base/pattern.h"
#include "base/ring.h"

// Constructor
SPRingsToolPlugin::SPRingsToolPlugin()
{
	// Setup plugin options
	pluginOptions_.add("element", "Si");
	pluginOptions_.add("linkElement", "O");
	pluginOptions_.add("links", "true");
	pluginOptions_.add("calculateForAll", "false");
	pluginOptions_.add("maxRingSize", "10");
	pluginOptions_.add("maxDistance", "8.0");
	pluginOptions_.add("debug", "false");

	// Create dialog if the tool has one
	if (hasDialog()) dialog_ = new SPRingsToolDialog(*this, pluginOptions_);
	else dialog_ = NULL;
}

// Destructor
SPRingsToolPlugin::~SPRingsToolPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* SPRingsToolPlugin::makeCopy() const
{
	return new SPRingsToolPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType SPRingsToolPlugin::type() const
{
	return PluginTypes::ToolPlugin;
}

// Return category of plugin
int SPRingsToolPlugin::category() const
{
	return PluginTypes::GeneralToolPlugin;
}

// Name of plugin
QString SPRingsToolPlugin::name() const
{
	return QString("SP Rings Tool Plugin");
}

// Nickname of plugin
QString SPRingsToolPlugin::nickname() const
{
	return QString("springs");
}

// Return whether the plugin is enabled
bool SPRingsToolPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString SPRingsToolPlugin::description() const
{
	return QString("Calculates shortest-path ring statistics in amorphous networks (Phys. Rev. B 1991, 44, 4925)");
}

/*
 * Tool Definition
 */

// Return button label to use in GUI
QString SPRingsToolPlugin::buttonLabel() const
{
	return QString("SPRings");
}

// Return icon for button in GUI
QIcon SPRingsToolPlugin::buttonIcon() const
{
	return QIcon(":/springstool_icons/icon.svg");
}

// Return group name for tool (used to group similar tools together)
QString SPRingsToolPlugin::groupName() const
{
	return QString("Glasses");
}

// Return whether the tool is enabled (appears in the GUI)
bool SPRingsToolPlugin::isEnabled() const
{
	return true;
}

// Return whether the tool has a dialog
bool SPRingsToolPlugin::hasDialog() const
{
	return true;
}

// Show the dialog for the tool
bool SPRingsToolPlugin::showDialog()
{
	// Check if a dialog actually exists
	if (dialog_ == NULL)
	{
		Messenger::error("No dialog is associated to the tool '%s'\n", qPrintable(name()));
		return false;
	}

	// Cast the dialog_ pointer into our custom class
	SPRingsToolDialog* testToolDialog = (SPRingsToolDialog*) dialog_;
	if (!testToolDialog)
	{
		Messenger::error("Error casting tool dialog into custom class for the tool '%s'\n", qPrintable(name()));
		return false;
	}
	testToolDialog->applyPluginOptions();
	testToolDialog->exec();
	
	return true;
}

// Run the tool with the current settings
bool SPRingsToolPlugin::runTool()
{
	// Get the target models
	RefList<Model,bool> targets;
	if (pluginOptions_.value("applyToAll") == "true") targets = allModels();
	else targets.add(currentModelOrFrame());

	bool links = pluginOptions_.value("links") == "true";
	int linkEl = ElementMap::find(pluginOptions_.value("linkElement"));
	int el = ElementMap::find(pluginOptions_.value("element"));
	int maxRingSize = pluginOptions_.value("maxRingSize").toInt();
	double maxDistance = pluginOptions_.value("maxDistance").toDouble();
	bool debug = pluginOptions_.value("debug") == "true";

	// Grab and cast the dialog_ pointer into a SPRingsToolDialog so we can access the TPlotWidget
	SPRingsToolDialog* springsToolDialog = (SPRingsToolDialog*) dialog_;

	// Loop over targets
	for (RefListItem<Model,bool>* ri = targets.first(); ri != NULL; ri = ri->next)
	{
		Model* sourceModel = ri->item;
		if (!sourceModel) continue;

		// Select all atoms of the type 'el' and 'linkEl' (if links are enabled) and copy to a working model
		sourceModel->selectNone(true);
		sourceModel->selectElement(el, true);
		if (links) sourceModel->selectElement(linkEl, true);
		Clipboard clip;
		clip.copyMarked(sourceModel);
		Model workingModel;
		workingModel.cell().copy(&sourceModel->cell());
		clip.pasteToModel(&workingModel, false);

		// Now have a model with the el and, possible, linkEl elements in it.
		// Do the linking of elements first
		if (links)
		{
			for (Atom* i = workingModel.atoms(); i != NULL; i = i->next)
			{
				if (i->element() != linkEl) continue;

				// Double loop over bonds
				for (RefListItem<Bond,int>* ri = i->bonds(); ri != NULL; ri = ri->next)
				{
					Atom* j = ri->item->partner(i);
					if (j->element() != el) continue;

					for (RefListItem<Bond,int>* rj = ri->next; rj != NULL; rj = rj->next)
					{
						Atom* k = rj->item->partner(i);
						if (k->element() != el) continue;

						// Create new bond between atoms j and k, since they are bridged by i
						workingModel.bondAtoms(j, k, Bond::Single);
					}
				}
			}
			if (debug) Messenger::print("Number of bonds in model after linking atoms = %i\n", workingModel.nBonds());

			// Finally, select and delete all linkEl
			workingModel.selectNone();
			workingModel.selectElement(linkEl);
			workingModel.selectionDelete();
		}

		/*
		 * Algorithm as suggested in Phys. Rev. B 1991, 44, 4925, section VI
		 *	1) Select an arbitrary vertex (atom) z
		 *	2) Generate each ring containing z, of max length m, and test if it is an SP ring
		 * 	3) Delete z
		 * 	4) Repeat 1) - 3) until no vertices remain.
		 *
		 * Since 2) is the most demanding step, here we will use a spherical distance cutoff in order to select only the atoms likely to be involved in the springs we are interested in.
		 * By doing this we limit the number of atoms we're dealing with, can form a full Pattern/Connectivity Matrix, and use the unimodal labelling property to check for SP springs.
		 */

		int nOk, ringSize, diameter, scope;
		bool odd;
		QString ringAtomInfo;

		// OK, here we go
		Model tempModel;
		tempModel.cell().copy(&workingModel.cell());
		PlotData spSPRings(sourceModel->name());
		spSPRings.x().resize(maxRingSize);
		spSPRings.y().resize(maxRingSize);
		for (int n = 0; n<maxRingSize; ++n) spSPRings.x()[n] = n+1;
		while (Atom* z = workingModel.atoms())
		{
			if (debug) Messenger::print("There are %i atoms remaining...\n", workingModel.nAtoms());
			// Select all atoms within maxDistance of z
			workingModel.selectNone();
			workingModel.selectRadial(z, maxDistance);
			clip.copySelection(&workingModel, true);

			// In another temporary model, paste these atoms
			tempModel.clearAtoms();
			clip.pasteToModel(&tempModel, false);

			// Perform a tree selection from the first atom (z), and remove any that don't get selected (since they won't be part of any springs)
			tempModel.selectTree(tempModel.atoms());
			tempModel.selectionInvert();
			tempModel.selectionDelete();

			// Can now create a full pattern, with springs and connectivity matrix
			Pattern pattern;
			pattern.setParent(&tempModel);
			pattern.initialise(0, 0, 1, tempModel.nAtoms());
			pattern.createMatrices(true, true);
			pattern.findRingsFrom(0, maxRingSize, -1);

			/*
			 * Go through the springs we've found and check for SP criteria
			 * We can get away with using the actual indicies of the atoms, since we copied / pasted and started from atom zero.
			 * Connectivity pattern must be as follows for an SP ring:
			 * For an even ring of size N:  0, 1, ..., N-1, N, N-1, ..., 1, (0)
			 * For an odd ring of size N:   0, 1, ..., N-1, N, N, N-1, ..., 1, (0)
			 *
			 * There is a subtlety here - while the definition of the shortest-path ring is independent of the atom order,
			 * it is necessary to consider the connectivity pattern from all atoms involved in the ring. Otherwise, using
			 * Figure 1 in the paper as an example, the ring 'bcfgdb' will be wrongly detected as a SP ring, since the algorithm
			 * as written will begin on atom 'b', and from that viewpoint the connectivity pattern conforms to the criteria
			 * above. An efficient way of evaluating SP rings is given in section VI.C of the paper, and considers the antipodes
			 * of the ring. For each pair of antipodes, the connectivity distance must be the diameter (largest possible connectivity
			 * distance) of the ring, otherwise it is not an SP ring.
			 */

			for (Ring* ring = pattern.rings(); ring != NULL; ring = ring->next)
			{
// 				// 'Index' will contain the distance connectivity we currently expect from the atom in the ring
// 				int n = 1, index = 1;
// 				QString s, t;
// 				if (debug) s.sprintf("%2i  ", ring->nAtoms());
// 				for (RefListItem<Atom,int>* ri = ring->atoms()->next; ri != NULL; ri = ri->next, ++n)
// 				{
// 					// Check index
// 					if (index != pattern.connectivity(0, ri->item->id())) break;
// 
// 					if (debug) s += QString::number(pattern.connectivity(0, ri->item->id()));
// 					if (debug) t += QString::number(index);
// 
// 					// If not yet halfway, add one to index
// 					if (n < ring->nAtoms()/2) ++index;
// 					if ((n == ring->nAtoms()/2) && (ring->nAtoms()%2 == 0)) --index;
// 					else if (n > ring->nAtoms()/2) --index;
// 				}
// 				if (debug) Messenger::print("Checking ring %s (%s)\n", qPrintable(s), qPrintable(t));

				// Grab some properties about the current ring
				odd = ring->nAtoms()%2 != 0;
				ringSize = ring->nAtoms();
				diameter = ringSize / 2;
				scope = diameter + (odd ? 1 : 0);

				// Construct some debug output
				if (debug)
				{
					ringAtomInfo.clear();
					for (RefListItem<Atom,int>* ri = ring->atoms(); ri != NULL; ri = ri->next)
					{
						if (ri != ring->atoms()) ringAtomInfo += "-";
						ringAtomInfo += QString::number(ri->item->id() + 1);
					}
					Messenger::print(QString("Checking ring of size %1 with temporary atom indices %2").arg(ringSize).arg(ringAtomInfo));
				}

				// Loop over half the atoms in the ring (no need to check each pair twice)
				RefListItem<Atom,int>** ringItems = ring->atomArray();
				if (debug) Messenger::print("  -- Ring is %s, diameter is %i", odd ? "odd" : "even", diameter);
				nOk = 0;
				for (int n = 0; n < scope; ++n)
				{
					if (debug) Messenger::print("    Checking atom ids %i and %i, connectivity distance is %i", ringItems[n]->item->id()+1, ringItems[(n+diameter)%ringSize]->item->id()+1, pattern.connectivity(ringItems[n]->item->id(), ringItems[(n+diameter)%ringSize]->item->id()));
					if (pattern.connectivity(ringItems[n]->item->id(), ringItems[(n+diameter)%ringSize]->item->id()) == diameter) ++nOk;
					else break;

					if (odd)
					{
						if (debug) Messenger::print("    Checking atom ids %i and %i, connectivity distance is %i (odd)", ringItems[n]->item->id()+1, ringItems[(n+1+diameter)%ringSize]->item->id()+1, pattern.connectivity(ringItems[n]->item->id(), ringItems[(n+1+diameter)%ringSize]->item->id()));
						if (pattern.connectivity(ringItems[n]->item->id(), ringItems[(n+1+diameter)%ringSize]->item->id()) == diameter) ++nOk;
						else break;
					}
				}

				/*
				 * Check how many antipodal pairs we matched with the correct diameter.
				 * For an ring with an even number of atoms, nOk should be equal to the diameter.
				 * For an odd ring, nOk should be equal to diameter*2 (as the loop above is written)
				 */
				if (!odd) nOk *= 2;
				if (nOk == (scope*2))
				{
					spSPRings.y()[ringSize-1] += 1.0;

					if (debug) Messenger::print("  ACCEPTED as it is a SP ring.");
				}
				else if (debug) Messenger::print("  REJECTED.");
			}
			

			// Remove atom z from the working model
			workingModel.deleteAtom(workingModel.atoms());
		}

		// Add our new data to the plot in the UI
		springsToolDialog->ui.PlotWidget->addBarsData(spSPRings, true);
	}

	// Rescale axes on plot widget
	springsToolDialog->ui.PlotWidget->plot()->xAxis->rescale();
	springsToolDialog->ui.PlotWidget->plot()->yAxis->rescale();

	// Update the display
	emit(updateWidgets(0));

	return true;
}

/*
 * QObject / Signals
 */

// Return interface as QObject
QObject* SPRingsToolPlugin::object()
{
	return this;
}
