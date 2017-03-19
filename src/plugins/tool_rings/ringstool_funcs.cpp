/*
        *** Rings Tool Functions
        *** src/plugins/tool_rings/ringstool_funcs.cpp
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

#include "plugins/tool_rings/ringstool.hui"
#include "plugins/tool_rings/ringstooldialog.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "base/pattern.h"
#include "base/ring.h"

// Constructor
RingsToolPlugin::RingsToolPlugin()
{
	// Setup plugin options
	pluginOptions_.add("element", "Si");
	pluginOptions_.add("linkElement", "O");
	pluginOptions_.add("links", "true");
	pluginOptions_.add("calculateForAll", "false");
	pluginOptions_.add("maxRingSize", "10");
	pluginOptions_.add("maxDistance", "8.0");

	// Create dialog if the tool has one
	if (hasDialog()) dialog_ = new RingsToolDialog(*this, pluginOptions_);
	else dialog_ = NULL;
}

// Destructor
RingsToolPlugin::~RingsToolPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* RingsToolPlugin::makeCopy() const
{
	return new RingsToolPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType RingsToolPlugin::type() const
{
	return PluginTypes::ToolPlugin;
}

// Return category of plugin
int RingsToolPlugin::category() const
{
	return PluginTypes::GeneralToolPlugin;
}

// Name of plugin
QString RingsToolPlugin::name() const
{
	return QString("SP Rings Tool Plugin");
}

// Nickname of plugin
QString RingsToolPlugin::nickname() const
{
	return QString("ringstool");
}

// Return whether the plugin is enabled
bool RingsToolPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString RingsToolPlugin::description() const
{
	return QString("Calculates shortest-path ring statistics in amorphous networks (Phys. Rev. B 1991, 44, 4925)");
}

/*
 * Tool Definition
 */

// Return button label to use in GUI
QString RingsToolPlugin::buttonLabel() const
{
	return QString("Rings");
}

// Return icon for button in GUI
QIcon RingsToolPlugin::buttonIcon() const
{
	return QIcon(":/ringstool_icons/icon.svg");
}

// Return group name for tool (used to group similar tools together)
QString RingsToolPlugin::groupName() const
{
	return QString("Glasses");
}

// Return whether the tool is enabled (appears in the GUI)
bool RingsToolPlugin::isEnabled() const
{
	return true;
}

// Return whether the tool has a dialog
bool RingsToolPlugin::hasDialog() const
{
	return true;
}

// Show the dialog for the tool
void RingsToolPlugin::showDialog()
{
	// Check if a dialog actually exists
	if (dialog_ == NULL)
	{
		Messenger::error("No dialog is associated to the tool '%s'\n", qPrintable(name()));
		return;
	}

	// Cast the dialog_ pointer into our custom class
	RingsToolDialog* testToolDialog = (RingsToolDialog*) dialog_;
	if (!testToolDialog)
	{
		Messenger::error("Error casting tool dialog into custom class for the tool '%s'\n", qPrintable(name()));
	}
	testToolDialog->applyPluginOptions();
	testToolDialog->exec();
}

// Run the tool with the current settings
bool RingsToolPlugin::runTool()
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
			printf("NBONDS = %i\n", workingModel.nBonds());

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
		 * Since 2) is the most demanding step, here we will use a spherical distance cutoff in order to select only the atoms likely to be involved in the rings we are interested in.
		 * By doing this we limit the number of atoms we're dealing with, can form a full Pattern/Connectivity Matrix, and use the unimodal labelling property to check for SP rings.
		 */

		// OK, here we go
		Model tempModel;
		tempModel.cell().copy(&workingModel.cell());
		while (Atom* z = workingModel.atoms())
		{
			printf("Atoms remaining... %i\n", workingModel.nAtoms());
			// Select all atoms within maxDistance of z
			workingModel.selectNone();
			workingModel.selectRadial(z, maxDistance);
			clip.copySelection(&workingModel, true);

			// In another temporary model, paste these atoms
			tempModel.clearAtoms();
			clip.pasteToModel(&tempModel, false);

			// Perform a tree selection from the first atom (z), and remove any that don't get selected (since they won't be part of any rings)
			tempModel.selectTree(tempModel.atoms());
			tempModel.selectionInvert();
			tempModel.selectionDelete();

			// Can now create a full pattern, with rings and connectivity matrix
			Pattern pattern;
			pattern.setParent(&tempModel);
			pattern.initialise(0, 0, 1, tempModel.nAtoms());
			pattern.createMatrices(true);
			pattern.findRingsFrom(0, maxRingSize, -1);

			// Go through the rings we've found and check for SP criteria
			// We can get away with using the actual indicies of the atoms, since we copied / pasted and started from atom zero.
			// Connectivity pattern must be as follows for an SP ring:
			// For an even ring of size N:  0, 1, ..., N-1, N, N-1, ..., 1, (0)
			// For an odd ring of size N:   0, 1, ..., N-1, N, N, N-1, ..., 1, (0)
			for (Ring* ring = pattern.rings(); ring != NULL; ring = ring->next)
			{
				// 'Index' will contain the distance connectivity we currently expect from the atom in the ring
				int n = 1, index = 1;
// 				QString s, t;
// 				s.sprintf("%2i  ", ring->nAtoms());
				for (RefListItem<Atom,int>* ri = ring->atoms()->next; ri != NULL; ri = ri->next, ++n)
				{
					// Check index
					if (index != pattern.connectivity(0, ri->item->id())) break;

// 					s += QString::number(pattern.connectivity(0, ri->item->id()));
// 					t += QString::number(index);
					// If not yet halfway, add one to index
					if (n < ring->nAtoms()/2) ++index;
					if ((n == ring->nAtoms()/2) && (ring->nAtoms()%2 == 0)) --index;
					else if (n > ring->nAtoms()/2) --index;
				}
// 				printf("RING = %s (%s)\n", qPrintable(s), qPrintable(t));

				if (n == ring->nAtoms()) printf("Accepted this ring\n.");
			}
			

			// Remove atom z from the working model
			workingModel.deleteAtom(workingModel.atoms());
			break;
		}
	}

	// Update the display
	emit(updateWidgets(0));

	return true;
}

/*
 * QObject / Signals
 */

// Return interface as QObject
QObject* RingsToolPlugin::object()
{
	return this;
}
