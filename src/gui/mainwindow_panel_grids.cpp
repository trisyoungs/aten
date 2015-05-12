/*
	*** Main Window - Grids Panel Functions
	*** src/gui/mainwindow_panel_grids.cpp
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

#include "gui/mainwindow.h"
#include "main/aten.h"
#include "templates/variantpointer.h"

// Update grids panel
void AtenWindow::updateGridsPanel(Model* sourceModel)
{
	Grid* currentGrid = aten_.current().g;

	ui.GridsList->clear();
	if (sourceModel)
	{
		// Update the list of grids
		for (Grid* g = sourceModel->grids(); g != NULL; g = g->next)
		{
			QListWidgetItem* item = new QListWidgetItem(g->name());
			item->setData(Qt::UserRole, VariantPointer<Grid>(g));
			if (g == currentGrid) item->setSelected(true);
			ui.GridsList->addItem(item);
		}

		// Check to see if a grid is selected - if not, try to select one...
		if (ui.GridsList->currentRow() == -1) ui.GridsList->setCurrentRow(0);
		if (ui.GridsList->currentRow() == -1) currentGrid = NULL;
		else currentGrid = (Grid*) VariantPointer<Grid>(ui.GridsList->currentItem()->data(Qt::UserRole));
	}

	updateGridInformation(currentGrid);
}

// Update grid information
void AtenWindow::updateGridInformation(Grid* sourceGrid)
{
	// Enable / disable controls
	ui.GridsPrimaryLowerCutoffSpin->setEnabled(sourceGrid);
	ui.GridsPrimaryUpperCutoffSpin->setEnabled(sourceGrid);
	ui.GridsPrimaryColourButton->setEnabled(sourceGrid);
	ui.GridsSecondaryLowerCutoffSpin->setEnabled(sourceGrid);
	ui.GridsSecondaryUpperCutoffSpin->setEnabled(sourceGrid);
	ui.GridsSecondaryColourButton->setEnabled(sourceGrid);
	ui.GridsSecondarySurfaceCheck->setEnabled(sourceGrid);
	if (!sourceGrid) return;

	ReturnValue rv;

	// Primary surface
	ui.GridsPrimaryLowerCutoffSpin->setRange(true, sourceGrid->minimum(), true, sourceGrid->maximum());
	ui.GridsPrimaryLowerCutoffSpin->setValue(sourceGrid->lowerPrimaryCutoff());
	ui.GridsPrimaryUpperCutoffSpin->setRange(true, sourceGrid->minimum(), true, sourceGrid->maximum());
	ui.GridsPrimaryUpperCutoffSpin->setValue(sourceGrid->upperPrimaryCutoff());
	rv.setArray(VTypes::DoubleData, sourceGrid->primaryColour(), 4);
	ui.GridsPrimaryColourButton->callPopupMethod("setColour", rv);

	// Secondary surface
	ui.GridsSecondaryLowerCutoffSpin->setRange(true, sourceGrid->minimum(), true, sourceGrid->maximum());
	ui.GridsSecondaryLowerCutoffSpin->setValue(sourceGrid->lowerSecondaryCutoff());
	ui.GridsSecondaryUpperCutoffSpin->setRange(true, sourceGrid->minimum(), true, sourceGrid->maximum());
	ui.GridsSecondaryUpperCutoffSpin->setValue(sourceGrid->upperSecondaryCutoff());
	rv.setArray(VTypes::DoubleData, sourceGrid->primaryColour(), 4);
	ui.GridsSecondaryColourButton->callPopupMethod("setColour", rv);

	// Enable / disable secondary surface controls
	ui.GridsSecondarySurfaceCheck->setChecked(sourceGrid->useSecondary());
	ui.GridsSecondaryLowerCutoffSpin->setEnabled(sourceGrid->useSecondary());
	ui.GridsSecondaryUpperCutoffSpin->setEnabled(sourceGrid->useSecondary());
	ui.GridsSecondaryColourButton->setEnabled(sourceGrid->useSecondary());
}
