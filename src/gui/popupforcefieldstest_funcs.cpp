/*
	*** Popup Widget - Forcefields Test Functions
	*** src/gui/popupforcefieldstest_funcs.cpp
	Copyright T. Youngs 2007-2018

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

#include "gui/popupforcefieldstest.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/popupelementtable.h"
#include "base/namespace.h"
#include "ff/forcefield.h"
#include "base/pattern.h"
#include "base/forcefieldatom.h"

ATEN_USING_NAMESPACE

// Constructor
ForcefieldsTestPopup::ForcefieldsTestPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	ui.ElementButton->setPopupWidget(new ElementTablePopup(parent_, ui.ElementButton), true);
}

// Update controls (before show()) (virtual)
void ForcefieldsTestPopup::updateControls()
{
	refreshing_ = true;

	refreshTypes();

	refreshing_ = false;
}

// Call named method associated to popup
bool ForcefieldsTestPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else
	{
		printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
		result = false;
	}
	return result;
}

/*
 * Private Functions
 */

void ForcefieldsTestPopup::refreshTypes()
{
	ui.FFTypeTable->clear();
	QTableWidgetItem *item;
	int count = 0;
	Forcefield* ff = parent_.aten().currentForcefield();
	if (ff == NULL) return;

	// Get current element from button
	ReturnValue element;
	ui.ElementButton->callPopupMethod("currentElement", element);

	// Reset header labels
	ui.FFTypeTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "Name" << "Description");
	for (ForcefieldAtom* ffa = ff->types(); ffa != NULL; ffa = ffa->next)
	{
		if (ffa->neta()->characterElement() != element.asInteger()) continue;
		
		ui.FFTypeTable->setRowCount(count+1);
		item = new QTableWidgetItem(QString::number(ffa->typeId()));
		ui.FFTypeTable->setItem(count, 0, item);
		item = new QTableWidgetItem(ffa->name());
		ui.FFTypeTable->setItem(count, 1, item);
		item = new QTableWidgetItem(ffa->description());
		ui.FFTypeTable->setItem(count, 2, item);
		count ++;
	}

	// Resize the columns
	ui.FFTypeTable->resizeColumnToContents(0);
	ui.FFTypeTable->resizeColumnToContents(1);
	ui.FFTypeTable->resizeColumnToContents(2);
}

/*
 * Widget Functions
 */

// Set the selected atoms to have the specified forcefield type
void ForcefieldsTestPopup::on_SetButton_clicked(bool checked)
{
	// Check selected forcefield against that assigned to the model
	Model* m = parent_.aten().currentModel();
	Forcefield* ff = parent_.aten().currentForcefield();
	if ((m == NULL) || (ff == NULL)) return;
	if (m->forcefield() != ff)
	{
		Messenger::print("The type you are trying to assign is in a different forcefield to that assigned to the model.");
		return;
	}

	// Get the selected row in the FFTypeList
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ForcefieldAtom* ffa = ff->findType(atoi(qPrintable(item->text())));
	if (ffa != NULL)
	{
		m->selectionSetType(ffa, true);
		Messenger::print("Manually set types of %i atoms.", parent_.aten().currentModel()->nSelected());
	}
	parent_.updateWidgets();
}

// Clear type definitions from the selected atoms
void ForcefieldsTestPopup::on_ClearButton_clicked(bool checked)
{
	parent_.aten().currentModel()->selectionSetType(NULL, false);
	Messenger::print("Cleared types of %i atoms.", parent_.aten().currentModel()->nSelected());
	parent_.updateWidgets();
}

// Test selected atom type on current atom selection
void ForcefieldsTestPopup::on_TestButton_clicked(bool checked)
{
	Forcefield* ff = parent_.aten().currentForcefield();
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ForcefieldAtom* ffa = ff->findType(atoi(qPrintable(item->text())));
	if (ffa != NULL)
	{
		Model* m = parent_.aten().currentModel();
		Neta* at = ffa->neta();
		if (m->createPatterns())
		{
			Messenger::print("Testing atom type '%s' (id = %i) from forcefield '%s' on current selection:", qPrintable(ffa->name()), ffa->typeId(), qPrintable(ff->name()));

			// Prepare for typing
			m->describeAtoms();
			int matchscore;
			for (RefListItem<Atom,int>* ri = m->selection(); ri != NULL; ri = ri->next)
			{
				// Get the pattern in which the atom exists
				Pattern* p = m->pattern(ri->item);
				if (ri->item->element() == at->characterElement())
				{
					matchscore = at->matchAtom(ri->item, p->ringList(), m);
					Messenger::print("Atom %i (%s) matched type with score %i.", ri->item->id()+1, ElementMap::symbol(ri->item), matchscore);
				}
				else Messenger::print("Atom %i (%s) is the wrong element for this type.", ri->item->id()+1, ElementMap::symbol(ri->item));
			}
		}
	}
}
