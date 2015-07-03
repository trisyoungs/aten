/*
	*** Popup Widget - Build Fragments
	*** src/gui/popupbuildfragments_funcs.cpp
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

#include "gui/popupbuildfragments.h"
#include "main/aten.h"
#include "main/version.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "templates/variantpointer.h"

ATEN_USING_NAMESPACE

// Constructor
BuildFragmentsPopup::BuildFragmentsPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Add in fragments to the table and tree
	callMethodSimple("updateFragments", "");
}

// Show popup, updating any controls as necessary beforehand
void BuildFragmentsPopup::popup()
{
	refreshing_ = true;

	show();

	refreshing_ = false;
}

// Call named method associated to popup
bool BuildFragmentsPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else if (methodName == "updateFragments")
	{
		QTreeWidgetItem* item, *group;
		QTableWidgetItem* tableItem;
		QString filterText = ui.FilterEdit->text();
		int row = 0, nPerRow = 5, column = 0;

		// Clear existing contents and set column width
		ui.FragmentTree->clear();
		ui.FragmentTable->clear();
		ui.FragmentTable->setColumnCount(nPerRow);
		ui.FragmentTable->setRowCount(1);

		// Go through all available fragment groups
		for (FragmentGroup* fragGroup = parent_.aten().fragmentGroups(); fragGroup != NULL; fragGroup = fragGroup->next)
		{
			// Are there any fragments in this group?
			if (fragGroup->nFragments() == 0) continue;

			// New row, if column counter is not zero
			if (column != 0)
			{
				++row;
				ui.FragmentTable->setRowCount(row+1);
			}

			// Create main tree branch
			group = new QTreeWidgetItem(ui.FragmentTree);
			group->setFlags(Qt::ItemIsEnabled);
			ui.FragmentTree->setItemExpanded(group, true);
			group->setText(0, fragGroup->name());
			group->setFirstColumnSpanned(true);

			column = 0;
			// Go through fragments in group
			for (Fragment* fragment = fragGroup->fragments(); fragment != NULL; fragment = fragment->next)
			{
				// Filter this fragment?
				if ((!filterText.isEmpty()) && (!fragment->masterModel()->name().contains(filterText, Qt::CaseInsensitive))) continue;

				// Add item to TTreeWidget
				item = new QTreeWidgetItem(group);
				item->setData(0, Qt::UserRole, VariantPointer<Fragment>(fragment));
				item->setIcon(1, fragment->icon());
				item->setText(2, QString::number(fragment->masterModel()->nAtoms()));
				item->setText(3, fragment->masterModel()->name());

				// Add item to TTableWidget
				if (column == 0)
				{
					tableItem = new QTableWidgetItem();
					tableItem->setText(fragGroup->name());
					ui.FragmentTable->setVerticalHeaderItem(row, tableItem);
				}
				tableItem = new QTableWidgetItem();
				tableItem->setData(Qt::UserRole, VariantPointer<Fragment>(fragment));

				tableItem->setIcon(fragment->icon());
				tableItem->setToolTip(fragment->masterModel()->name());
				ui.FragmentTable->setItem(row, column, tableItem);
				column++;
				if (column == nPerRow)
				{
					row++;
					column = 0;
					ui.FragmentTable->setRowCount(row+1);
				}
				
				// If the currentFragment_ is NULL, set it as soon as possible
				if (currentFragment_ == NULL) currentFragment_ = fragment;
				if (currentFragment_ == fragment) item->setSelected(true);
			}
		}
		
		// Resize columns and rows
		for (int n=0; n<3; n++) ui.FragmentTree->resizeColumnToContents(n);
		ui.FragmentTable->resizeRowsToContents();
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */


void BuildFragmentsPopup::on_FragmentTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous)
{
	if (refreshing_) return;

	if (current == NULL) currentFragment_ = NULL;
	else
	{
		// Cast into TTreeWidgetItem
		if (current == NULL) currentFragment_ = NULL;
		else
		{
			// If this is a header item in the Tree, the fragment pointer will be NULL
			// No VTypes::FragmentData exists, so it was stored as a model
			currentFragment_ = VariantPointer<Fragment>(current->data(0, Qt::UserRole));
		}
	}

	hide();
}

void BuildFragmentsPopup::on_FragmentTable_currentItemChanged(QTableWidgetItem *current, QTableWidgetItem *previous)
{
	if (refreshing_) return;

	if (current == NULL) currentFragment_ = NULL;
	else
	{
		// Cast into TTreeWidgetItem
		if (current == NULL) currentFragment_ = NULL;
		else
		{
			// If this is a header item in the Tree, the fragment pointer will be NULL
			// No VTypes::FragmentData exists, so it was stored as a model
			currentFragment_ = VariantPointer<Fragment>(current->data(Qt::UserRole));
		}
	}
	
	hide();
}

void BuildFragmentsPopup::on_FilterEdit_textChanged(const QString& text)
{
	callMethodSimple("updateFragments");
}

void BuildFragmentsPopup::on_ClearFilterButton_clicked(bool checked)
{
	ui.FilterEdit->setText("");
}

void BuildFragmentsPopup::on_ViewAsGridCheck_clicked(bool checked)
{
	ui.FragmentsStack->setCurrentIndex(checked ? 1 : 0);
}

