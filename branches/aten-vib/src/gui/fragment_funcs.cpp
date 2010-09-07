/*
	*** Qt GUI: Fragment library window functions
	*** src/gui/fragment_funcs.cpp
	Copyright T. Youngs 2007-2010

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

#include "base/sysfunc.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/fragment.h"
#include "gui/ttreewidgetitem.h"
#include "gui/ttablewidgetitem.h"
#include "gui/tcanvas.uih"
#include "model/model.h"
#include "model/fragment.h"
#include "main/aten.h"

// Constructor
AtenFragment::AtenFragment(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
	ui.FragmentTable->setVisible(FALSE);

	// Private variables
	currentFragment_ = NULL;
}

// Destructor
AtenFragment::~AtenFragment()
{
}

void AtenFragment::showWindow()
{
	show();
}

// Return current drawing fragment
Fragment *AtenFragment::currentFragment()
{
	return currentFragment_;
}

// Refresh the atom list
void AtenFragment::refresh()
{
	msg.enter("AtenFragment::refresh");

	TTreeWidgetItem *item, *group;
	TTableWidgetItem *tabitem;
	ui.FragmentTree->clear();
	ui.FragmentTable->clear();

	int row = 0, nperrow = 5, column = 0;

	ui.FragmentTable->setColumnCount(nperrow);
	ui.FragmentTable->setRowCount(1);

	// Go through all available fragment groups
	for (FragmentGroup *fg = aten.fragmentGroups(); fg != NULL; fg = fg->next)
	{
		// Are there any fragments in this group?
		if (fg->nFragments() == 0) continue;

		// New row, if column counter is not zero
		if (column != 0)
		{
			row++;
			ui.FragmentTable->setRowCount(row+1);
		}

		// Create main tree branch
		group = new TTreeWidgetItem(ui.FragmentTree);
		group->setFlags(Qt::ItemIsEnabled);
		ui.FragmentTree->setItemExpanded(group, TRUE);
		group->setText(0, fg->name());
		//group->setFirstColumnSpanned(TRUE);	// Removed, since not present in Qt4.2

		column = 0;
		// Go through fragments in group
		for (Fragment *f = fg->fragments(); f != NULL; f = f->next)
		{
			// Filter?
			if (!filterText_.isEmpty() && (strstr(lowerCase(f->masterModel()->name()), filterText_.get()) == 0)) continue;
			// Add item to TTreeWidget
			item = new TTreeWidgetItem(group);
			item->data.set(VTypes::ModelData, f);		// No VTypes::FragmentData, so set as a Model instead
			item->setIcon(1,f->icon());
			item->setText(2,itoa(f->masterModel()->nAtoms()));
			item->setText(3,f->masterModel()->name());

			// Add item to TTableWidget
			if (column == 0)
			{
				tabitem = new TTableWidgetItem();
				tabitem->setText(fg->name());
				ui.FragmentTable->setVerticalHeaderItem(row, tabitem);
			}
			tabitem = new TTableWidgetItem();
			tabitem->data.set(VTypes::ModelData, f);		// No VTypes::FragmentData, so set as a Model instead
			tabitem->setIcon(f->icon());
			tabitem->setToolTip(f->masterModel()->name());
			ui.FragmentTable->setItem(row, column, tabitem);
			column++;
			if (column == nperrow)
			{
				row++;
				column = 0;
				ui.FragmentTable->setRowCount(row+1);
			}
			
			// If the currentFragment_ is NULL, set it as soon as possible
			if (currentFragment_ == NULL) currentFragment_ = f;
			if (currentFragment_ == f) item->setSelected(TRUE);
		}
	}
	// Resize columns and rows
	for (int n=0; n<3; n++) ui.FragmentTree->resizeColumnToContents(n);
	ui.FragmentTable->resizeRowsToContents();
	msg.exit("AtenFragment::refresh");
}

void AtenFragment::dialogFinished(int result)
{
	gui.mainWindow->ui.actionFragmentWindow->setChecked(FALSE);
}

void AtenFragment::on_FragmentTree_currentItemChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous)
{
	if (current == NULL) currentFragment_ = NULL;
	else
	{
		// Cast into TTreeWidgetItem
		TTreeWidgetItem *twi = (TTreeWidgetItem*) current;
		if ((twi == NULL) || (twi->data.type() != VTypes::ModelData)) currentFragment_ = NULL;
		else
		{
			// If this is a header item in the Tree, the fragment pointer will be NULL
			// No VTypes::FragmentData exists, so it was stored as a model
			currentFragment_ = (Fragment*) twi->data.asPointer(VTypes::ModelData);
		}
	}
}

void AtenFragment::on_FragmentTree_doubleClicked(const QModelIndex &index)
{
	gui.mainWindow->ui.actionDrawFragment->trigger();
}

void AtenFragment::on_FragmentTable_currentItemChanged(QTableWidgetItem *current, QTableWidgetItem *previous)
{
	if (current == NULL) currentFragment_ = NULL;
	else
	{
		// Cast into TTreeWidgetItem
		TTableWidgetItem *twi = (TTableWidgetItem*) current;
		if ((twi == NULL) || (twi->data.type() != VTypes::ModelData)) currentFragment_ = NULL;
		else
		{
			// If this is a header item in the Tree, the fragment pointer will be NULL
			// No VTypes::FragmentData exists, so it was stored as a model
			currentFragment_ = (Fragment*) twi->data.asPointer(VTypes::ModelData);
		}
	}
}

void AtenFragment::on_FragmentTable_doubleClicked(const QModelIndex &index)
{
	gui.mainWindow->ui.actionDrawFragment->trigger();
}


void AtenFragment::on_FragmentFilterEdit_textChanged(const QString &text)
{
	filterText_ = lowerCase(qPrintable(text));
	refresh();
}

void AtenFragment::on_FragmentShowAllButton_clicked(bool checked)
{
	ui.FragmentFilterEdit->setText("");
	filterText_.clear();
	refresh();
}

void AtenFragment::on_ViewAsListCheck_clicked(bool checked)
{
	if (checked)
	{
		ui.FragmentTree->setVisible(TRUE);
		ui.FragmentTable->setVisible(FALSE);
	}
	else
	{
		ui.FragmentTree->setVisible(FALSE);
		ui.FragmentTable->setVisible(TRUE);
	}
}

void AtenFragment::on_ViewAsGridCheck_clicked(bool checked)
{
	if (checked)
	{
		ui.FragmentTree->setVisible(FALSE);
		ui.FragmentTable->setVisible(TRUE);
	}
	else
	{
		ui.FragmentTree->setVisible(TRUE);
		ui.FragmentTable->setVisible(FALSE);
	}
}
