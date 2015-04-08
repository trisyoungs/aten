/*
	*** Fragment Library Dock Widget
	*** src/gui/fragment_funcs.cpp
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

#include <QCloseEvent>
#include "base/sysfunc.h"
#include "base/progress.h"
#include "gui/mainwindow.h"
#include "gui/fragments.h"
#include "model/fragment.h"
#include "main/aten.h"
#include "templates/variantpointer.h"

ATEN_USING_NAMESPACE

// Constructor
FragmentsWidget::FragmentsWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	ui.setupUi(this);
	ui.FragmentTable->setVisible(false);

	// Private variables
	currentFragment_ = NULL;
	bondId_ = 0;
	iconsGenerated_ = false;
}

void FragmentsWidget::showWidget()
{
	show();
	refresh();
}

// Increment bond id value
void FragmentsWidget::increaseBondId()
{
	++bondId_;
}

// Return bondId (as reference so it can be reset by associated Fragment routines)
int &FragmentsWidget::bondId()
{
	return bondId_;
}

// Return current drawing fragment
Fragment* FragmentsWidget::currentFragment()
{
	return currentFragment_;
}

// Refresh the atom list
void FragmentsWidget::refresh()
{
	Messenger::enter("FragmentsWidget::refresh");
	
	QTreeWidgetItem* item, *group;
	QTableWidgetItem* tableItem;
	ui.FragmentTree->clear();
	ui.FragmentTable->clear();
	
	int row = 0, nperrow = 5, column = 0;

	ui.FragmentTable->setColumnCount(nperrow);
	ui.FragmentTable->setRowCount(1);

	// Generate icon if necessary (first run only) and allowed (through prefs)
// 	if ((!iconsGenerated_) && prefs.generateFragmentIcons())   // ATEN2 TODO
// 	{
// 		int nFragments = 0;
// 		for (FragmentGroup* fg = parent_.aten().fragmentGroups(); fg != NULL; fg = fg->next) nFragments += fg->nFragments();
// 		int pid = progress.initialise("Initialising fragment icons", nFragments);
// 		for (FragmentGroup* fg = parent_.aten().fragmentGroups(); fg != NULL; fg = fg->next)
// 		{
// 			for (Fragment* fragment = fg->fragments(); fragment != NULL; fragment = fragment->next)
// 			{
// 				fragment->masterModel()->regenerateIcon();
// 				progress.update(pid, -1, fragment->masterModel()->name());
// 			}
// 		}
// 		progress.terminate(pid);
// 	}

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
#if QT_VERSION >= 0x040300
		group->setFirstColumnSpanned(true);
#endif

		column = 0;
		// Go through fragments in group
		for (Fragment* fragment = fragGroup->fragments(); fragment != NULL; fragment = fragment->next)
		{
			// Filter this fragment?
			if ((!filterText_.isEmpty()) && (!fragment->masterModel()->name().contains(filterText_, Qt::CaseInsensitive))) continue;

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
			if (column == nperrow)
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
	iconsGenerated_ = true;
	Messenger::exit("FragmentsWidget::refresh");
}

void FragmentsWidget::on_FragmentTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous)
{
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
}

void FragmentsWidget::on_FragmentTree_doubleClicked(const QModelIndex &index)
{
// 	gui.buildWidget->ui.DrawFragmentButton->click(); ATEN2 TODO
}

void FragmentsWidget::on_FragmentTable_currentItemChanged(QTableWidgetItem *current, QTableWidgetItem *previous)
{
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
}

void FragmentsWidget::on_FragmentTable_doubleClicked(const QModelIndex &index)
{
	// gui.buildWidget->ui.DrawFragmentButton->click(); ATEN2 TODO
}


void FragmentsWidget::on_FragmentFilterEdit_textChanged(const QString &text)
{
	filterText_ = text.toLower();
	refresh();
}

void FragmentsWidget::on_FragmentShowAllButton_clicked(bool checked)
{
	ui.FragmentFilterEdit->setText("");
	filterText_.clear();
	refresh();
}

void FragmentsWidget::on_ViewAsListCheck_clicked(bool checked)
{
	if (checked)
	{
		ui.FragmentTree->setVisible(true);
		ui.FragmentTable->setVisible(false);
	}
	else
	{
		ui.FragmentTree->setVisible(false);
		ui.FragmentTable->setVisible(true);
	}
}

void FragmentsWidget::on_ViewAsGridCheck_clicked(bool checked)
{
	if (checked)
	{
		ui.FragmentTree->setVisible(false);
		ui.FragmentTable->setVisible(true);
	}
	else
	{
		ui.FragmentTree->setVisible(true);
		ui.FragmentTable->setVisible(false);
	}
}

void FragmentsWidget::closeEvent(QCloseEvent* event)
{
	event->accept();
}
