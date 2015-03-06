/*
	*** Select filter functions interface
	*** src/gui/selectfilter_funcs.cpp
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
#include "gui/selectfilter.h"
#include "parser/tree.h"

// Constructor
AtenSelectFilter::AtenSelectFilter(AtenWindow& parent) : QDialog(&parent), parent_(parent)
{
	ui.setupUi(this);

	// Private variables
	partialList_ = NULL;
	fullList_ = NULL;
	selectedFilter_ = NULL;
	appendExtension_ = TRUE;
}

// Select item in list and finish dialog
void AtenSelectFilter::on_FilterTable_doubleClicked(const QModelIndex &index)
{
	int row = index.row();
	if (row == -1) selectedFilter_ = NULL;
	else if (ui.ShowAllCheck->isChecked()) selectedFilter_ = (*fullList_)[row]->item;
	else selectedFilter_ = (*partialList_)[row]->item;
	accept();
}

// Select item in list
void AtenSelectFilter::on_FilterTable_itemSelectionChanged()
{
	int row = ui.FilterTable->currentRow();
	if (row == -1) selectedFilter_ = NULL;
	else if (ui.ShowAllCheck->isChecked()) selectedFilter_ = (*fullList_)[row]->item;
	else selectedFilter_ = (*partialList_)[row]->item;
	if (row != -1) ui.OkButton->setDisabled(FALSE);
}

void AtenSelectFilter::on_ShowAllCheck_clicked(bool checked)
{
	update();
}

// Update the list of filters
void AtenSelectFilter::update()
{
	ui.FilterTable->clear();
	Refitem<Tree,int>* first = (ui.ShowAllCheck->isChecked() ? fullList_->first() : partialList_->first());
	ui.FilterTable->setHorizontalHeaderLabels(QStringList() << "Extension(s)" << "Filter" << "Source");
	ui.FilterTable->setRowCount( ui.ShowAllCheck->isChecked() ? fullList_->nItems() : partialList_->nItems() );
	QTableWidgetItem *item;
	int count = 0;
	for (Refitem<Tree,int>* ri = first; ri != NULL; ri = ri->next)
	{
		// First column lists the extensions, second column the filter description, third column the source filter file
		item = new QTableWidgetItem(ri->item->filter.extensionList());
		ui.FilterTable->setItem(count, 0, item);
		item = new QTableWidgetItem(ri->item->filter.name());
		ui.FilterTable->setItem(count, 1, item);
		item = new QTableWidgetItem(ri->item->name());
		ui.FilterTable->setItem(count, 2, item);
		count ++;
	}
	for (count=0; count<3; count++) ui.FilterTable->resizeColumnToContents(count);
}

// Select a filter from the list supplied
Tree* AtenSelectFilter::selectFilter(const char* text, Reflist<Tree,int>* partial, Reflist<Tree,int>* full, bool showExtCheck)
{
	// Set source structures
	partialList_ = partial;
	fullList_ = full;

	// Change textlabel
	ui.TextLabel->setText(text);

	// Check and disable ShowAllCheck if no partial matches were found
	ui.ShowAllCheck->setChecked(partial == NULL);
	ui.ShowAllCheck->setEnabled(partial != NULL);

	// Disable 'OK' button until a filter is selected from the list
	ui.OkButton->setDisabled(TRUE);

	// Show/hide 'Append Extension' checkbox
	ui.AppendExtensionCheck->setVisible(showExtCheck);
	
	update();

	// Execute the dialog and check on the result
	exec();
	appendExtension_ = ui.AppendExtensionCheck->isChecked();
	return selectedFilter_;
}


// Whether to append extension to filename
bool AtenSelectFilter::appendExtension()
{
	return appendExtension_;
}

void AtenSelectFilter::on_CancelButton_clicked(bool checked)
{
	selectedFilter_ = NULL;
	reject();
}

void AtenSelectFilter::on_OkButton_clicked(bool checked)
{
	// Get the current selection again, just to be on the safe side
	int row = ui.FilterTable->currentRow();
	if (row == -1) selectedFilter_ = NULL;
	else if (ui.ShowAllCheck->isChecked()) selectedFilter_ = (*fullList_)[row]->item;
	else selectedFilter_ = (*partialList_)[row]->item;
	accept();
}
