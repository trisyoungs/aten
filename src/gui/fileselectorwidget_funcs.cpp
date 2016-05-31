/*
	*** FileSelector Functions
	*** src/gui/fileselectorwidget_funcs.cpp
	Copyright T. Youngs 2007-2016

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

#include "gui/fileselectorwidget.h"
#include "base/lineparser.h"
#include "templates/variantpointer.h"
#include <QFileSystemModel>

// Static Singletons
QStringList FileSelectorWidget::favourites_;

// Constructor
FileSelectorWidget::FileSelectorWidget(QWidget* parent) : QWidget(parent)
{
	ui.setupUi(this);

	// Setup file system model and attach to view
	fileSystemModel_.setRootPath("");
	fileSystemModel_.setFilter(QDir::AllDirs | QDir::AllEntries);
	fileSystemModel_.setNameFilterDisables(false);
	ui.FileView->setModel(&fileSystemModel_);
	ui.FileView->hideColumn(2);

	// Connect signals
	connect(&fileSystemModel_, SIGNAL(directoryLoaded(QString)), this, SLOT(resizeFileView(QString)));

	refreshing_ = false;
}

/*
 * Local Data / Functions
 */

// Set mode of file selector
void FileSelectorWidget::setMode(FileSelectorWidget::SelectionMode mode, const RefList<IOPluginInterface,int>& ioPlugins, QDir startingDir)
{
	mode_ = mode;

	// Set relevant selection mode for file view
	if (mode_ == FileSelectorWidget::FileSelectorWidget::OpenMultipleMode) ui.FileView->setSelectionMode(QTableView::ExtendedSelection);
	else ui.FileView->setSelectionMode(QTableView::SingleSelection);

	// Populate filter combo
	for (RefListItem<IOPluginInterface,int>* ri = ioPlugins.first(); ri != NULL; ri = ri->next)
	{
		IOPluginInterface* interface = ri->item;
		if (!interface->canImport()) continue;
		ui.FilterCombo->addItem(interface->filterString(), VariantPointer<IOPluginInterface>(interface));
	}
	ui.FilterCombo->addItem("All Files (*)");

	setCurrentDirectory(startingDir.absolutePath());
	updateWidgets();
}

// Set current directory of file selector
void FileSelectorWidget::setCurrentDirectory(QString directory)
{
	refreshing_ = true;

	// See if the supplied dir is valid in the context of our fileSystemModel_
	QModelIndex index = fileSystemModel_.index(directory);
	if (index.isValid())
	{
		currentDirectory_ = directory;
		ui.FileView->setRootIndex(index);
		ui.DirectoryEdit->setText(currentDirectory_.absolutePath());
	}

	refreshing_ = false;

	updateWidgets();
}

// Clear selected filenames list
void FileSelectorWidget::clearSelectedFilenames()
{
	selectedFilenames_.clear();
}

// Return selected files, including full path
QStringList FileSelectorWidget::selectedFiles()
{
	QStringList fileList;
	for (int n=0; n<selectedFilenames_.count(); ++n) fileList << currentDirectory_.absoluteFilePath(selectedFilenames_.at(n));

	return fileList;
}

// Return selected file plugin
IOPluginInterface* FileSelectorWidget::selectedPlugin()
{
	// Get selected filter from combo box
	IOPluginInterface* interface = (IOPluginInterface*) VariantPointer<IOPluginInterface>(ui.FilterCombo->itemData(ui.FilterCombo->currentIndex()));
	return interface;
}

/*
 * Widget Functions
*/

// Update widgets, e.g. after directory change
void FileSelectorWidget::updateWidgets()
{
	// Favourites list
	ui.FavouritesTable->clear();
	ui.FavouritesTable->setColumnCount(1);
	ui.FavouritesTable->setHorizontalHeaderLabels(QStringList() << "Favourites");
	ui.FavouritesTable->setRowCount(favourites_.count());
	for (int n=0; n<favourites_.count(); ++n)
	{
		QTableWidgetItem* item = new QTableWidgetItem(favourites_.at(n));
		ui.FavouritesTable->setItem(0, n, item);
	}

	// File table
	ui.FileView->resizeColumnsToContents();

	// Files edit
	QString files;
	for (int n=0; n<selectedFilenames_.count(); ++n)
	{
		if (n != 0) files += " ";
		files += """" + selectedFilenames_.at(n) + """";
	}
	ui.FilesEdit->setText(files);
}

// Resize columns of file table
void FileSelectorWidget::resizeFileView(QString dummy)
{
	ui.FileView->resizeColumnsToContents();
}

void FileSelectorWidget::on_FileView_clicked(const QModelIndex& index)
{
	// Get current model selection and reconstruct selected files list
	QItemSelectionModel* selectionModel = ui.FileView->selectionModel();
	QModelIndexList selectedRows = selectionModel->selectedRows();
	selectedFilenames_.clear();
	for (int n=0; n<selectedRows.count(); ++n)
	{
		if (fileSystemModel_.isDir(selectedRows.at(n))) continue;
		selectedFilenames_ << fileSystemModel_.fileName(selectedRows.at(n));
	}

	emit(selectionValid(selectedFilenames_.count() > 0));

	updateWidgets();
}

void FileSelectorWidget::on_FileView_doubleClicked(const QModelIndex& index)
{
	// If the target is a directory, change to that directory.
	// Otherwise, adjust the selectedFilenames_ list and emit the selectionMade() signal.
	if (fileSystemModel_.isDir(index))
	{
		setCurrentDirectory(fileSystemModel_.filePath(index));

		// Need to clear the current files list, since it will no longer be valid
		selectedFilenames_.clear();
		emit(selectionValid(false));

		return;
	}

	// Not a dir, so adjust the filenames list and emit the signal
	QItemSelectionModel* selectionModel = ui.FileView->selectionModel();
	QModelIndexList selectedRows = selectionModel->selectedRows();
	selectedFilenames_.clear();
	for (int n=0; n<selectedRows.count(); ++n)
	{
		if (fileSystemModel_.isDir(selectedRows.at(n))) continue;
		selectedFilenames_ << fileSystemModel_.fileName(selectedRows.at(n));
	}

	emit(selectionValid(selectedFilenames_.count() > 0));

	updateWidgets();

	emit(selectionMade(false));
}

void FileSelectorWidget::on_FilesEdit_returnPressed()
{
	emit(selectionMade(false));
}

void FileSelectorWidget::on_FilesEdit_textChanged(QString textChanged)
{
	// Split current string into separate arguments
	LineParser parser;
	parser.getArgsDelim(Parser::UseQuotes, ui.FilesEdit->text());

	selectedFilenames_.clear();
	for (int n = 0; n < parser.nArgs(); ++n) selectedFilenames_ << parser.argc(n);

	emit(selectionValid(selectedFilenames_.count() > 0));
}

void FileSelectorWidget::on_FilterCombo_currentIndexChanged(int index)
{
	// Grab data for selected item
	IOPluginInterface* interface = (IOPluginInterface*) VariantPointer<IOPluginInterface>(ui.FilterCombo->itemData(index));

	if (!interface)
	{
		// Unrecognised interface, or the All Files entry, so remove any filtering from the file system model
		fileSystemModel_.setNameFilters(QStringList());
	}
	else
	{
		// Add extensions and exact names to the names filters
		QStringList nameFilters;
		for (int n=0; n<interface->extensions().count(); ++n) nameFilters << "*." + interface->extensions().at(n);
		for (int n=0; n<interface->exactNames().count(); ++n) nameFilters << interface->exactNames().at(n);
		fileSystemModel_.setNameFilters(nameFilters);
	}
}
