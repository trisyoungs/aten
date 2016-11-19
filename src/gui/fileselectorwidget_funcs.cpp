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
#include <QInputDialog>
#include <QMenu>

// Static Singletons
QStringList FileSelectorWidget::favourites_;

// Constructor
FileSelectorWidget::FileSelectorWidget(QWidget* parent) : QWidget(parent)
{
	ui.setupUi(this);

	// Setup file system model and attach to view
	fileSystemModel_.setRootPath("");
	fileSystemModel_.setFilter(QDir::AllDirs | QDir::NoDotAndDotDot | QDir::AllEntries);
	fileSystemModel_.setNameFilterDisables(false);
	ui.FileView->setModel(&fileSystemModel_);
	ui.FileView->hideColumn(2);

	// Connect signals
	connect(&fileSystemModel_, SIGNAL(directoryLoaded(QString)), this, SLOT(resizeFileView(QString)));
	connect(ui.FavouritesTable, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(favouritesContextMenuRequested(QPoint)));

	refreshing_ = false;
}

/*
 * Local Data / Functions
 */

// Set mode of file selector
void FileSelectorWidget::setMode(FileSelectorWidget::SelectionMode mode, QDir startingDir)
{
	mode_ = mode;

	// Set relevant selection mode for file view
	if (mode_ == FileSelectorWidget::OpenMultipleMode) ui.FileView->setSelectionMode(QTableView::ExtendedSelection);
	else ui.FileView->setSelectionMode(QTableView::SingleSelection);

	setCurrentDirectory(startingDir.absolutePath());
	updateWidgets();
}

// Refresh plugins (filters) combo
void FileSelectorWidget::refreshPlugins(const RefList<FilePluginInterface,KVMap>& filePlugins)
{
	ui.FilterCombo->clear();

	for (RefListItem<FilePluginInterface,KVMap>* ri = filePlugins.first(); ri != NULL; ri = ri->next)
	{
		FilePluginInterface* plugin = ri->item;

		// The mode_ of the file selector determines which type of plugin we display
		if (mode_ == FileSelectorWidget::SaveSingleMode)
		{
			if (!plugin->canExport()) continue;
		}
		else if (!plugin->canImport()) continue;
		ui.FilterCombo->addItem(plugin->filterString(), VariantPointer< RefListItem<FilePluginInterface,KVMap> >(ri));
	}
	ui.FilterCombo->addItem("All Files (*)");
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

// Set current filename selection
void FileSelectorWidget::setSelectedFilename(QString filename)
{
	selectedFilenames_.clear();

	// Set current directory to match that of the filename
	QFileInfo fileInfo(filename);
	setCurrentDirectory(fileInfo.absolutePath());
	selectedFilenames_ << fileInfo.fileName();

	// Check to see if the filename exists in the current dir
	QModelIndex index = fileSystemModel_.index(currentDirectory_.filePath(filename));
	if (index.isValid()) ui.FileView->selectRow(index.row());
}

// Set current plugin selection
void FileSelectorWidget::setSelectedPlugin(const FilePluginInterface* plugin)
{
	for (int n=0; n<ui.FilterCombo->count(); ++n)
	{
		RefListItem<FilePluginInterface,KVMap>* refItem = (RefListItem<FilePluginInterface,KVMap>*) VariantPointer< RefListItem<FilePluginInterface,KVMap> >(ui.FilterCombo->itemData(n));
		FilePluginInterface* filterPlugin = refItem->item;
		if (filterPlugin == plugin)
		{
			ui.FilterCombo->setCurrentIndex(n);
			return;
		}
	}

	printf("Plugin provided did not match any in the FileSelectorWidget.\n");
}

// Return selected files, including full path
QStringList FileSelectorWidget::selectedFiles()
{
	QStringList fileList;
	for (int n=0; n<selectedFilenames_.count(); ++n) fileList << currentDirectory_.absoluteFilePath(selectedFilenames_.at(n));

	return fileList;
}

// Return selected file plugin
const FilePluginInterface* FileSelectorWidget::selectedPlugin()
{
	// Get selected filter from combo box data
	RefListItem<FilePluginInterface,KVMap>* refItem = (RefListItem<FilePluginInterface,KVMap>*) VariantPointer< RefListItem<FilePluginInterface,KVMap> >(ui.FilterCombo->currentData());
	return (refItem ? refItem->item : NULL);
}

// Return reference to the options for the selected file plugin
KVMap& FileSelectorWidget::selectedPluginOptions()
{
	RefListItem<FilePluginInterface,KVMap>* refItem = (RefListItem<FilePluginInterface,KVMap>*) VariantPointer< RefListItem<FilePluginInterface,KVMap> >(ui.FilterCombo->currentData());
	return (refItem ? refItem->item : NULL);
}

// Add favourite place to list
void FileSelectorWidget::addFavourite(QString place)
{
	favourites_ << place;
}

// Clear favourite places list
void FileSelectorWidget::clearFavourites()
{
	favourites_.clear();
}

// Return favourite places list
const QStringList FileSelectorWidget::favourites()
{
	return favourites_;
}

/*
 * Widget Functions
 */

// Update widgets, e.g. after directory change
void FileSelectorWidget::updateWidgets()
{
	refreshing_ = true;

	// Favourites list
	ui.FavouritesTable->clear();
	ui.FavouritesTable->setColumnCount(1);
	ui.FavouritesTable->setHorizontalHeaderLabels(QStringList() << "Favourites");
	ui.FavouritesTable->setRowCount(favourites_.count());
	for (int n=0; n<favourites_.count(); ++n)
	{
		QDir favourite(favourites_.at(n));
		QTableWidgetItem* item = new QTableWidgetItem(favourite.dirName());
		if (item->text().isEmpty()) item->setText(favourites_.at(n));
		item->setData(Qt::UserRole, n);
		item->setToolTip(favourites_.at(n));

		// Set icon, depending on what the item is...
		if (favourite == QDir::home()) item->setIcon(style()->standardIcon(QStyle::SP_DirHomeIcon));
		else if (favourite.isRoot()) item->setIcon(style()->standardIcon(QStyle::SP_DriveHDIcon));
		else item->setIcon(style()->standardIcon(QStyle::SP_DirIcon));
		ui.FavouritesTable->setItem(0, n, item);
	}

	// File table
	ui.FileView->resizeColumnsToContents();
	ui.FileView->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);

	// Files edit
	QString files;
	for (int n=0; n<selectedFilenames_.count(); ++n)
	{
		if (n != 0) files += " ";
		files += """" + selectedFilenames_.at(n) + """";
	}
	ui.FilesEdit->setText(files);

	refreshing_ = false;
}

// Resize columns of file table
void FileSelectorWidget::resizeFileView(QString dummy)
{
	ui.FileView->resizeColumnsToContents();
	ui.FileView->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
}

// Custom context menu requested for favourites list
void FileSelectorWidget::favouritesContextMenuRequested(const QPoint& point)
{
	// Get item at point
	QTableWidgetItem* item = ui.FavouritesTable->itemAt(point);

	// Build the context menu to display
	QMenu contextMenu;
	QAction* addCurrentAction = contextMenu.addAction("&Add current directory");
	QAction* removeAction = contextMenu.addAction("&Remove");

	// Disable items if required
	if (!item) removeAction->setEnabled(false);
 	for (int n=0; n<favourites_.count(); ++n) if (currentDirectory_ == QDir(favourites_.at(n))) addCurrentAction->setEnabled(false);

	// Show it
	QPoint menuPosition = ui.FavouritesTable->mapToGlobal(point);
	QAction* menuResult = contextMenu.exec(menuPosition);

	// What was clicked?
	if (menuResult == addCurrentAction)
	{
		favourites_ << currentDirectory_.absolutePath();
		updateWidgets();
	}
	else if (menuResult == removeAction)
	{
		// Get directory string from item
		int index = item->data(Qt::UserRole).toInt();
		favourites_.removeAt(index);
		updateWidgets();
	}
}

void FileSelectorWidget::on_DirectoryEdit_returnPressed()
{
	if (refreshing_) return;

	// Try to convert the text to a proper directory
	QDir newDirectory;
	newDirectory.setPath(ui.DirectoryEdit->text());

	setCurrentDirectory(newDirectory.absolutePath());
}

void FileSelectorWidget::on_DirectoryUpButton_clicked(bool checked)
{
	if (currentDirectory_.cdUp()) setCurrentDirectory(currentDirectory_.absolutePath());
}

void FileSelectorWidget::on_DirectoryCreateButton_clicked(bool checked)
{
	// Get the new name of the directory to create
	bool ok;
	QString newDirectory = QInputDialog::getText(this, "Create Directory", "Enter new directory name:", QLineEdit::Normal, QString(), &ok);
	if (ok)
	{
		currentDirectory_.mkdir(newDirectory);
	}
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
	if (refreshing_) return;

	// Split current string into separate arguments
	LineParser parser;
	parser.getArgsDelim(Parser::UseQuotes, ui.FilesEdit->text());

	selectedFilenames_.clear();
	for (int n = 0; n < parser.nArgs(); ++n) selectedFilenames_ << parser.argc(n);

	emit(selectionValid(selectedFilenames_.count() > 0));
}

void FileSelectorWidget::on_FilterCombo_currentIndexChanged(int index)
{
	if (refreshing_) return;

	// Grab data for selected item
	RefListItem<FilePluginInterface,KVMap>* ri = (RefListItem<FilePluginInterface,KVMap>*) VariantPointer< RefListItem<FilePluginInterface,KVMap> >(ui.FilterCombo->itemData(index));
	FilePluginInterface* plugin = (ri ? ri->item : NULL);

	if (!plugin)
	{
		// Unrecognised interface, or the All Files entry, so remove any filtering from the file system model
		fileSystemModel_.setNameFilters(QStringList());
		emit(pluginOptionsAvailable(false));
	}
	else
	{
		// Add extensions and exact names to the names filters
		QStringList nameFilters;
		for (int n=0; n< plugin->extensions().count(); ++n) nameFilters << "*." + plugin->extensions().at(n);
		for (int n=0; n< plugin->exactNames().count(); ++n) nameFilters << plugin->exactNames().at(n);
		fileSystemModel_.setNameFilters(nameFilters);

		if (mode_ == FileSelectorWidget::SaveSingleMode) emit(pluginOptionsAvailable(plugin->hasExportOptions()));
		else emit(pluginOptionsAvailable(plugin->hasImportOptions()));
	}

	emit(pluginSelectionChanged());
}

void FileSelectorWidget::on_FavouritesTable_currentItemChanged(QTableWidgetItem* current, QTableWidgetItem* previous)
{
	if (refreshing_ || (!current)) return;

	// Set current directory
	setCurrentDirectory(favourites_.at(current->data(Qt::UserRole).toInt()));

	updateWidgets();
}
