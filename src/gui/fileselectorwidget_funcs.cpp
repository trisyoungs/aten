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
#include <QFileSystemModel>

// Constructor
FileSelectorWidget::FileSelectorWidget(QWidget* parent) : QWidget(parent)
{
	ui.setupUi(this);

	fileSystemModel_.setRootPath("");
	ui.FileView->setModel(&fileSystemModel_);

	refreshing_ = false;
}

/*
 * Local Data / Functions
 */

// Set mode of file selector
void FileSelectorWidget::setMode(SelectionMode mode, PluginTypes::PluginType pluginType, QDir startingDir)
{
	mode_ = mode;
	pluginType_ = pluginType;

	// Set relevant selection mode for file view
	if (mode_ == FileSelectorWidget::FileSelectorWidget::OpenMultipleMode) ui.FileView->setSelectionMode(QTableView::ExtendedSelection);
	else ui.FileView->setSelectionMode(QTableView::SingleSelection);

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

// Return selected files, including full path
QStringList FileSelectorWidget::selectedFiles()
{
	QStringList fileList;
	for (int n=0; n<selectedFilenames_.count(); ++n) fileList << currentDirectory_.absoluteFilePath(selectedFilenames_.at(n));

	return fileList;
}

/*
 * Widget Functions
*/

// Update widgets, e.g. after directory change
void FileSelectorWidget::updateWidgets()
{
	// Favourites list
	// ATEN2 TODO

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

void FileSelectorWidget::on_FileView_clicked(const QModelIndex& index)
{
	// Get current model selection and reconstruct selected files list
	QItemSelectionModel* selectionModel = ui.FileView->selectionModel();
	QModelIndexList selectedRows = selectionModel->selectedRows();
	selectedFilenames_.clear();
	for (int n=0; n<selectedRows.count(); ++n) selectedFilenames_ << fileSystemModel_.fileName(selectedRows.at(n));

	updateWidgets();
}

void FileSelectorWidget::on_FileView_doubleClicked(const QModelIndex& index)
{
	QItemSelectionModel* selectionModel = ui.FileView->selectionModel();
	QModelIndexList selectedRows = selectionModel->selectedRows();
	selectedFilenames_.clear();
	for (int n=0; n<selectedRows.count(); ++n) selectedFilenames_ << fileSystemModel_.fileName(selectedRows.at(n));

	updateWidgets();

	emit(selectionMade(false));
}

void FileSelectorWidget::on_FilesEdit_returnPressed()
{
}

void FileSelectorWidget::on_FilesEdit_textChanged(QString textChanged)
{
	// Split current string into separate arguments
	LineParser parser;
	parser.getArgsDelim(LineParser::UseQuotes, ui.FilesEdit->text());

	selectedFilenames_.clear();
	for (int n = 0; n < parser.nArgs(); ++n) selectedFilenames_ << parser.argc(n);
}

void FileSelectorWidget::on_FilterCombo_currentIndexChanged(int index)
{
}