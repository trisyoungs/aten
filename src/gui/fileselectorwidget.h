/*
	*** FileSelector Widget
	*** src/gui/fileselectorwidget.h
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

#ifndef ATEN_FILESELECTOR_H
#define ATEN_FILESELECTOR_H

#include "gui/ui_fileselectorwidget.h"
#include "plugins/plugintypes.h"
#include "plugins/interfaces/fileplugin.h"
#include "templates/reflist.h"
#include "base/namespace.h"
#include <QDir>
#include <QFileSystemModel>

ATEN_USING_NAMESPACE

// Forward Declarations (Aten)
/* none */

// FileSelector Widget
class FileSelectorWidget : public QWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	FileSelectorWidget(QWidget* parent = 0);
	// Main form declaration
	Ui::FileSelectorWidget ui;
	// Selection mode enum
	enum SelectionMode { OpenSingleMode, OpenMultipleMode, SaveSingleMode };

	private:
	// Whether widget is currently refreshing
	bool refreshing_;


	/*
	 * Local Data / Functions
	 */
	private:
	// File system model
	QFileSystemModel fileSystemModel_;
	// Mode of operation
	SelectionMode mode_;
	// Current directory location
	QDir currentDirectory_;
	// Current filename selection
	QStringList selectedFilenames_;
	// Favourite places list
	static QStringList favourites_;

	public:
	// Set mode of file selector
	void setMode(SelectionMode mode, QDir startingDir);
	// Refresh plugins (filters) combo
	void refreshPlugins(const RefList<FilePluginInterface,KVMap>& filePlugins);
	// Set current directory of file selector
	void setCurrentDirectory(QString directory);
	// Clear selected filenames list
	void clearSelectedFilenames();
	// Set current filename selection
	void setSelectedFilename(QString filename);
	// Set current plugin selection
	void setSelectedPlugin(const FilePluginInterface* plugin);
	// Return selected files, including full path
	QStringList selectedFiles();
	// Return selected file plugin
	const FilePluginInterface* selectedPlugin();
	// Return reference to the options for the selected file plugin
	KVMap& selectedPluginOptions();
	// Clear favourite places list
	static void clearFavourites();
	// Add favourite place to list
	static void addFavourite(QString place);
	// Return favourite places list
	static const QStringList favourites();


	/*
	 * Widget Functions
	 */
	private slots:
	void on_DirectoryEdit_returnPressed();
	void on_DirectoryUpButton_clicked(bool checked);
	void on_DirectoryCreateButton_clicked(bool checked);
	void on_FileView_clicked(const QModelIndex& index);
	void on_FileView_doubleClicked(const QModelIndex& index);
	void on_FilesEdit_textChanged(QString textChanged);
	void on_FilesEdit_returnPressed();
	void on_FilterCombo_currentIndexChanged(int index);
	void on_FavouritesTable_currentItemChanged(QTableWidgetItem* current, QTableWidgetItem* previous);

	public slots:
	// Update widgets, e.g. after directory change
	void updateWidgets();
	// Resize columns of file view
	void resizeFileView(QString dummy = QString());
	// Custom context menu requested for favourites list
	void favouritesContextMenuRequested(const QPoint& point);


	/*
	 * Signals
	 */
	signals:
	void selectionMade(bool);
	void selectionValid(bool);
	void pluginSelectionChanged();
	void pluginOptionsAvailable(bool);
};

#endif
