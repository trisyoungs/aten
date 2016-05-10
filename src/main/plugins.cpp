/*
	*** Aten Plugins
	*** src/main/plugins.cpp
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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include <QDir>
#include <QPluginLoader>

// Load plugins
bool Aten::loadPlugins()
{
// 	foreach (QObject *plugin, QPluginLoader::staticInstances())

// 	// Get plugins directory, accounting for Windows and Mac platforms
// 	QDir pluginsDir = QDir(qApp->applicationDirPath());
// #if defined(Q_OS_WIN)
// 	if (pluginsDir.dirName().toLower() == "debug" || pluginsDir.dirName().toLower() == "release")
// 	pluginsDir.cdUp();
// #elif defined(Q_OS_MAC)
// 	if (pluginsDir.dirName() == "MacOS")
// 	{
// 		pluginsDir.cdUp();
// 		pluginsDir.cdUp();
// 		pluginsDir.cdUp();
// 	}
// #endif
// 	pluginsDir.cd("plugins");
// 
// 	foreach (QString fileName, pluginsDir.entryList(QDir::Files)) {
// 	QPluginLoader loader(pluginsDir.absoluteFilePath(fileName));
// 	QObject *plugin = loader.instance();
// 	if (plugin) {
// 		populateMenus(plugin);
// 		pluginFileNames += fileName;
// 	}
//     }
}