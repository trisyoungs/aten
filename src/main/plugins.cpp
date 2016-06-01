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
#include "plugins/interfaces.h"
#include <QDir>
#include <QPluginLoader>

// Load specified plugin and register its functions
bool Aten::loadPlugin(QString fileName)
{
	Messenger::print(Messenger::Verbose, "Querying plugin file '%s'...\n", qPrintable(fileName));

	// Create a pluginloader for the filename provided
	QPluginLoader loader(fileName);

	QObject *plugin = loader.instance();
	if (!plugin)
	{
		Messenger::error("File '%s' does not appear to be a valid plugin.", qPrintable(fileName));
		Messenger::print(loader.errorString());
		return false;
	}

	// Determine which type of plugin this is by attempting to cast it to the available types
	FilePluginInterface* filePlugin = qobject_cast<FilePluginInterface *>(plugin);
	if (filePlugin)
	{
		pluginStore_.registerFilePlugin(filePlugin);
	}
	return true;
}

// Load plugins
void Aten::loadPlugins()
{
	Messenger::enter("Aten::loadPlugins");

	nPluginsFailed_ = 0;
	failedPlugins_.clear();

	// Load main plugins
	Messenger::print(Messenger::Verbose, "Looking for plugins in '%s'...", qPrintable(pluginDir_.path()));
	int nFailed = searchPluginsDir(pluginDir_);
	if (nFailed > 0) nPluginsFailed_ += nFailed;

	// Try to load user plugins - we don't mind if the directory doesn't exist...
	QDir userPluginsDir = atenDirectoryFile("plugins");
	Messenger::print(Messenger::Verbose, "Looking for user plugins in '%s'...", qPrintable(userPluginsDir.path()));
	nFailed = searchPluginsDir(userPluginsDir);
	if (nFailed > 0) nPluginsFailed_ += nFailed;

	Messenger::exit("Aten::loadPlugins");
}

// Search directory for plugins
int Aten::searchPluginsDir(QDir path)
{
	Messenger::enter("Aten::searchPluginsDir");

	int i, nFailed = 0;
	QString s = "Plugins --> [" + path.absolutePath() + "] ";
	
	// First check - does this directory actually exist
	if (!path.exists())
	{
		Messenger::exit("Aten::searchPluginsDir");
		return -1;
	}

	// Plugins the directory contents - show only files and exclude '.' and '..', and also the potential README
	QStringList pluginsList = path.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	pluginsList.removeOne("README");
	for (i=0; i< pluginsList.size(); i++)
	{
		QFileInfo fileInfo(pluginsList.at(i));
		if ((fileInfo.suffix() != "so") && (fileInfo.suffix() != "dll")) continue;

		if (loadPlugin(path.absoluteFilePath(pluginsList.at(i)))) s += pluginsList.at(i) + "  ";
		else ++nFailed;
	}
	Messenger::print(s);

	Messenger::exit("Aten::searchPluginsDir");

	return nFailed;
}

// Return plugin store reference
const PluginStore& Aten::pluginStore()
{
	return pluginStore_;
}