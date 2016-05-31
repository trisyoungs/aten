/*
	*** Aten's locations
	*** src/main/locations.cpp
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
#include <QApplication>

ATEN_USING_NAMESPACE

// Return the home directory path
QDir Aten::homeDir() const
{
	return homeDir_;
}

// Return the working directory path
QDir Aten::workDir() const
{
	return workDir_;
}

// Return the data directory path
QDir Aten::dataDir() const
{
	return dataDir_;
}

// Return the plugin directory path
QDir Aten::pluginDir() const
{
	return pluginDir_;
}

// Return full path of file in data directory
QString Aten::dataDirectoryFile(QString filename)
{
	return QDir::toNativeSeparators(dataDir_.absoluteFilePath(filename));
}

// Return full path of file in user's Aten directory
QString Aten::atenDirectoryFile(QString filename)
{
	QDir atenDir = homeDir_.absoluteFilePath(atenDirName_);
	return QDir::toNativeSeparators(atenDir.absoluteFilePath(filename));
}

// Set/get necessary directories
void Aten::setDirectories()
{
	Messenger::enter("Aten::setDirectories()");

	// User's home directory
#ifdef _WIN32
	if (getenv("USERPROFILE") != '\0') homeDir_ = getenv("USERPROFILE");
	else homeDir_ = "C:\\";
#else
	if (getenv("HOME") != '\0') homeDir_ = getenv("HOME");	
	else homeDir_ = "/tmp";
#endif

	// Name of user's aten directory in user's home directory
#ifdef _WIN32
	atenDirName_ = "aten";
#else
	atenDirName_ = ".aten";
#endif

	// Working directory
	workDir_ = getenv("PWD");

	// Construct a list of possible paths for the data/ directory, including the current value of dataDir_
	QStringList dataDirPaths;
	if (dataDir_ != QDir()) dataDirPaths << dataDir_.path();
	else if (getenv("ATENDATA") != '\0') dataDirPaths << getenv("ATENDATA");
	dataDirPaths << "/usr/share/aten";
	dataDirPaths << "/usr/local/share/aten";
	dataDirPaths << "../share/aten";
	dataDirPaths << QApplication::applicationDirPath() + "/../share/aten";
	dataDirPaths << QApplication::applicationDirPath() + "/../SharedSupport";
	dataDirPaths << QApplication::applicationDirPath() + "/..";

	// Check each one until we find one that exists
	dataDir_ = QDir();
	for (int i=0; i < dataDirPaths.size(); ++i)
	{
		QDir path = dataDirPaths.at(i);
		Messenger::print("Checking for data directory '%s'...", qPrintable(path.absolutePath()));
		if (path.exists())
		{
			Messenger::print("Data directory exists at '%s' - using this path...", qPrintable(path.absolutePath()));
			dataDir_ = path;
			break;
		}
	}
	if (!dataDir_.exists()) Messenger::print("No valid data directory found in any location. Consider using the ATENDATA environment variable to set one.");

	// Construct a list of possible paths for the plugins directory, including the current value of pluginsDir_
	QStringList pluginDirPaths;
	if (pluginDir_ != QDir()) pluginDirPaths << pluginDir_.path();
	else if (getenv("ATENPLUGINS") != '\0') pluginDirPaths << getenv("ATENPLUGINS");
	pluginDirPaths << "/usr/lib64/aten/plugins";
	pluginDirPaths << "/usr/lib/aten/plugins";
	pluginDirPaths << "/usr/local/lib64/aten/plugins";
	pluginDirPaths << "/usr/local/lib/aten/plugins";
	pluginDirPaths << "../share/aten/plugins";
	pluginDirPaths << QApplication::applicationDirPath() + "/../share/aten/plugins";
	pluginDirPaths << QApplication::applicationDirPath() + "/../SharedSupport/plugins";
	pluginDirPaths << QApplication::applicationDirPath() + "/../plugins";
	pluginDirPaths << dataDirectoryFile("plugins");

	// Check each one until we find one that exists
	pluginDir_ = QDir();
	for (int i=0; i < pluginDirPaths.size(); ++i)
	{
		QDir path = pluginDirPaths.at(i);
		Messenger::print("Checking for plugin directory '%s'...", qPrintable(path.absolutePath()));
		if (path.exists())
		{
			Messenger::print("Plugin directory exists at '%s' - using this path...", qPrintable(path.absolutePath()));
			pluginDir_ = path;
			break;
		}
	}
	if (!pluginDir_.exists()) Messenger::print("No valid plugin directory found in any location. Consider using the ATENPLUGINS environment variable to set one.");

	Messenger::enter("Aten::setDirectories()");
}
