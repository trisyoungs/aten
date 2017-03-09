/*
	*** External Command Definition
	*** src/base/externalcommand.cpp
	Copyright T. Youngs 2007-2017

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

#include "base/externalcommand.h"
#include <QFileInfo>
#include <QDir>
#include <QStandardPaths>

ATEN_USING_NAMESPACE

// Constructor
ExternalCommand::ExternalCommand() : ListItem<ExternalCommand>()
{
}

/*
 * Definition
 */

// Set short name of command
void ExternalCommand::setName(QString name)
{
	name_= name;
}

// Return short name of command
QString ExternalCommand::name()
{
	return name_;
}

// Set executable name
void ExternalCommand::setExecutable(QString executable)
{
	executable_ = executable;
}

// Return executable name
QString ExternalCommand::executable()
{
	return executable_;
}

// Add search path
void ExternalCommand::addSearchPath(QString path)
{
	searchPaths_ << path;
}

// Return list of search paths
QStringList ExternalCommand::searchPaths()
{
	return searchPaths_;
}

// Return absolute path to command (using search paths)
QString ExternalCommand::absoluteExecutable()
{
	// Go through specified paths first
	foreach(QString path, searchPaths_)
	{
		QDir searchDir = path;
		if (searchDir.exists(executable_)) return searchDir.absoluteFilePath(executable_);
	}

	// Now search system paths
	QString absolutePath = QStandardPaths::locate(QStandardPaths::ApplicationsLocation, executable_, QStandardPaths::LocateFile);

	return absolutePath;
}

// Set arguments
void ExternalCommand::setArguments(QString arguments)
{
	arguments_ = arguments;
}

// Return argumenets list
QString ExternalCommand::arguments()
{
	return arguments_;
}
