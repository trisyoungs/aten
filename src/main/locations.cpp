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


	// Construct a list of possible paths, including the current value of dataDir_
	QStringList paths;
	if (dataDir_ != QDir()) paths << dataDir_.path();
	else if (getenv("ATENDATA") != '\0') paths << getenv("ATENDATA");
	paths << "/usr/share/aten";
	paths << "/usr/local/share/aten";
	paths << "../share/aten";
	paths << QApplication::applicationDirPath() + "/../share/aten";
	paths << QApplication::applicationDirPath() + "/../SharedSupport";
	paths << QApplication::applicationDirPath() + "/..";

	// Check each one until we find one that exists
	for (int i=0; i < paths.size(); ++i)
	{
		QDir path = paths.at(i);
		Messenger::print("Checking for data directory '%s'...", qPrintable(path.absolutePath()));
		if (path.exists())
		{
			Messenger::print("Data directory exists at '%s' - using this path...", qPrintable(path.absolutePath()));
			dataDir_ = path;

			Messenger::enter("Aten::setDataDir()");
			return;
		}
	}
	Messenger::print("No valid data directory found in any location.");

	Messenger::enter("Aten::setDirectories()");
}
