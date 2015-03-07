/*
	*** Aten Include-Specific Routines
	*** src/main/aten_includes.cpp
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

#include "main/aten.h"
#include <QtGui/QApplication>
#include <QtCore/QDir>

ATEN_USING_NAMESPACE

// Load includes
void Aten::openIncludes()
{
	Messenger::enter("Aten::openIncludes");
	Dnchar path;
	bool found = FALSE;
	int nfailed;

	nIncludesFailed_ = 0;
	failedIncludes_.clear();

	// Construct a list of possible locations for the Includes
	QStringList paths;
	if (!dataDir_.isEmpty())
	{
		Messenger::print(Messenger::Verbose, "Aten::openIncludes() - data directory is '%s'.\n", dataDir_.get());
		paths << dataDir_.get();
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Data directory has not yet been set. Default locations will be searched...\n");
		// Default locations
		paths << "/usr/share/aten";
		paths << "/usr/local/share/aten";
		paths << "../share/aten";
		paths << QApplication::applicationDirPath() + "/../share/aten";
		paths << QApplication::applicationDirPath() + "/../SharedSupport";
	}

	for (int i=0; i < paths.size(); i++)
	{
		path.sprintf("%s/includes", qPrintable(paths.at(i)));
		path = qPrintable(QDir::toNativeSeparators(path.get()));
		Messenger::print(Messenger::Verbose, "Looking for includes in '%s'...\n", path.get());
		nfailed = parseIncludeDir(path);
		if (nfailed == -1) continue;	// Directory not found
		found = TRUE;
		nIncludesFailed_ += nfailed;
		dataDir_ = qPrintable(QDir::toNativeSeparators(paths.at(i)));
		break;
	}

	if (!found) Messenger::print("No includes found in any known default locations.\n");

	// Try to load user includes - we don't mind if the directory doesn't exist...
	path.sprintf("%s%c%s%cincludes%c", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP, PATHSEP);
	path = qPrintable(QDir::toNativeSeparators(path.get()));
	Messenger::print(Messenger::Verbose, "Looking for user includes in '%s'...\n", path.get());
	nfailed = parseIncludeDir(path);
	if (nfailed > 0) nIncludesFailed_ += nfailed;

	for (Tree* tree = includeFunctions_.functions(); tree != NULL; tree = tree->next)
		Messenger::print(Messenger::Verbose, " Included Function : %s\n", tree->name());

	Messenger::exit("Aten::openIncludes");
}

// Parse include directory
int Aten::parseIncludeDir(const char* path)
{
	Messenger::enter("Aten::parseIncludeDir");
	int i, nfailed = 0;
	Dnchar s(-1, "INCLUDES --> [%s] ", path);
	// First check - does this directory actually exist
	QDir includedir(path);
	if (!includedir.exists())
	{
		Messenger::exit("Aten::parseIncludeDir");
		return -1;
	}
	// Include the directory contents - show only files and exclude '.' and '..'
	QStringList includelist = includedir.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<includelist.size(); i++)
	{
		// Construct filter Program...
		QString filename(path);
		filename += "/";
		filename += includelist.at(i);
		if (!includeFunctions_.generateFromFile(qPrintable(QDir::toNativeSeparators(filename)), qPrintable(includelist.at(i)), TRUE, FALSE, FALSE))
		{
			Messenger::print("Failed to load includes from '%s'...\n", qPrintable(includelist.at(i)));
			failedIncludes_.add()->set( qPrintable(QDir::toNativeSeparators(filename)) );
			nfailed ++;
		}
		else
		{
			// Add on a bit of useful text to print out
			s.strcatf("%s  ", qPrintable(includelist.at(i)));
		}
	}
	s += '\n';
	Messenger::print(s);

	Messenger::exit("Aten::parseIncludeDir");
	return nfailed;
}

// Load include from specified filename
bool Aten::openInclude(const char* filename)
{
	Messenger::enter("Aten::openInclude");
	// Construct includes Program...
	if (!includeFunctions_.generateFromFile(filename, filename, TRUE, FALSE, FALSE))
	{
		Messenger::print("Failed to load includes from '%s'...\n", filename);
		failedIncludes_.add()->set( filename );
		Messenger::exit("Aten::openInclude");
		return FALSE;
	}
	Messenger::exit("Aten::openInclude");
	return TRUE;
}

// Return status of include load on startup
int Aten::nIncludesFailed() const
{
	return nIncludesFailed_;
}

// Return list of failed includes
Dnchar* Aten::failedIncludes() const
{
	return failedIncludes_.first();
}

// Find global include function by name
Tree* Aten::findIncludeFunction(const char* name)
{
	return includeFunctions_.findFunction(name);
}

