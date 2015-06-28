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
#include <QDir>

ATEN_USING_NAMESPACE

// Load includes
void Aten::openIncludes()
{
	Messenger::enter("Aten::openIncludes");

	nIncludesFailed_ = 0;
	failedIncludes_.clear();

	QDir includesDir = dataDirectoryFile("includes");
	Messenger::print(Messenger::Verbose, "Looking for includes in '%s'...", qPrintable(includesDir.path()));
	int nFailed = parseIncludeDir(includesDir);
	if (nFailed > 0) nIncludesFailed_ += nFailed;

	// Try to load user includes - we don't mind if the directory doesn't exist...
	includesDir = atenDirectoryFile("includes");
	Messenger::print(Messenger::Verbose, "Looking for user includes in '%s'...", qPrintable(includesDir.path()));
	nFailed = parseIncludeDir(includesDir);
	if (nFailed > 0) nIncludesFailed_ += nFailed;

	for (Tree* tree = includeFunctions_.functions(); tree != NULL; tree = tree->next)
		Messenger::print(Messenger::Verbose, "Included Function : %s", qPrintable(tree->name()));

	Messenger::exit("Aten::openIncludes");
}

// Parse include directory
int Aten::parseIncludeDir(QDir path)
{
	Messenger::enter("Aten::parseIncludeDir");

	int i, nFailed = 0;
	QString s = "Includes --> [" + path.absolutePath() + "] ";
	
	// First check - does this directory actually exist
	if (!path.exists())
	{
		Messenger::exit("Aten::parseIncludeDir");
		return -1;
	}

	// Include the directory contents - show only files and exclude '.' and '..'
	QStringList includeList = path.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i< includeList.size(); i++)
	{
		// Load filter Program...
		if (openInclude(path.absoluteFilePath(includeList.at(i)), includeList.at(i))) s += includeList.at(i) + "  ";
		else ++nFailed;
	}
	Messenger::print(s);

	Messenger::exit("Aten::parseIncludeDir");
	return nFailed;
}

// Load include from specified filename
bool Aten::openInclude(QString fileName, QString name)
{
	Messenger::enter("Aten::openInclude");

	// Construct includes Program...
	if (!includeFunctions_.generateFromFile(fileName, name, false, false))
	{
		Messenger::error("Failed to load includes from '%s'...", qPrintable(fileName));
		failedIncludes_ << fileName;
		Messenger::exit("Aten::openInclude");
		return false;
	}

	Messenger::exit("Aten::openInclude");
	return true;
}

// Return status of include load on startup
int Aten::nIncludesFailed() const
{
	return nIncludesFailed_;
}

// Return list of failed includes
QStringList Aten::failedIncludes() const
{
	return failedIncludes_;
}

// Find global include function by name
Tree* Aten::findIncludeFunction(QString name)
{
	return includeFunctions_.findFunction(name);
}

