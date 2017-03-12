/*
	*** Aten Partition-Specific Routines
	*** src/main/partitions.cpp
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

#include "main/aten.h"
#include <QDir>

ATEN_USING_NAMESPACE

// Load partitions
void Aten::loadPartitions()
{
	Messenger::enter("Aten::loadPartitions");

	bool found = false;
	int nFailed = 0;

	nPartitioningSchemesFailed_ = 0;
	failedPartitioningSchemes_.clear();

	// Generate default partition ('none')
	PartitioningScheme* ps = partitioningSchemes_.add();
	bool success = ps->schemeDefinition().generateFromString("string name = 'None', description = 'No partitioning'; int partition(double x, double y, double z) { return 0; } string partitionName(int id) { if (id == 0) return 'Whole Cell'; else return 'UNKNOWN'; } int nPartitions = 1, roughgrid[3] = { 2,2,2 }, finegrid[3] = {2,2,2};", "Default Partitioning", "");
	if (success) success = ps->initialiseFromProgram();
	if (!success)
	{
		Messenger::print("Failed to create default partition!");
		failedPartitioningSchemes_ << "Default";
		++nFailed;
		partitioningSchemes_.remove(ps);
	}
	
	QDir path = dataDir_.filePath("partitions");
	Messenger::print(Messenger::Verbose, "Looking for partitions in '%s'...", qPrintable(path.absolutePath()));
	nFailed = searchPartitionsDir(path);
	if (nFailed > 0) nPartitioningSchemesFailed_ += nFailed;

	// Try to load user partitions - we don't mind if the directory doesn't exist...
	path = atenDirectoryFile("partitions");
	Messenger::print(Messenger::Verbose, "Looking for user partitions in '%s'...", qPrintable(path.path()));
	nFailed = searchPartitionsDir(path);
	if (nFailed > 0) nPartitioningSchemesFailed_ += nFailed;

	Messenger::exit("Aten::loadPartitions");
}

// Parse filter index file (rooted in the path provided)
int Aten::searchPartitionsDir(QDir path)
{
	Messenger::enter("Aten::searchPartitionsDir");

	// First check - does this directory actually exist
	if (!path.exists())
	{
		Messenger::warn("Partitions directory '%s' does not exist.", qPrintable(path.path()));
		Messenger::exit("Aten::searchPartitionsDir");
		return -1;
	}

	// Partition the directory contents - show only files and exclude '.' and '..'
	int i, nFailed = 0;
	QString s = "Partitions --> ";
	QStringList partitionList = path.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<partitionList.size(); ++i)
	{
		// Construct Program...
		if (loadPartition(path.absoluteFilePath(partitionList.at(i)), partitionList.at(i))) s += partitionList.at(i) + "  ";
		else ++nFailed;
	}
	Messenger::print(s);

	Messenger::exit("Aten::searchPartitionsDir");
	return nFailed;
}

// Load partition from specified filename
bool Aten::loadPartition(QString fileName, QString name)
{
	Messenger::enter("Aten::loadPartition");

	// Construct partitions Program...
	PartitioningScheme* ps = partitioningSchemes_.add();
	bool success = ps->schemeDefinition().generateFromFile(fileName, name);
	
	if ((!success) || (!ps->initialiseFromProgram()))
	{
		Messenger::print("Failed to load partition from '%s'...", qPrintable(fileName));
		failedPartitioningSchemes_ << fileName;
		partitioningSchemes_.remove(ps);
		Messenger::exit("Aten::loadPartition");
		return false;
	}
	
	Messenger::exit("Aten::loadPartition");
	return true;
}

// Return status of partition load on startup
int Aten::nPartitioningSchemesFailed() const
{
	return nPartitioningSchemesFailed_;
}

// Return list of failed partitions
QStringList Aten::failedPartitioningSchemes() const
{
	return failedPartitioningSchemes_;
}

// Find partitioning scheme by name
PartitioningScheme* Aten::findPartitioningScheme(QString name)
{
	PartitioningScheme* scheme;
	for (scheme = partitioningSchemes_.first(); scheme != NULL; scheme = scheme->next) if (name == scheme->name()) break;
	if (scheme == NULL) 
	{
		Messenger::print("Error: No such scheme '%s'.", qPrintable(name));
		Messenger::print("Available schemes are:");
		for (scheme = partitioningSchemes_.first(); scheme != NULL; scheme = scheme->next) Messenger::print("  %10s  %s", qPrintable(scheme->name()), qPrintable(scheme->description()));
		return NULL;
	}
	else return scheme;
}

// Return number of partitioning schemes in the list
int Aten::nPartitioningSchemes()
{
	return partitioningSchemes_.nItems();
}

// Return first partitioning scheme in the list
PartitioningScheme* Aten::partitioningSchemes()
{
	return partitioningSchemes_.first();
}

// Return nth partitioning scheme in the list
PartitioningScheme* Aten::partitioningSchemes(int index)
{
	return partitioningSchemes_[index];
}

// Copy specified partitioning scheme and add it to the list
void Aten::addPartitioningScheme(PartitioningScheme& scheme)
{
	PartitioningScheme* newScheme = partitioningSchemes_.add();
	newScheme->copy(scheme);
}

// Return partitioning scheme for Pores tool
PartitioningScheme& Aten::poresPartitioningScheme()
{
	return poresPartitioningScheme_;
}
