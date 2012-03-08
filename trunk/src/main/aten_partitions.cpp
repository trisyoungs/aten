/*
	*** Aten Partition-Specific Routines
	*** src/main/aten_partition.cpp
	Copyright T. Youngs 2007-2012

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
#include "gui/gui.h"

// Load partitions
void Aten::openPartitions()
{
	msg.enter("Aten::openPartitions");
	Dnchar path;
	bool found = FALSE;
	int nfailed = 0;

	nPartitioningSchemesFailed_ = 0;
	failedPartitioningSchemes_.clear();

	// Generate default partition ('none')
	PartitioningScheme *ps = partitioningSchemes_.add();
	bool success = ps->schemeDefinition().generateFromString("string name = 'None', description = 'No partitioning'; int partition(double x, double y, double z) { return 0; } string partitionName(int id) { if (id == 0) return 'Whole Cell'; else return 'UNKNOWN'; } int npartitions = 1, roughgrid[3] = { 2,2,2 }, finegrid[3] = {2,2,2};", "Default Partitioning", FALSE);
	if (success) success = ps->initialiseFromProgram();
	if (!success)
	{
		msg.print("Failed to create default partition!\n");
		failedPartitioningSchemes_.add()->set("none");
		nfailed ++;
		partitioningSchemes_.remove(ps);
	}
	
	// Construct a list of possible locations for the Partitions
	QStringList paths;
	if (!aten.dataDir_.isEmpty())
	{
		msg.print(Messenger::Verbose, "Aten::openPartitions() - data directory is '%s'.\n", dataDir_.get());
		paths << dataDir_.get();
	}
	else
	{
		msg.print(Messenger::Verbose, "Data directory has not yet been set. Default locations will be searched...\n");
		// Default locations
		paths << "/usr/share/aten";
		paths << "/usr/local/share/aten";
		paths << "../share/aten";
		paths << gui.application()->applicationDirPath() + "/../share/aten";
		paths << gui.application()->applicationDirPath() + "/../SharedSupport";
	}

	for (int i=0; i < paths.size(); i++)
	{
		path.sprintf("%s/partitions", qPrintable(paths.at(i)));
		path = qPrintable(QDir::toNativeSeparators(path.get()));
		msg.print(Messenger::Verbose, "Looking for partitions in '%s'...\n", path.get());
		nfailed = parsePartitionsDir(path);
		if (nfailed == -1) continue;	// Directory not found
		found = TRUE;
		nPartitioningSchemesFailed_ += nfailed;
		dataDir_ = qPrintable(QDir::toNativeSeparators(paths.at(i)));
		break;
	}

	if (!found) msg.print("No partitions found in any known default locations.\n");

	// Try to load user partitions - we don't mind if the directory doesn't exist...
	path.sprintf("%s%c%s%cpartitions%c", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP, PATHSEP);
	path = qPrintable(QDir::toNativeSeparators(path.get()));
	msg.print(Messenger::Verbose, "Looking for user partitions in '%s'...\n", path.get());
	nfailed = parsePartitionsDir(path);
	if (nfailed > 0) nPartitioningSchemesFailed_ += nfailed;

	msg.exit("Aten::openPartitions");
}

// Parse filter index file (rooted in the path provided)
int Aten::parsePartitionsDir(const char *path)
{
	msg.enter("Aten::parsePartitionsDir");
	int i, nfailed = 0;
	Dnchar s("--> ");
	// First check - does this directory actually exist
	QDir partitiondir(path);
	if (!partitiondir.exists())
	{
		msg.exit("Aten::parsePartitionsDir");
		return -1;
	}
	// Partition the directory contents - show only files and exclude '.' and '..'
	QStringList partitionlist = partitiondir.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<partitionlist.size(); i++)
	{
		// Construct Program...
		QString filename(path);
		filename += "/";
		filename += partitionlist.at(i);
		PartitioningScheme *ps = partitioningSchemes_.add();
		bool success = ps->schemeDefinition().generateFromFile(qPrintable(QDir::toNativeSeparators(filename)), qPrintable(partitionlist.at(i)), FALSE);
		if (success) success = ps->initialiseFromProgram();
		
		if (!success)
		{
			msg.print("Failed to load partitions from '%s'...\n", qPrintable(partitionlist.at(i)));
			failedPartitioningSchemes_.add()->set( qPrintable(QDir::toNativeSeparators(filename)) );
			nfailed ++;
			partitioningSchemes_.remove(ps);
		}
		else
		{
			// Add on a bit of useful text to print out
			s.strcatf("%s  ", qPrintable(partitionlist.at(i)));
		}
	}
	s += '\n';
	msg.print(Messenger::Verbose, s);

	msg.exit("Aten::parsePartitionsDir");
	return nfailed;
}

// Load partition from specified filename
bool Aten::openPartition(const char *filename)
{
	msg.enter("Aten::openPartition");
	// Construct partitions Program...
	PartitioningScheme *ps = partitioningSchemes_.add();
	bool success = ps->schemeDefinition().generateFromFile(filename, filename, FALSE);
// 	if (success) success = 
	
	if ((!success) || (!ps->initialiseFromProgram()))
	{
		msg.print("Failed to load partition from '%s'...\n", filename);
		failedPartitioningSchemes_.add()->set( filename );
		partitioningSchemes_.remove(ps);
		msg.exit("Aten::openPartition");
		return FALSE;
	}
	
	msg.exit("Aten::openPartition");
	return TRUE;
}

// Return status of partition load on startup
int Aten::nPartitioningSchemesFailed() const
{
	return nPartitioningSchemesFailed_;
}

// Return list of failed partitions
Dnchar *Aten::failedPartitioningSchemes() const
{
	return failedPartitioningSchemes_.first();
}

// Find partitioning scheme by name
PartitioningScheme  *Aten::findPartitioningScheme(const char *name)
{
	PartitioningScheme *scheme;
	for (scheme = partitioningSchemes_.first(); scheme != NULL; scheme = scheme->next) if (strcmp(name, scheme->name()) == 0) break;
	if (scheme == NULL) 
	{
		msg.print("Error: No such scheme '%s'.\n", name);
		msg.print("Available schemes are:\n");
		for (scheme = partitioningSchemes_.first(); scheme != NULL; scheme = scheme->next) msg.print("  %10s  %s\n", scheme->name(), scheme->description());
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
PartitioningScheme *Aten::partitioningSchemes()
{
	return partitioningSchemes_.first();
}

// Return nth partitioning scheme in the list
PartitioningScheme *Aten::partitioningSchemes(int index)
{
	return partitioningSchemes_[index];
}

// Copy specified partitioning scheme and add it to the list
void Aten::addPartitioningScheme(PartitioningScheme &scheme)
{
	PartitioningScheme *newScheme = partitioningSchemes_.add();
	newScheme->copy(scheme);
}

