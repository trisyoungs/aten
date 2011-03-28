/*
	*** Aten Partition-Specific Routines
	*** src/main/aten_partition.cpp
	Copyright T. Youngs 2007-2011

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
	int nfailed;

	nPartitionsFailed_ = 0;
	failedPartitions_.clear();

	// Generate default partition ('none')
	PartitioningScheme *ps = partitioningSchemes_.add();
	bool success = ps->schemeDefinition().generateFromString("string name = 'None', description = 'No partitioning'; int partition(double x, double y, double z) { return 0; } string partitionname(int id) { if (id == 0) return 'Whole Cell'; else return 'UNKNOWN'; } int npartitions = 1, roughgrid[3] = { 2,2,2 }, finegrid[3] = {2,2,2};", "Default Partitioning", FALSE);
	if (success) success = ps->initialise();
	if (!success)
	{
		msg.print("Failed to create default partition!\n");
		failedPartitions_.add()->set("none");
		nfailed ++;
		partitioningSchemes_.remove(ps);
	}
	
	// Construct a list of possible locations for the Partitions
	QStringList paths;
	if (!aten.dataDir_.isEmpty())
	{
		msg.print(Messenger::Verbose, "$ATENDATA points to '%s'.\n", dataDir_.get());
		paths << dataDir_.get();
	}
	else msg.print(Messenger::Verbose, "$ATENDATA has not been set. Default locations will be searched...\n");
	// Default locations
	paths << "/usr/share/aten";
	paths << "/usr/local/share/aten";
	paths << "../share/aten";
	paths << gui.app->applicationDirPath() + "/../share/aten";
	paths << gui.app->applicationDirPath() + "/../SharedSupport";

	for (int i=0; i < paths.size(); i++)
	{
		path.sprintf("%s/partitions", qPrintable(paths.at(i)));
		path = qPrintable(QDir::toNativeSeparators(path.get()));
		msg.print(Messenger::Verbose, "Looking for partitions in '%s'...\n", path.get());
		nfailed = parsePartitionsDir(path);
		if (nfailed == -1) continue;	// Directory not found
		found = TRUE;
		nPartitionsFailed_ += nfailed;
		dataDir_ = qPrintable(QDir::toNativeSeparators(paths.at(i)));
		break;
	}

	if (!found) msg.print("No partitions found in any known default locations.\n");

	// Try to load user partitions - we don't mind if the directory doesn't exist...
	path.sprintf("%s%c.aten%cpartitions%c", homeDir_.get(), PATHSEP, PATHSEP, PATHSEP);
	path = qPrintable(QDir::toNativeSeparators(path.get()));
	msg.print(Messenger::Verbose, "Looking for user partitions in '%s'...\n", path.get());
	nfailed = parsePartitionsDir(path);
	if (nfailed > 0) nPartitionsFailed_ += nfailed;

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
		if (success) success = ps->initialise();
		
		if (!success)
		{
			msg.print("Failed to load partitions from '%s'...\n", qPrintable(partitionlist.at(i)));
			failedPartitions_.add()->set( qPrintable(QDir::toNativeSeparators(filename)) );
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
	bool success = ps->schemeDefinition().generateFromFile(filename, filename, TRUE);
	if (success) success = ps->initialise();
	
	if (success)
	{
		msg.print("Failed to load Partitions from '%s'...\n", filename);
		failedPartitions_.add()->set( filename );
		partitioningSchemes_.remove(ps);
		msg.exit("Aten::openPartition");
		return FALSE;
	}
	msg.exit("Aten::openPartition");
	return TRUE;
}

// Return status of partition load on startup
int Aten::nPartitionsFailed() const
{
	return nPartitionsFailed_;
}

// Return list of failed partitions
Dnchar *Aten::failedPartitions() const
{
	return failedPartitions_.first();
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
