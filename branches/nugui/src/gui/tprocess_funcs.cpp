/*
	*** TProcess
	*** src/gui/tprocess.cpp
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

#include "gui/tprocess.uih"
#include "base/messenger.h"
#include "base/dnchar.h"
#include <stdio.h>

// Constructor
TProcess::TProcess(QObject *parent) : QProcess(parent)
{
	finished_ = FALSE;
	outputFileSpecified_ = FALSE;
	QObject::connect(this, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(processFinished(int,QProcess::ExitStatus)));
}

void TProcess::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	finished_ = TRUE;
}

// Execute specified command
bool TProcess::execute(const char *command, const char *args, const char *outputfile)
{
	// Check on state of any existing process
	if (state() != QProcess::NotRunning)
	{
		printf("Existing process running - terminating first...\n");
		kill();
	}

	finished_ = FALSE;

	// Set up outputfile (if specified)
	outputFile_.close();
	stream_.flush();
	stream_.setDevice(NULL);
	if (outputfile == NULL) outputFileSpecified_ = FALSE;
	else
	{
		outputFile_.setFileName(outputfile);
		outputFileSpecified_ = TRUE;
	}
	
	// Attempt to start process
	if (args == NULL) start(command);
	else
	{
		Dnchar cmdplusargs(-1,"%s %s", command, args);
		start(cmdplusargs.get());
	}
	if (!waitForStarted(1000))
	{
		printf("Error: Failed to run command '%s'\n", command);
		return FALSE;
	}

	return TRUE;
}

// Return whether process has finished
bool TProcess::finished()
{
	return finished_;
}

// Return whether new (any) output is available for reading from the output file
bool TProcess::outputAvailable()
{
	// First check - was an outputfile specified on execute?
	// If it wasn't, is there any stdoutput to read?
	if (!outputFileSpecified_)
	{
		// Is some old output already available?
		if (stdOutput_.data() != '\0') return TRUE;
		stdOutput_ = readAllStandardOutput();
		if (stdOutput_.data() != '\0') return TRUE;
		return FALSE;
	}
	
	// Second check - if output file doesn't exist, no output to be had
	if (!outputFile_.exists()) return FALSE;
	
	// Third check - able to open output file
	if ((!outputFile_.isOpen()) && (!outputFile_.open(QIODevice::ReadOnly | QIODevice::Text))) return FALSE;
	
	// So, has the device already been assigned to the QTextStream?
	if (stream_.device() != &outputFile_) stream_.setDevice(&outputFile_);
	
	// Is there any data to read?
	if (stream_.atEnd()) return FALSE;
	
	// There is!
	return TRUE;
}

// Return next line from output file
void TProcess::printLineToMessages()
{
	// Standard output takes priority over file...
	if (stdOutput_.data() != '\0')
	{
		msg.print(stdOutput_.data());
		stdOutput_.clear();
	}
	else
	{
		QString line = stream_.readLine() + "\n";
		msg.print(qPrintable(line));
	}
}
