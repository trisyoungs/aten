/*
	*** TProcess Functions
	*** src/gui/tprocess_funcs.cpp
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

#include "gui/tprocess.uih"
#include "base/messenger.h"
#include "base/sysfunc.h"
#include <stdio.h>

ATEN_USING_NAMESPACE

// Constructor
TProcess::TProcess(QObject *parent) : QProcess(parent)
{
	finished_ = false;
	outputFileSpecified_ = false;
	QObject::connect(this, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(processFinished(int,QProcess::ExitStatus)));
}

void TProcess::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	finished_ = true;
}

// Execute specified command
bool TProcess::execute(QString command, QString args, QString outputfile)
{
	// Check on state of any existing process
	if (state() != QProcess::NotRunning)
	{
		printf("Existing process running - terminating first...\n");
		kill();
	}

	finished_ = false;

	// Set up outputfile (if specified)
	outputFile_.close();
	stream_.flush();
	stream_.setDevice(NULL);
	if (outputfile.isEmpty()) outputFileSpecified_ = false;
	else
	{
		outputFile_.setFileName(outputfile);
		outputFileSpecified_ = true;
	}
	
	// Attempt to start process
	if (args == NULL) start(command);
	else
	{
		QString cmdPlusArgs = command + " " + args;
		start(cmdPlusArgs);
	}
	if (!waitForStarted(1000))
	{
		printf("Error: Failed to run command '%s'\n", qPrintable(command));
		return false;
	}

	return true;
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
		if (!stdOutput_.isEmpty()) return true;
		stdOutput_ = readAllStandardOutput();
		if (!stdOutput_.isEmpty()) return true;
		return false;
	}
	
	// Second check - if output file doesn't exist, no output to be had
	if (!outputFile_.exists()) return false;
	
	// Third check - able to open output file
	if ((!outputFile_.isOpen()) && (!outputFile_.open(QIODevice::ReadOnly | QIODevice::Text))) return false;
	
	// So, has the device already been assigned to the QTextStream?
	if (stream_.device() != &outputFile_) stream_.setDevice(&outputFile_);
	
	// Is there any data to read?
	if (stream_.atEnd()) return false;
	
	// There is!
	return true;
}

// Return next line from output file
void TProcess::printLineToMessages()
{
	// Standard output takes priority over file...
	if (stdOutput_.isEmpty())
	{
		QString line = stream_.readLine() + "\n";
		Messenger::print(qPrintable(line));
	}
	else
	{
		Messenger::print(stdOutput_.data());
		stdOutput_.clear();
	}
}
