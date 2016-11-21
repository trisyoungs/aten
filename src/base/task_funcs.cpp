/*
	*** Task
	*** src/base/task_funcs.cpp
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

#include "base/messenger.h"
#include "base/sysfunc.h"
#include <QTextStream>
#include <stdarg.h>
#include <stdio.h>
#include <QStringList>
#include <QByteArray>

// Constructor
Task::Task()
{
	type_ = Task::SteppedTask;
	currentStep_ = 0;
	nSteps_ = 0;
	canceled_ = false;
	commandFinished_ = false;
	commandOutputFileSpecified_ = false;
	QObject::connect(this, SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(processFinished(int,QProcess::ExitStatus)));
}

/*
 * Basic Data
 */

// Return type of task
Task::TaskType Task::type()
{
	return type_;
}

// Return title
QString Task::title()
{
	return title_;
}

// Return estimated time until completion of task (as string)
QString Task::etaText()
{
	// If no steps yet completed, no way to estimate
	if (currentStep_ == 0) return "??:??:??";

	// Get time difference between last known step and the beginning of the task, and project the total time required
	int mSecs = startTime_.msecsTo(lastStepTime_);
	double mSecsPerStep = mSecs / double(currentStep_);
	QDateTime endTime = startTime_.addMSecs(mSecsPerStep*nSteps_);

	// Calculate number of seconds to end time, and format into string
	int nSecs = QDateTime::currentDateTime().secsTo(endTime);
	int nHours = nSecs/3600;
	nSecs -= nHours*3600;
	int nMinutes = nSecs/60;
	nSecs -= nMinutes*60;
	return QString("%1:%2:%3").arg(nHours, 2, 10, QChar('0')).arg(nMinutes, 2, 10, QChar('0')).arg(nSecs, 2, 10, QChar('0'));
}

// Return timestamp of task creation
QDateTime Task::startTime()
{
	return startTime_;
}

// Cancel task (set flag)
void Task::cancel()
{
	canceled_ = true;
}

// Return if task has been canceled
bool Task::canceled()
{
	return canceled_;
}

/*
 * Stepped Task
 */

// Initialise stepped task
void Task::initialise(QString title, int nSteps)
{
	type_ = Task::SteppedTask;
	title_ = title;
	currentStep_ = 0;
	nSteps_ = nSteps;
	completion_ = 0.0;
	startTime_ = QDateTime::currentDateTime();
	lastStepTime_ = QDateTime();
}

// Return total number of steps in task
int Task::nSteps()
{
	return nSteps_;
}

// Return current step in task
int Task::currentStep()
{
	return currentStep_;
}

// Return timestamp of last completed step
QDateTime Task::lastStepTime()
{
	return lastStepTime_;
}

// Return percentage completion
double Task::completion()
{
	return completion_;
}

// Update stepped task, returning if canceled by the user
void Task::update(int newCurrentStep)
{
	currentStep_ = newCurrentStep;
	completion_ = 100.0 * currentStep_ / nSteps_;
	lastStepTime_ = QDateTime::currentDateTime();
}

// Increment stepped task progress, returning if canceled by the user
void Task::increment(int deltaSteps)
{
	update(currentStep_ + deltaSteps);
}

/*
 * External Command
 */

void Task::processFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
	commandFinished_ = true;
	commandFailed_ = (exitStatus != QProcess::NormalExit);

	if (commandOutputAvailable()) printLineToMessages();

	Messenger::print("Command finished with exitCode = %i, status = %i\n", exitCode, exitStatus);
}

// Initialise and execute specified command
bool Task::initialiseCommand(QString title, QString command, QString args, QString outputFile)
{
	// Check on state of any existing process
	if (state() != QProcess::NotRunning)
	{
		printf("Existing process running - terminating first...\n");
		kill();
	}

	title_ = title;
	type_ = Task::ExternalTask;

	commandFinished_ = false;
	commandFailed_ = false;

	// Set up outputfile (if specified)
	commandOutputFile_.close();
	commandStream_.flush();
	commandStream_.setDevice(NULL);
	if (outputFile.isEmpty()) commandOutputFileSpecified_ = false;
	else
	{
		commandOutputFile_.setFileName(outputFile);
		commandOutputFileSpecified_ = true;
	}
	
	// Attempt to start process
	if (args.isEmpty()) start(command);
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

// Return whether command has finished
bool Task::commandFinished()
{
	return commandFinished_;
}

// Return whether command failed
bool Task::commandFailed()
{
	return commandFailed_;
}

// Return whether new (any) output is available for reading from the output file
bool Task::commandOutputAvailable()
{
	// First check - was an outputfile specified on execute?
	// If it wasn't, is there any stdoutput to read?
	if (!commandOutputFileSpecified_)
	{
		// Is some old output already available?
		if (!commandStdOutput_.isEmpty()) return true;
		commandStdOutput_ = readAllStandardOutput() + readAllStandardError();
		if (!commandStdOutput_.isEmpty()) return true;
		return false;
	}
	
	// Second check - if output file doesn't exist, no output to be had
	if (!commandOutputFile_.exists()) return false;
	
	// Third check - able to open output file
	if ((!commandOutputFile_.isOpen()) && (!commandOutputFile_.open(QIODevice::ReadOnly | QIODevice::Text))) return false;
	
	// So, has the device already been assigned to the QTextStream?
	if (commandStream_.device() != &commandOutputFile_) commandStream_.setDevice(&commandOutputFile_);
	
	// Is there any data to read?
	if (commandStream_.atEnd()) return false;
	
	// There is!
	return true;
}

// Return next line from output file
void Task::printLineToMessages()
{
	// Standard output takes priority over file...
	if (commandStdOutput_.isEmpty())
	{
		QString line = commandStream_.readLine() + "\n";
		Messenger::print(qPrintable(line));
	}
	else
	{
		QString line(commandStdOutput_);
		Messenger::print(line);
		commandStdOutput_.clear();
	}
}