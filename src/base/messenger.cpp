/*
	*** Messaging Routines
	*** src/base/messenger.cpp
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

#include "base/messenger.h"
#include "base/sysfunc.h"
#include "gui/progress.h"
#include <QTextStream>
#include <stdarg.h>
#include <stdio.h>

ATEN_USING_NAMESPACE

// Static Members
int Messenger::outputTypes_ = 0;
int Messenger::callLevel_ = 0;
bool Messenger::quiet_ = false;
bool Messenger::printToConsole_ = true;
int Messenger::bufferSize_ = 100;
QList<Message> Messenger::messageBuffer_;
AtenProgress* Messenger::atenProgress_ = NULL;
RefList<Task,int> Messenger::tasks_;
int Messenger::taskPoint_ = -1;
QString Messenger::cliProgressText_;
int Messenger::messageBufferPoint_ = 0;
int Messenger::backPrintedMessagePoint_ = -1;

// Message output types
const char* OutputTypeKeywords[] = { "all", "calls", "commands", "parse", "typing", "verbose" };
Messenger::OutputType Messenger::outputType(QString s, bool reportError)
{
	Messenger::OutputType ot = (Messenger::OutputType) enumSearch("output type",Messenger::nOutputTypes,OutputTypeKeywords,s);
	if ((ot == Messenger::nOutputTypes) && reportError) enumPrintValid(Messenger::nOutputTypes,OutputTypeKeywords);
	return ot;
}
const char* Messenger::outputType(Messenger::OutputType ot)
{
	return OutputTypeKeywords[ot];
}

// Constructor
Messenger::Messenger()
{
}

/*
 * Output Control
 */

// Add a debug level to the debug output bitvector
void Messenger::addOutputType(Messenger::OutputType outputType)
{
	// Convert output type into bit if necessary
	if (outputType == Messenger::All) outputTypes_ = (1 << nOutputTypes) - 1;
	else if (!(outputTypes_&(1 << outputType))) outputTypes_ += (1 << outputType);
}

// Remove a debug level from the debug output bitvector
void Messenger::removeOutputType(Messenger::OutputType outputType)
{
	// Convert output type into bit if necessary
	if (outputType == Messenger::All) outputTypes_ = 0;
	else if (outputTypes_&(1 << outputType)) outputTypes_ -= (1 << outputType);
}

// Returns whether the specified debug level is set
bool Messenger::isOutputActive(Messenger::OutputType outputType)
{
	return outputTypes_&(1 << outputType);
}

// Set status of quiet mode
void Messenger::setQuiet(bool quiet)
{
	quiet_ = quiet;
}

// Return status of quiet mode
bool Messenger::isQuiet()
{
	return quiet_;
}

// Set status console printing
void Messenger::setPrintToConsole(bool printToConsole)
{
	printToConsole_ = printToConsole;
}

/*
 * Messaging Functions
 */

// Clear message buffer
void Messenger::clearMessageBuffer()
{
	messageBuffer_.clear();
}

// Add message to buffer
void Messenger::addToBuffer(QString message, Message::MessageType type)
{
	// Add to buffer (at start), and reduce buffer to max allowable size
	messageBuffer_.prepend(Message(message, type));
	while (messageBuffer_.count() > bufferSize_) messageBuffer_.removeLast();
	++messageBufferPoint_;
}

// Output message to console
void Messenger::outputMessage(QString message)
{
	if (!printToConsole_) return;

	// Prepend message with current CLI progress?
	if (tasks_.nItems()) QTextStream(stdout) << cliProgressText_ << message << endl;
	else QTextStream(stdout) << message << endl;
}

// Return list of messages in buffer
QList<Message>& Messenger::messageBuffer()
{
	return messageBuffer_;
}

// Return number of lines currently in buffer
int Messenger::nMessagesBuffered()
{
	return messageBuffer_.count();
}

// Print formatted normal message
void Messenger::print(const char* fmtString, ...)
{
	// If program is in quiet mode, don't print anything
	if (quiet_) return;

	// Create message
	va_list arguments;
	va_start(arguments, fmtString);
	QString message;
	message.vsprintf(fmtString, arguments);
	va_end(arguments);

	// Add to message buffer
	addToBuffer(message);

	// Output message
	outputMessage(message);
}

// Print formatted warning message
void Messenger::warn(const char* fmtString, ...)
{
	// If program is in quiet mode, don't print anything
	if (quiet_) return;

	// Create message
	va_list arguments;
	va_start(arguments, fmtString);
	QString message;
	message.vsprintf(fmtString, arguments);
	va_end(arguments);

	// Add to message buffer
	addToBuffer(message, Message::WarningMessage);

	// Output message
	outputMessage(message);
}

// Print formatted error message
void Messenger::error(const char* fmtString, ...)
{
	// If program is in quiet mode, don't print anything
	if (quiet_) return;

	// Create message
	va_list arguments;
	va_start(arguments, fmtString);
	QString message;
	message.vsprintf(fmtString, arguments);
	va_end(arguments);

	// Add to message buffer
	addToBuffer(message, Message::ErrorMessage);

	// Output message
	outputMessage(message);
}

// Print normal message (QString)
void Messenger::print(QString message)
{
	// If program is in quiet mode, don't print anything
	if (quiet_) return;

	// Add to message buffer
	addToBuffer(message);

	// Output message
	outputMessage(message);
}

// Print warning message (QString)
void Messenger::warn(QString message)
{
	// If program is in quiet mode, don't print anything
	if (quiet_) return;

	// Add to message buffer
	addToBuffer(message, Message::WarningMessage);

	// Output message
	outputMessage(message);
}

// Print error message (QString)
void Messenger::error(QString message)
{
	// If program is in quiet mode, don't print anything
	if (quiet_) return;

	// Add to message buffer
	addToBuffer(message, Message::ErrorMessage);

	// Output message
	outputMessage(message);
}

// Print normal message in specific output level
void Messenger::print(Messenger::OutputType outputType, const char* fmtString, ...)
{
	// If program is in quiet mode, or the output type isn't active, return now
	if (quiet_ || (! isOutputActive(outputType))) return;

	// Create message
	va_list arguments;
	va_start(arguments, fmtString);
	QString message;
	message.vsprintf(fmtString, arguments);
	va_end(arguments);

	// Add to message buffer
	addToBuffer(message);

	// Output message
	outputMessage(message);
}

// Print warning message in specific output level
void Messenger::warn(Messenger::OutputType outputType, const char* fmtString, ...)
{
	// If program is in quiet mode, or the output type isn't active, return now
	if (quiet_ || (! isOutputActive(outputType))) return;

	// Create message
	va_list arguments;
	va_start(arguments, fmtString);
	QString message;
	message.vsprintf(fmtString, arguments);
	va_end(arguments);

	// Add to message buffer
	addToBuffer(message, Message::WarningMessage);

	// Output message
	outputMessage(message);
}

// Print error message in specific output level
void Messenger::error(Messenger::OutputType outputType, const char* fmtString, ...)
{
	// If program is in quiet mode, or the output type isn't active, return now
	if (quiet_ || (! isOutputActive(outputType))) return;

	// Create message
	va_list arguments;
	va_start(arguments, fmtString);
	QString message;
	message.vsprintf(fmtString, arguments);
	va_end(arguments);

	// Add to message buffer
	addToBuffer(message, Message::ErrorMessage);

	// Output message
	outputMessage(message);
}

// Function enter
void Messenger::enter(const char* callname)
{
	if (!isOutputActive(Messenger::Calls)) return;

	printf("%2i ",callLevel_);
	for (int n=0; n<callLevel_; ++n) printf("--");
	printf("Begin : %s...\n", callname);
	++callLevel_;
}

// Function leave
void Messenger::exit(const char* callName)
{
	if (!isOutputActive(Messenger::Calls)) return;

	--callLevel_;
	printf("%2i ", callLevel_);
	for (int n=0; n<callLevel_; ++n) printf("--");
	printf("End   : %s.\n", callName);
}

/*
 * Progress Indication
 */

// Whether the progress indicator should be shown
bool Messenger::progressIndicatorRequired()
{
	if (tasks_.nItems() == 0) return false;

	// Check start time of first task
	RefListItem<Task,int>* ri= tasks_.first();
	Task* task = ri->item;

	return (task->startTime().secsTo(QDateTime::currentDateTime()) >= 0);
}

// Show CLI progress indicator
void Messenger::showCLIProgress()
{
	if (!printToConsole_) return;

	// Get last Task in list
	RefListItem<Task,int>* ri = tasks_.last();
	if (!ri) return;
	Task* task = ri->item;

	// Get most recent message in buffer
	QString messageText = messageBuffer_.count() > 0 ? messageBuffer_.first().text() : "???";

	// Create CLI progress text prefix string
	cliProgressText_ = QString("\r(%1 %2%) [%3] ").arg(task->etaText()).arg(int(task->completion()),3,10).arg(tasks_.nItems());

	// Overwrite last message with abbreviated progress information if the current message is the same as the last time we updated
	if (messageBufferPoint_ == backPrintedMessagePoint_) QTextStream(stdout) << cliProgressText_ << messageText;
	else backPrintedMessagePoint_ = messageBufferPoint_;
}

// Set pointer to custom progress dialog in GUI
void Messenger::setAtenProgress(AtenProgress* atenProgress)
{
	atenProgress_ = atenProgress;
}

// Push new task onto stack
Task* Messenger::initialiseTask(QString title, int totalSteps)
{
	Task* task = new Task;
	tasks_.add(task);

	task->initialise(title, totalSteps);
	++taskPoint_;

	return task;
}

// Push new command task onto stack
Task* Messenger::initialiseCommandTask(QString title, QString command, QString args, QString outputFile)
{
	Task* task = new Task;
	tasks_.add(task);

	task->initialiseCommand(title, command, args, outputFile);
	++taskPoint_;

	return task;
}

// Return number of current tasks
int Messenger::nTasks()
{
	return tasks_.nItems();
}

// Return list of current tasks
RefListItem<Task,int>* Messenger::tasks()
{
	return tasks_.first();
}

// Flag all tasks as being canceled
void Messenger::cancelAllTasks()
{
	// Set canceled status of all tasks
	for (RefListItem<Task,int>* ri = tasks_.first(); ri!= NULL; ri = ri->next)
	{
		Task* task = ri->item;
		task->cancel();
	}
}

// Return task log point
int Messenger::taskPoint()
{
	return taskPoint_;
}

// Update specified task
bool Messenger::updateTaskProgress(Task* task, int newCurrentStep)
{
	// Check that task is in list
	bool canceled = false;
	if (!tasks_.contains(task)) error("Internal Error: Tried to update a task that doesn't exist.");
	else if (task->type() == Task::SteppedTask)
	{
		task->update(newCurrentStep);
		canceled = task->canceled();
	}
	else if (task->type() == Task::ExternalTask)
	{
		if (task->commandOutputAvailable()) task->printLineToMessages();
		QCoreApplication::processEvents(QEventLoop::AllEvents, 50);
		canceled = task->canceled();
		if (!canceled) canceled = task->commandFailed();
	}

	// Call progress indicator if necessary
	if (progressIndicatorRequired())
	{
		if (atenProgress_) atenProgress_->updateAndShow();
		else if (printToConsole_) showCLIProgress();
	}

	return (!canceled);
}

// Update specified task
bool Messenger::incrementTaskProgress(Task* task, int deltaSteps)
{
	// Check that task is in list
	bool canceled = false;
	if (!tasks_.contains(task)) error("Internal Error: Tried to update a task that doesn't exist.");
	else if (task->type() == Task::SteppedTask)
	{
		task->increment(deltaSteps);
		canceled = task->canceled();
	}
	else if (task->type() == Task::ExternalTask)
	{
		if (task->commandOutputAvailable()) task->printLineToMessages();
		QCoreApplication::processEvents(QEventLoop::AllEvents, 50);
		canceled = task->canceled();
		if (!canceled) canceled = task->commandFailed();
	}

	// Call progress indicator if necessary
	if (progressIndicatorRequired())
	{
		if (atenProgress_) atenProgress_->updateAndShow();
		else if (printToConsole_) showCLIProgress();
	}

	return (!canceled);
}

// Terminate specifed task
void Messenger::terminateTask(Task* task)
{
	if (!tasks_.contains(task)) error("Internal Error: Tried to cancel a task that doesn't exist.");
	else tasks_.remove(task);
	++taskPoint_;

	// Still tasks remaining?
	if (tasks_.nItems() == 0)
	{
		if (atenProgress_) atenProgress_->terminate();
		else if (printToConsole_) printf("\n");
	}
	else
	{
		if (atenProgress_) atenProgress_->updateAndShow();
		else if (printToConsole_) showCLIProgress();
	}
}
