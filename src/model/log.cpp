/*
	*** Model log functions
	*** src/model/log.cpp
	Copyright T. Youngs 2007-2018

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

#include "model/model.h"

ATEN_USING_NAMESPACE

// Set complete log structure
void Model::setChangeLog(Log& source)
{
	changeLog_ = source;
}

// Return log structure
Log& Model::changeLog()
{
	return changeLog_;
}

// Reset changelog
void Model::resetLogs()
{
	changeLog_.reset();
}

// Log change in specified quantity
void Model::logChange(Log::LogType logType)
{
	changeLog_.add(logType);
}

// Return log quantity specified
int Model::log(Log::LogType logType) const
{
	return changeLog_.log(logType);
}

// Return whether model has been modified
bool Model::isModified() const
{
	return changeLog_.isModified();
}

// Update save point for model
void Model::updateSavePoint()
{
	changeLog_.updateSavePoint();
}

// Print points information
void Model::printLogs() const
{
	Messenger::print("Logs for model '%s':", qPrintable(name_));
	changeLog_.print();
	Messenger::print(" Expression point : %i", expressionPoint_);
	Messenger::print("   Patterns point : %i", patternsPoint_);
	Messenger::print("RenderGroup point : %i", renderGroupPoint_);
	Messenger::print("       Icon point : %i", iconPoint_);
	Messenger::print("    ZMatrix point : %i", zMatrixPoint_);
}
