/*
	*** External Command
	*** src/base/externalcommand.h
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

#ifndef ATEN_EXTERNALCOMMAND_H
#define ATEN_EXTERNALCOMMAND_H

#include <QStringList>
#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// External Command
class ExternalCommand : public ListItem<ExternalCommand>
{
	public:
	// Constructor / Destructor
	ExternalCommand();


	/*
	 * Definition
	 */
	private:
	// Short name of command
	QString name_;
	// Executable name
	QString executable_;
	// Search paths for executable
	QStringList searchPaths_;
	// Arguments for executable
	QString arguments_;

	public:
	// Set short name of command
	void setName(QString name);
	// Return short name of command
	QString name();
	// Set executable name
	void setExecutable(QString executable);
	// Return executable name
	QString executable();
	// Add search path
	void addSearchPath(QString path);
	// Return list of search paths
	QStringList searchPaths();
	// Return absolute path to command (using search paths) or empty string if not found
	QString absoluteExecutable();
	// Set arguments
	void setArguments(QString arguments);
	// Return arguments list
	QString arguments();
};

ATEN_END_NAMESPACE

#endif

