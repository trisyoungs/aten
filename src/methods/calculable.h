/*
	*** Basic calculable definition
	*** src/methods/calculable.h
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

#ifndef ATEN_CALCULABLE_H
#define ATEN_CALCULABLE_H

#include <QString>
#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

// Calculable Class
class Calculable : public ListItem<Calculable>
{
	public:
	// Constructor / Destructor
	Calculable();
	virtual ~Calculable();


	/*
	// Identity
	*/
	protected:
	// Identifiable name of the quantity
	QString name_;
	// Filename (of loaded file or target to be saved)
	QString filename_;

	public:
	// Set identifiable name of the quantity
	void setName(QString s);
	// Return name of the quantity
	QString name() const;
	// Set filename of the quantity
	void setFilename(QString s);
	// Return filename of the quantity
	QString filename() const;

	/*
	// Methods
	*/
	public:
	// Accumulate quantity data from supplied config
	virtual void accumulate(Model* sourceModel)=0;
	// Initialise - check sites, create arrays etc.
	virtual bool initialise()=0;
	// Finalise data
	virtual void finalise(Model* sourceModel)=0;
	// Save data
	virtual bool save()=0;
};

ATEN_END_NAMESPACE

#endif
