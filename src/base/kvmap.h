/*
	*** Key/Value map class
	*** src/base/kvmap.h
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

#ifndef ATEN_KVMAP_H
#define ATEN_KVMAP_H

#include <cstdlib>
#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Simple string key/value pair class
class KVPair : public ListItem<KVPair>
{
	public:
	// Constructor / Destructor
	KVPair(QString key = QString(), QString value = QString());

	/*
	 * Data
	 */
	private:
	// Key
	QString key_;
	// Value
	QString value_;

	public:
	// Retrieve key associated to pair
	QString key() const;
	// Set value associated to pair
	void setValue(QString value);
	// Retrieve value associated to pair
	QString value() const;
};

// Simple string key/value list class
class KVMap
{
	public:
	// Constructor / Destructor
	KVMap();
	// Assignment Operator
	void operator=(const KVMap& source);
	// Copy Constructor
	KVMap(const KVMap& source);

	
	/*
	 * Data
	 */
	private:
	// Pair data
	List<KVPair> pairs_;

	public:
	// Clear pairs
	void clear();
	// Return number of pairs defined
	int nPairs() const;
	// Set (existing) key/value pair
	void add(QString key, QString value);
	// Set (existing) key/value pair from 'option=value' string
	void add(QString optionEqualsValue);
	// Search to see if specific key is in the table
	KVPair* search(QString key) const;
	// Retrieve value associated to key
	QString value(QString key);
	// Return whether the specified key is set (to value specified if provided)
	bool isSet(QString key, QString value = QString()) const;
	// Return first key in list
	KVPair* pairs();
};

ATEN_END_NAMESPACE

#endif

