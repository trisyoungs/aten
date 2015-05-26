/*
	*** Key/Value map class
	*** src/base/kvmap.cpp
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

#include "base/kvmap.h"
#include <string.h>

ATEN_USING_NAMESPACE

/*
 * KVPair
 */

// Constructor
KVPair::KVPair(QString key, QString value) : ListItem<KVPair>()
{
	// Private variables
	if (key != NULL) key_ = key;
	if (value != NULL) value_ = value;
}

// Retrieve key associated to pair
QString KVPair::key() const
{
	return key_;
}

// Set value associated to key
void KVPair::setValue(QString value)
{
	value_ = value;
}

// Retrieve value associated to key
QString KVPair::value() const
{
	return value_;
}

/*
 * KVMap
 */

// Constructor
KVMap::KVMap()
{
}

// Clear pairs
void KVMap::clear()
{
	pairs_.clear();
}

// Return number of pairs defined
int KVMap::nPairs() const
{
	return pairs_.nItems();
}

// Set (existing) key/value pair
void KVMap::add(QString key, QString value)
{
	// Search for existing value...
	KVPair* kvp = NULL;
	for (kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (kvp->key() == key) break;
	if (kvp == NULL)
	{
		kvp = new KVPair(key,value);
		pairs_.own(kvp);
	}
	else kvp->setValue(value);
}

// Search map for specified key
KVPair* KVMap::search(QString key) const
{
	// Search for existing value...
	for (KVPair* kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (kvp->key() == key) return kvp;
	return NULL;
}

// Retrieve value associated to key
QString KVMap::value(QString key)
{
	// Search for existing value...
	for (KVPair* kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (kvp->key() == key) return kvp->value();
	return NULL;
}

// Return first key in list
KVPair* KVMap::pairs()
{
	return pairs_.first();
}
