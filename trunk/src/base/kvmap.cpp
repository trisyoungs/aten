/*
	*** Key/Value map class
	*** src/base/kvmap.cpp
	Copyright T. Youngs 2007-2009

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

/*
// KVPair
*/

// Constructor
KVPair::KVPair(const char *key, const char *value)
{
	// Private variables
	if (key != NULL) key_ = key;
	if (value != NULL) value_ = value;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Retrieve key associated to pair
const char *KVPair::key()
{
	return key_.get();
}

// Set value associated to key
void KVPair::setValue(const char *newvalue)
{
	value_ = newvalue;
}

// Retrieve value associated to key
const char *KVPair::value()
{
	return value_.get();
}

/*
// KVMap
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
int KVMap::nPairs()
{
}

// Set (existing) key/value pair
void KVMap::add(const char *key, const char *value)
{
	// Search for existing value...
	KVPair *kvp = NULL;
	for (kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (strcmp(kvp->key(),key) == 0) break;
	if (kvp == NULL)
	{
		kvp = new KVPair(key,value);
		pairs_.own(kvp);
	}
	else kvp->setValue(value);
}

// Search map for specified key
KVPair *KVMap::search(const char *key) const
{
	// Search for existing value...
	for (KVPair *kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (strcmp(kvp->key(),key) == 0) return kvp;
	return NULL;
}

// Retrieve value associated to key
const char *KVMap::value(const char *key) const
{
	// Search for existing value...
	for (KVPair *kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (strcmp(kvp->key(),key) == 0) return kvp->value();
	return NULL;
}

