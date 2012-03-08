/*
	*** Key/Value map template
	*** src/templates/kvmap.h
	Copyright T. Youngs 2007-2012

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

#ifndef ATEN_KVTABLE_H
#define ATEN_KVTABLE_H

#include "base/constants.h"
#include "templates/list.h"

/*
// Simple string key/value pair template object
*/
template <class K, class V> class KVData
{
	public:
	// Constructor / Destructor
	KVData<K,V>();
	// List pointers
	KVData<K,V> *prev, *next;

	/*
	// Data
	*/
	private:
	// Key
	K key_;
	// Value
	V value_;

	public:
	// Set key
	void setKey(K key);
	// Retrieve key associated to pair
	K key() const;
	// Set value associated to pair
	void setValue(V value);
	// Retrieve value associated to pair
	V value() const;
};

// Constructor
template <class K, class V> KVData<K,V>::KVData()
{
	// Private variables
	key_ = K();
	value_ = V();

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set key
template <class K, class V> void KVData<K,V>::setKey(K keyname)
{
	key_ = keyname;
}

// Retrieve key associated to pair
template <class K, class V> K KVData<K,V>::key() const
{
	return key_;
}

// Set value associated to key
template <class K, class V> void KVData<K,V>::setValue(V newvalue)
{
	value_ = newvalue;
}

// Retrieve value associated to key
template <class K, class V> V KVData<K,V>::value() const
{
	return value_;
}

/*
// Simple string key/value template
*/
template <class K, class V> class KVTable
{
	public:
	// Constructor / Destructor
	KVTable<K,V>();

	/*
	// Data
	*/
	private:
	// Pair data
	List< KVData<K,V> > pairs_;

	public:
	// Clear pairs
	void clear();
	// Return number of pairs defined
	int nPairs() const;
	// Set (existing) key/value pair
	void add(K key, V value);
	// Search to see if specific key is in the table
	KVData<K,V> *search(K key);
	// Retrieve value associated to key
	V value(K key);
	// Retrieve value associated to key, also setting success flag if the key was/wasn't found
	V value(K key, bool &success);
	// Return first pair in list
	KVData<K,V> *pairs();
};

// Constructor
template <class K, class V> KVTable<K,V>::KVTable()
{
}

// Clear pairs
template <class K, class V> void KVTable<K,V>::clear()
{
	pairs_.clear();
}

// Return number of pairs defined
template <class K, class V> int KVTable<K,V>::nPairs() const
{
	return pairs_.nItems();
}

// Set (existing) key/value pair
template <class K, class V> void KVTable<K,V>::add(K key, V value)
{
	// Search for existing value...
	KVData<K,V> *kvp = NULL;
	for (kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (key == kvp->key()) break;
	if (kvp == NULL)
	{
		kvp = new KVData<K,V>;
		K test;
		kvp->setKey(key);
		kvp->setValue(value);
		pairs_.own(kvp);
	}
	else kvp->setValue(value);
}

// Search map for specified key
template <class K, class V> KVData<K,V> *KVTable<K,V>::search(K key)
{
	// Search for existing value...
	for (KVData<K,V> *kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (key == kvp->key()) return kvp;
	return NULL;
}

// Retrieve value associated to key
template <class K, class V> V KVTable<K,V>::value(K key)
{
	// Search for existing value...
	bool success;
	return value(key, success);
}

// Retrieve value associated to key, also setting success flag if the key was/wasn't found
template <class K, class V> V KVTable<K,V>::value(K key, bool &success)
{
	// Search for existing value...
	success = TRUE;
	for (KVData<K,V> *kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (key == kvp->key()) return kvp->value();
	success = FALSE;
	return V();
}

// Return first pair in list
template <class K, class V> KVData<K,V> *KVTable<K,V>::pairs()
{
	return pairs_.first();
}

#endif
