/*
	*** Key/Value map class
	*** src/base/kvmap.h
	Copyright T. Youngs 2007-2010

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

#include "base/dnchar.h"
#include <cstdlib>
#include "templates/list.h"

// Simple string key/value pair class
class KVPair
{
	public:
	// Constructor / Destructor
	KVPair(const char *key = NULL, const char *value = NULL);
	// List pointers
	KVPair *prev, *next;

	/*
	// Data
	*/
	private:
	// Key
	Dnchar key_;
	// Value
	Dnchar value_;

	public:
	// Retrieve key associated to pair
	const char *key();
	// Set value associated to pair
	void setValue(const char *value);
	// Retrieve value associated to pair
	const char *value();
};

// Simple string key/value list class
class KVMap
{
	public:
	// Constructor / Destructor
	KVMap();

	/*
	// Data
	*/
	private:
	// Pair data
	List<KVPair> pairs_;

	public:
	// Clear pairs
	void clear();
	// Return number of pairs defined
	int nPairs();
	// Set (existing) key/value pair
	void add(const char *key, const char *value);
	// Search to see if specific key is in the table
	KVPair *search(const char *key) const;
	// Retrieve value associated to key
	const char *value(const char *key) const;
};

#endif

