/*
	*** Key/Value map class
	*** src/base/kvmap.cpp
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

#include "base/kvmap.h"
#include <QStringList>

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

// Assignment Operator
KVMap& KVMap::operator=(const KVMap& source)
{
	pairs_ = source.pairs_;

	return *this;
}

// Copy Constructor
KVMap::KVMap(const KVMap& source)
{
	(*this) = source;
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
		kvp = new KVPair(key, value);
		pairs_.own(kvp);
	}
	else kvp->setValue(value);
}

// Return comma-separated list of keys
QString KVMap::keys()
{
	QString result;
	for (KVPair* kvp = pairs_.first(); kvp != NULL; kvp = kvp->next)
	{
		if (kvp != pairs_.first()) result += ", " + kvp->key();
		else result += kvp->key();
	}
	return result;
}

// Set (existing) key/value pair from 'option=value' string
void KVMap::add(QString optionEqualsValue)
{
	QStringList parts = optionEqualsValue.split("=");
	if (parts.count() == 2) add(parts.at(0), parts.at(1));
	else printf("Failed to parse 'option=value' string '%s'.\n", qPrintable(optionEqualsValue));
}

// Search map for specified key
KVPair* KVMap::search(QString key) const
{
	// Search for existing value...
	for (KVPair* kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (kvp->key() == key) return kvp;
	return NULL;
}

// Retrieve value associated to key
QString KVMap::value(QString key) const
{
	// Search for existing value...
	for (KVPair* kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) if (kvp->key() == key) return kvp->value();
	return NULL;
}

// Return whether standard option is set (to value specified if provided)
bool KVMap::isSet(QString key, QString value) const
{
	KVPair* pair = search(key); 
	if (!pair) return false;

	return (value.isEmpty() ? true : (pair->value() == value));
}

// Return first key in list
KVPair* KVMap::pairs()
{
	return pairs_.first();
}

// Dump contents of map
void KVMap::dump() const
{
	printf("Number of key/valus pairs in map %p = %i\n", this, pairs_.nItems());
	int count = 0;
	for (KVPair* kvp = pairs_.first(); kvp != NULL; kvp = kvp->next) printf("  %i   %s=%s\n", count++, qPrintable(kvp->key()), qPrintable(kvp->value()));
}
