/*
	*** Data Storage class
	*** src/base/datatore.cpp
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

#include "base/datastore.h"
#include <string.h>

/*
// DataStoreItem
*/

// Constructor
DataStoreItem::DataStoreItem(const char *key) : ListItem<DataStoreItem>()
{
	// Private variables
	if (key != NULL) key_ = key;
}

// Retrieve key associated to data
const char *DataStoreItem::key() const
{
	return key_.get();
}

// Retrieve value associated to key
ReturnValue &DataStoreItem::data()
{
	return data_;
}

/*
// DataStore
*/

// Constructor
DataStore::DataStore()
{
}

// Clear data
void DataStore::clear()
{
	data_.clear();
}

// Return number of data defined
int DataStore::nItems() const
{
	return data_.nItems();
}

// Add new (or return existing) key/value data
DataStoreItem *DataStore::addData(const char *key)
{
	// Search for existing value...
	DataStoreItem *kvp = NULL;
	for (kvp = data_.first(); kvp != NULL; kvp = kvp->next) if (strcmp(kvp->key(),key) == 0) break;
	if (kvp == NULL)
	{
		kvp = new DataStoreItem(key);
		data_.own(kvp);
	}
	return kvp;
}

// Search map for specified key
DataStoreItem *DataStore::searchForKey(const char *key)
{
	// Search for existing value...
	for (DataStoreItem *kvp = data_.first(); kvp != NULL; kvp = kvp->next) if (strcmp(kvp->key(),key) == 0) return kvp;
	return NULL;
}

// Retrieve data associated to key
ReturnValue &DataStore::dataForKey(const char *key)
{
	static ReturnValue dummy;
	// Search for existing value...
	for (DataStoreItem *kvp = data_.first(); kvp != NULL; kvp = kvp->next) if (strcmp(kvp->key(),key) == 0) return kvp->data();
	printf("Warning: Requested data for key '%s' in DataStore, but no such item was present.\n", key);
	return dummy;
}

// Return first key in list
DataStoreItem *DataStore::dataItems()
{
	return data_.first();
}

