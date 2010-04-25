/*
	*** Object namemap class
	*** src/templates/namemap.h
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

#ifndef ATEN_NAMEMAP_H
#define ATEN_NAMEMAP_H

#include "base/dnchar.h"
#include "templates/list.h"

// Simple string/data pair class
template <class T> class NameMap
{
	public:
	// Constructor / Destructor
	NameMap<T>();
	// List pointers
	NameMap *next, *prev;

	/*
	// Data
	*/
	private:
	// Character data
	Dnchar name_;
	// Data type B
	T data_;

	public:
	// Set name
	void setName(const char *name);
	// Return name
	const char *name() const;
	// Set data
	void setData(T data);
	// Return data
	T data() const;
	// Set name and data
	void set(const char *name, T data);
};

// Constructor
template <class T> NameMap<T>::NameMap()
{
	// Public variables
	prev = NULL;
	next = NULL;
}

// Set name
template <class T> void NameMap<T>::setName(const char *name)
{
	name_ = name;
}

// Returns name
template <class T> const char *NameMap<T>::name() const
{
	return name_.get();
}

// Set data
template <class T> void NameMap<T>::setData(T data)
{
	data_ = data;
}

// Returns data
template <class T> T NameMap<T>::data() const
{
	return data_;
}

// Set name and data
template <class T> void NameMap<T>::set(const char *name, T data)
{
	name_ = name;
	data_ = data;
}

// NameMap list
template <class T> class NameMapList
{
	public:
	// Constructor / Destructor
	NameMapList<T>(T defaultvalue);

	/*
	// Data
	*/
	private:
	// Data to return in the event of no match in data() members
	T defaultValue_;
	// List of data items
	List< NameMap<T> > data_;

	public:
	// Create new entry
	int add(const char *name, T data);
	// Return data item associated to name
	T data(const char *name) const;
	// Return nth data item associated to name
	T data(int n);
	// Return nth data item name
	const char *name(int n);
	// Return index in list of supplied item
	int index(const char *name);
};

// Constructor
template <class T> NameMapList<T>::NameMapList(T defaultvalue)
{
	// Private variables
	defaultValue_ = defaultvalue;
}

// Create new item
template <class T> int NameMapList<T>::add(const char *name, T data)
{
	// Does the named value already exist
	int n=0;
	NameMap<T> *nm;
	for (nm = data_.first(); nm != NULL; nm = nm->next)
	{
		if (strcmp(nm->name(),name) == 0) break;
		++n;
	}
	if (nm == NULL)
	{
		nm = data_.add();
		n = data_.nItems();
	}
	nm->set(name, data);
	return n;
}

// Retrieve item by name
template <class T> T NameMapList<T>::data(const char *name) const
{
	NameMap<T> *nm;
	for (nm = data_.first(); nm != NULL; nm = nm->next) if (strcmp(nm->name(),name) == 0) break;
	return (nm == NULL ? defaultValue_ : nm->data());
}

// Create new item
template <class T> T NameMapList<T>::data(int id)
{
	NameMap<T> *nm = data_[id];
	return (nm == NULL ? defaultValue_ : nm->data());
}

// Create new item
template <class T> const char *NameMapList<T>::name(int id)
{
	NameMap<T> *nm = data_[id];
	return (nm == NULL ? "NULL" : nm->name());
}

// Return index in list of supplied item
template <class T> int NameMapList<T>::index(const char *name)
{
	int n=0;
	for (NameMap<T> *nm = data_.first(); nm != NULL; nm = nm->next)
	{
		if (strcmp(nm->name(),name) == 0) return n;
		++n;
	}
	return -1;
}

#endif
