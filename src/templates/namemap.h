/*
	*** Object namemap class
	*** src/templates/namemap.h
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

#ifndef ATEN_NAMEMAP_H
#define ATEN_NAMEMAP_H

#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Simple string/data pair class
template <class T> class NameMap : public ListItem< NameMap<T> >
{
	public:
	// Constructor / Destructor
	NameMap<T>();


	/*
	 * Data
	 */
	private:
	// Character data
	QString name_;
	// Data type B
	T data_;

	public:
	// Set name
	void setName(QString name);
	// Return name
	QString name() const;
	// Set data
	void setData(T data);
	// Return data
	T data() const;
	// Set name and data
	void set(QString name, T data);
};

// Constructor
template <class T> NameMap<T>::NameMap() : ListItem< NameMap<T> >()
{
}

// Set name
template <class T> void NameMap<T>::setName(QString name)
{
	name_ = name;
}

// Returns name
template <class T> QString NameMap<T>::name() const
{
	return name_;
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
template <class T> void NameMap<T>::set(QString name, T data)
{
	name_ = name;
	data_ = data;
}

// NameMap list
template <class T> class NameMapList
{
	public:
	// Constructor
	NameMapList<T>(T defaultvalue);
	NameMapList<T>();

	/*
	 * Data
	 */
	private:
	// Data to return in the event of no match in data() members
	T defaultValue_;
	// List of data items
	List< NameMap<T> > data_;

	public:
	// Set default value
	void setDefaultValue(T defaultvalue);
	// Clear all items
	void clear();
	// Create new entry
	int add(QString name, T data);
	// Return number of items defined
	int nItems() const;
	// Return first data item in list
	NameMap<T>* first();
	// Return data item associated to name
	T data(QString name) const;
	// Return nth data item associated to name
	T data(int n);
	// Return nth data item name
	QString name(int n);
	// Return index in list of supplied item
	int index(QString name);
};

// Constructors
template <class T> NameMapList<T>::NameMapList(T defaultvalue)
{
	// Private variables
	defaultValue_ = defaultvalue;
}

template <class T> NameMapList<T>::NameMapList()
{
}

// Set default value
template <class T> void NameMapList<T>::setDefaultValue(T defaultvalue)
{
	defaultValue_ = defaultvalue;
}

// Create new item
template <class T> int NameMapList<T>::add(QString name, T data)
{
	// Does the named value already exist
	int n=0;
	NameMap<T>* nm;
	for (nm = data_.first(); nm != NULL; nm = nm->next)
	{
		if (nm->name() == name) break;
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

// Clear all items
template <class T> void NameMapList<T>::clear()
{
	data_.clear();
}

// Return number of items defined
template <class T> int NameMapList<T>::nItems() const
{
	return data_.nItems();
}

// Return first data item in list
template<class T> NameMap<T>* NameMapList<T>::first()
{
	return data_.first();
}

// Retrieve item by name
template <class T> T NameMapList<T>::data(QString name) const
{
	NameMap<T>* nm;
	for (nm = data_.first(); nm != NULL; nm = nm->next) if (nm->name() == name) break;
	return (nm == NULL ? defaultValue_ : nm->data());
}

// Create new item
template <class T> T NameMapList<T>::data(int id)
{
	NameMap<T>* nm = data_[id];
	return (nm == NULL ? defaultValue_ : nm->data());
}

// Create new item
template <class T> QString NameMapList<T>::name(int n)
{
	NameMap<T>* nm = data_[n];
	return (nm == NULL ? "NULL" : nm->name());
}

// Return index in list of supplied item
template <class T> int NameMapList<T>::index(QString name)
{
	int n=0;
	for (NameMap<T>* nm = data_.first(); nm != NULL; nm = nm->next)
	{
		if (nm->name() == name) return n;
		++n;
	}
	return -1;
}

ATEN_END_NAMESPACE

#endif
