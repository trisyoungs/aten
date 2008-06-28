/*
	*** Object namemap class
	*** src/templates/namemap.h
	Copyright T. Youngs 2007,2008

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

//#include "base/messenger.h"
//#include <stdlib.h>
//#include <stdio.h>
#include "classes/dnchar.h"

// Simple string/data pair class
template <class T> class Namemap
{
	public:
	// Constructor / Destructor
	Namemap<T>();
	// List pointers
	Namemap *next, *prev;

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
template <class T> Namemap<T>::Namemap()
{
	// Public variables
	prev = NULL;
	next = NULL;
}

// Set name
template <class T> void Namemap<T>::setName(const char *name)
{
	name_ = name;
}

// Returns name
template <class T> const char *Namemap<T>::name() const
{
	return name_.get();
}

// Set data
template <class T> void Namemap<T>::setData(T data)
{
	data_ = data;
}

// Returns data
template <class T> T Namemap<T>::data() const
{
	return data_;
}

// Set name and data
template <class T> void Namemap<T>::set(const char *name, T data)
{
	name_ = name;
	data_ = data;
}

#endif
