/*
	*** Pointer pair/data class
	*** src/templates/pointerpair.h
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

#ifndef ATEN_POINTERPAIR_H
#define ATEN_POINTERPAIR_H

#include "templates/list.h"

// Pointer pair/data class
template <class T, class D> class PointerPair
{
	public:
	// Constructor / Destructor
	PointerPair<T,D>(T *ptr1, T *ptr2, int size);
	~PointerPair();
	// List pointers
	PointerPair *next, *prev;
	
	/*
	// Data
	*/
	private:
	// Pointer data
	T *p1_, *p2_;
	// Size of associated data
	int size_;
	// Associated data
	D *data_;

	public:
	// Return first pointer of pair
	T *ptr1();
	// Return second pointer of pair
	T *ptr2();
	// Set data member
	void setData(int id, D value);
	// Return data member
	D *data();
	// Return data element
	D data(int id) const;
};

// Pointer pair/data table
template <class T, class D> class PairTable
{
	public:
	// Constructor / Destructor
	PairTable<T,D>();
	~PairTable();

	/*
	// Data list
	*/
	private:
	// List of pair/data members
	List< PointerPair<T,D> > data_;

	public:
	// Clear all members
	void clear();
	// Add data member
	PointerPair<T,D> *add(T *ptr1, T *ptr2, int size);
	// Search for data member
	PointerPair<T,D> *find(T *ptr1, T *ptr2) const;
};

/*
// PointerPair
*/

// Constructor
template <class T, class D> PointerPair<T,D>::PointerPair(T *p1, T *p2, int size)
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Set pointer data
	p1_ = p1;
	p2_ = p2;

	// Create data array
	size_ = size;
	data_ = new D[size_];
}

// Destructor
template <class T, class D> PointerPair<T,D>::~PointerPair()
{
	delete[] data_;
}

// Return first pointer of pair
template <class T, class D> T *PointerPair<T,D>::ptr1()
{
	return p1_;
}

// Return second pointer of pair
template <class T, class D> T *PointerPair<T,D>::ptr2()
{
	return p2_;
}

// Set data member
template <class T, class D> void PointerPair<T,D>::setData(int id, D value)
{
	if ((id < 0) || (id >= size_)) printf("Internal Error : Data item %i is out of range for PointerPair. Can't set.\n", id);
	else data_[id] = value;
}

// Return data member
template <class T, class D> D *PointerPair<T,D>::data()
{
	return data_;
}

// Return data element
template <class T, class D> D PointerPair<T,D>::data(int id) const
{
	if ((id < 0) || (id >= size_))
	{
		printf("Internal Error : Data item %i is out of range for PointerPair. Can't retrieve.\n", id);
		return D();
	}
	return data_[id];
}

/*
// PairTable
*/

// Constructor
template <class T, class D> PairTable<T,D>::PairTable()
{
}

// Destructor
template <class T, class D> PairTable<T,D>::~PairTable()
{
	clear();
}

// Clear data items
template <class T, class D> void PairTable<T,D>::clear()
{
	data_.clear();
}

// Add data member
template <class T, class D> PointerPair<T,D> *PairTable<T,D>::add(T *ptr1, T *ptr2, int size)
{
	// Create new data member
	PointerPair<T,D> *pp = new PointerPair<T,D>(ptr1,ptr2,size);
	data_.own(pp);
	return pp;
}

// Search for data member
template <class T, class D> PointerPair<T,D> *PairTable<T,D>::find(T *ptr1, T *ptr2) const
{
	PointerPair<T,D> *pp;
	for (pp = data_.first(); pp != NULL; pp = pp->next)
	{
		if ((ptr1 == pp->ptr1()) && (ptr2 == pp->ptr2())) break;
		if ((ptr2 == pp->ptr1()) && (ptr1 == pp->ptr2())) break;
	}
	return pp;
}

#endif
