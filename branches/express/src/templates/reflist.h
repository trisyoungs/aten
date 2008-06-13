/*
	*** Object reference list
	*** src/templates/reflist.h
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

#ifndef ATEN_REFLIST_H
#define ATEN_REFLIST_H

#include "base/debug.h"
#include <stddef.h>
#include <stdio.h>

// Reference item
template <class T, class D> class Refitem
{
	public:
	// Constructor / Destructor
	Refitem<T,D>();
	~Refitem();

	// List pointers
	Refitem<T,D> *prev, *next;
	// Pointer to item
	T *item;
	// Additional temporary info stored in structure
	D data;
};

// Reference list
template <class T, class D> class Reflist
{
	public:
	// Constructor / Destructor
	Reflist<T,D>();
	~Reflist();

	/*
	// Reference List of Items
	*/
	private:
	// Head and tail of reference items
	Refitem<T,D> *itemsHead_, *itemsTail_;
	// Number of items in list
	int nItems_;

	public:
	// Returns the head of the atom list
	Refitem<T,D> *first();
	// Returns the last item in the list
	Refitem<T,D> *last();
	// Returns the number of atoms in the list
	int nItems();
	// Add reference to the list
	void add(T*);
	// Add reference to the list with extra data
	void add(T* x, D extradata);
	// Add reference to list, unless already there
	void addUnique(T*);
	// Delete the reference from the list
	void remove(Refitem<T,D>*);
	// Delete the reference containing specified item from the list
	void remove(T*);
	// Operator =
	void operator=(Reflist<T,D> &source);
	// Element access operator
	Refitem<T,D> *operator[](int);
	// Search references for item
	Refitem<T,D> *search(T*);
	// Clear the list of all references
	void clear();
	// Move head of list to tail of list
	void moveHeadToTail();
	// Add references to contents of list (head of which is provided)
	void createFromList(T*);
	// Fills the supplied array with pointer values to the reference items
	void fillArray(int, T**);
};

// Constructors
template <class T, class D> Refitem<T,D>::Refitem()
{
	item = NULL;
	next = NULL;
	prev = NULL;
}

template <class T, class D> Reflist<T,D>::Reflist()
{
	itemsHead_ = NULL;
	itemsTail_ = NULL;
	nItems_ = 0;
}

// Destructors
template <class T, class D> Reflist<T,D>::~Reflist()
{
	clear();
}

template <class T, class D> Refitem<T,D>::~Refitem()
{
}

// Assignment operator =
template <class T, class D> void Reflist<T,D>::operator=(Reflist<T,D> &source)
{
	// Clear any current data...
	clear();
	for (Refitem<T,D> *ri = source.first(); ri != NULL; ri = ri->next) add(ri->item, ri->data);
}

// Returns the head of the atom list
template <class T, class D> Refitem<T,D> *Reflist<T,D>::first()
{
	return itemsHead_;
}

// Returns the last item in the list
template <class T, class D> Refitem<T,D> *Reflist<T,D>::last()
{
	return itemsTail_;
}

// Returns the number of atoms in the list
template <class T, class D> int Reflist<T,D>::nItems()
{
	return nItems_;
}

// Add item to list
template <class T, class D> void Reflist<T,D>::add(T* target)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the list
	itemsHead_ == NULL ? itemsHead_ = newitem : itemsTail_->next = newitem;
	newitem->prev = itemsTail_;
	itemsTail_ = newitem;
	newitem->item = target;
	nItems_ ++;
}

// Add item to list with extra data
template <class T, class D> void Reflist<T,D>::add(T* target, D extradata)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the list
	itemsHead_ == NULL ? itemsHead_ = newitem : itemsTail_->next = newitem;
	newitem->prev = itemsTail_;
	itemsTail_ = newitem;
	newitem->item = target;
	newitem->data = extradata;
	nItems_ ++;
}

// Add unique item to list
template <class T, class D> void Reflist<T,D>::addUnique(T* target)
{
	if (search(target) == NULL) add(target);
}

// Remove Refitem from list
template <class T, class D> void Reflist<T,D>::remove(Refitem<T,D> *xitem)
{
	// Delete a specific Refitem from the list
	xitem->prev == NULL ? itemsHead_ = xitem->next : xitem->prev->next = xitem->next;
	xitem->next == NULL ? itemsTail_ = xitem->prev : xitem->next->prev = xitem->prev;
	delete xitem;
	nItems_ --;
}

// Remove item from list
template <class T, class D> void Reflist<T,D>::remove(T *xitem)
{
	// Delete a specific item from the list
	Refitem<T,D> *r = search(xitem);
	if (r != NULL) remove(r);
}

// Element access operator
template <class T, class D> Refitem<T,D>* Reflist<T,D>::operator[](int index)
{
	if (index >= nItems_)
	{
		printf("reflist::[] <<<< SEVERE - Array index (%i) out of bounds (0-%i) >>>>\n",index,nItems_-1);
		return NULL;
	}
	// Scan through for element number 'index' in the list and return it
	Refitem<T,D> *result = itemsHead_;
	for (int i=0; i<index; i++) result = result->next;
	return result;
}

// Search for item
template <class T, class D> Refitem<T,D>* Reflist<T,D>::search(T *xitem)
{
	// Search references for specified item
	Refitem<T,D> *result = NULL;
	Refitem<T,D> *r = itemsHead_;
	while (r != NULL)
	{
		if (r->item == xitem)
		{
			result = r;
			break;
		}
		r = r->next;
	}
	return result;
}

// Clear atoms from list
template <class T, class D> void Reflist<T,D>::clear()
{
	// Clear the list 
	Refitem<T,D> *xitem = itemsHead_;
	while (xitem != NULL)
	{
		remove(xitem);
		xitem = itemsHead_;
	}
	itemsHead_ = NULL;
	itemsTail_ = NULL;
	nItems_ = 0;
}

// Move head to tail
template <class T, class D> void Reflist<T,D>::moveHeadToTail()
{
	// Add a new item to the list (a copy of the current head)
	add(itemsHead_->item,itemsHead_->data);
	// Delete head item
	remove(itemsHead_);
}

// Create from list
template <class T, class D> void Reflist<T,D>::createFromList(T *xitem)
{
	// Add references to the items held in the list pointed to by xitem
	clear();
	while (xitem != NULL)
	{
		add(xitem);
		xitem = xitem->next;
	}
}

// Fill array
template <class T, class D> void Reflist<T,D>::fillArray(int n, T **data)
{
	int count = 0;
	Refitem<T,D> *ri = itemsHead_;
	while (ri != NULL)
	{
		data[count] = ri->item;
		count ++;
		if (count == n) break;
		ri = ri->next;
		if (ri == NULL) printf("reflist::fill_array <<<< Not enough items in list - requested %i, had %i >>>>\n",n,nItems_);
	}
}

#endif
