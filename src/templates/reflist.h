/*
	*** Object reference list
	*** src/templates/reflist.h
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

#ifndef ATEN_REFLIST_H
#define ATEN_REFLIST_H

#include "base/messenger.h"
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
	Refitem<T,D> *listHead_, *listTail_;
	// Number of items in list
	int nItems_;

	public:
	// Returns the head of the atom list
	Refitem<T,D> *first() const;
	// Returns the last item in the list
	Refitem<T,D> *last() const;
	// Returns the number of atoms in the list
	int nItems() const;
	// Add reference to the list
	Refitem<T,D> *add(T *item);
	// Add reference to the list with extra data
	Refitem<T,D> *add(T* item, D extradata);
	// Add reference to the beginning of the list
	void addStart(T *item);
	// Add reference to the beginning of the list with extra data
	void addStart(T *item, D extradata);
	// Add reference to list, unless already there
	void addUnique(T *item);
	// Add reference to list, unless already there
	void addUnique(T *, D extradata);
	// Add an orphaned item into this list
	void own(Refitem<T,D> *item);
	// Delete the reference from the list
	void remove(Refitem<T,D> *item);
	// Delete the reference containing specified item from the list
	void remove(T *item);
	// Remove the first item in the list
	void removeFirst();
	// Remove the last item in the list
	void removeLast();
	// Operator =
	void operator=(Reflist<T,D> &source);
	// Element access operator
	Refitem<T,D> *operator[](int);
	// Search references for item
	Refitem<T,D> *search(T *item);
	// Clear the list of all references
	void clear();
	// Move head of list to tail of list
	void moveHeadToTail();
	// Add references to contents of list (head of which is provided)
	void createFromList(T *listhead);
	// Fills the supplied array with 'n' pointer values to the reference items
	void fillArray(int nitems, T **itemarray);
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
	listHead_ = NULL;
	listTail_ = NULL;
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
template <class T, class D> Refitem<T,D> *Reflist<T,D>::first() const
{
	return listHead_;
}

// Returns the last item in the list
template <class T, class D> Refitem<T,D> *Reflist<T,D>::last() const
{
	return listTail_;
}

// Returns the number of atoms in the list
template <class T, class D> int Reflist<T,D>::nItems() const
{
	return nItems_;
}

// Add item to list
template <class T, class D> Refitem<T,D> *Reflist<T,D>::add(T* target)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the list
	listHead_ == NULL ? listHead_ = newitem : listTail_->next = newitem;
	newitem->prev = listTail_;
	listTail_ = newitem;
	newitem->item = target;
	nItems_ ++;
	return newitem;
}

// Add item to list with extra data
template <class T, class D> Refitem<T,D> *Reflist<T,D>::add(T* target, D extradata)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the list
	listHead_ == NULL ? listHead_ = newitem : listTail_->next = newitem;
	newitem->prev = listTail_;
	listTail_ = newitem;
	newitem->item = target;
	newitem->data = extradata;
	nItems_ ++;
	return newitem;
}

// Add item to start of list
template <class T, class D> void Reflist<T,D>::addStart(T* target)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the beginning of the list
	newitem->next = listHead_;
	listHead_ == NULL ? listHead_ = newitem : listHead_->prev = newitem;
	listHead_ = newitem;
	newitem->item = target;
	nItems_ ++;
}

// Add item to start of list with extra data
template <class T, class D> void Reflist<T,D>::addStart(T* target, D extradata)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the beginning of the list
	newitem->next = listHead_;
	listHead_ == NULL ? listHead_ = newitem : listHead_->prev = newitem;
	listHead_ = newitem;
	newitem->item = target;
	newitem->data = extradata;
	nItems_ ++;
}

// Add unique item to list
template <class T, class D> void Reflist<T,D>::addUnique(T* target)
{
	if (search(target) == NULL) add(target);
}

// Add unique item to list
template <class T, class D> void Reflist<T,D>::addUnique(T* target, D extradata)
{
	if (search(target) == NULL) add(target, extradata);
}

// Own an existing item
template <class T, class D> void Reflist<T,D>::own(Refitem<T,D> *olditem)
{
	// In the interests of 'pointer cleanliness, refuse to own the item if its pointers are not NULL
	if ((olditem->next != NULL) || (olditem->prev != NULL))
	{
		printf("reflist::own <<<< Reflist refused to own an item that still had ties >>>>\n");
		return;
	}
	listHead_ == NULL ? listHead_ = olditem : listTail_->next = olditem;
	olditem->prev = listTail_;
	olditem->next = NULL;
	listTail_ = olditem;
	nItems_ ++;
}

// Remove Refitem from list
template <class T, class D> void Reflist<T,D>::remove(Refitem<T,D> *xitem)
{
	// Delete a specific Refitem from the list
	xitem->prev == NULL ? listHead_ = xitem->next : xitem->prev->next = xitem->next;
	xitem->next == NULL ? listTail_ = xitem->prev : xitem->next->prev = xitem->prev;
	delete xitem;
	nItems_ --;
}

// Remove first item from list
template <class T, class D> void Reflist<T,D>::removeFirst()
{
	remove(listHead_);
}

// Remove last item from list
template <class T, class D> void Reflist<T,D>::removeLast()
{
	remove(listTail_);
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
	if ((index < 0) || (index >= nItems_))
	{
		printf("reflist::[] <<<< SEVERE - Array index (%i) out of bounds (0-%i) >>>>\n",index,nItems_-1);
		return NULL;
	}
	// Scan through for element number 'index' in the list and return it
	Refitem<T,D> *result = listHead_;
	for (int i=0; i<index; i++) result = result->next;
	return result;
}

// Search for item
template <class T, class D> Refitem<T,D>* Reflist<T,D>::search(T *xitem)
{
	// Search references for specified item
	Refitem<T,D> *r;
	for (r = listHead_; r != NULL; r = r->next) if (r->item == xitem) break;
	return r;
}

// Clear atoms from list
template <class T, class D> void Reflist<T,D>::clear()
{
	// Clear the list 
	Refitem<T,D> *xitem = listHead_;
	while (xitem != NULL)
	{
		remove(xitem);
		xitem = listHead_;
	}
	listHead_ = NULL;
	listTail_ = NULL;
	nItems_ = 0;
}

// Move head to tail
template <class T, class D> void Reflist<T,D>::moveHeadToTail()
{
	// Add a new item to the list (a copy of the current head)
	add(listHead_->item,listHead_->data);
	// Delete head item
	remove(listHead_);
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
	Refitem<T,D> *ri = listHead_;
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
