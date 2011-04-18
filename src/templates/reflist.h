/*
	*** Object reference list
	*** src/templates/reflist.h
	Copyright T. Youngs 2007-2011

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
	// Static array of items
	Refitem<T,D> **items_;
	// Array regeneration flag
	bool regenerate_;

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
	Refitem<T,D> *addStart(T *item);
	// Add reference after the specified item
	Refitem<T,D> *addAfter(Refitem<T,D> *target, T *item);
	// Add reference to the beginning of the list with extra data
	Refitem<T,D> *addStart(T *item, D extradata);
	// Add reference to list, unless already there
	Refitem<T,D> *addUnique(T *item);
	// Add reference to list, unless already there
	Refitem<T,D> *addUnique(T *, D extradata);
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
	Refitem<T,D> *contains(T *item);
	// Search references for data
	Refitem<T,D> *containsData(D data);
	// Clear the list of all references
	void clear();
	// Move head of list to tail of list
	void moveHeadToTail();
	// Add references to contents of list (head of which is provided)
	void createFromList(T *listhead);
	// Fills the supplied array with 'n' pointer values to the reference items
	void fillArray(int nitems, T **itemarray);
	// Swap the two items specified
	void swap(T *item1, T *item2);
	// Return array of items
	Refitem<T,D> **array();
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
	items_ = NULL;
	regenerate_ = 1;
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
template <class T, class D> Refitem<T,D> *Reflist<T,D>::add(T* item)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the list
	listHead_ == NULL ? listHead_ = newitem : listTail_->next = newitem;
	newitem->prev = listTail_;
	listTail_ = newitem;
	newitem->item = item;
	nItems_ ++;
	regenerate_ = 1;
	return newitem;
}

// Add item to list with extra data
template <class T, class D> Refitem<T,D> *Reflist<T,D>::add(T* item, D extradata)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the list
	listHead_ == NULL ? listHead_ = newitem : listTail_->next = newitem;
	newitem->prev = listTail_;
	listTail_ = newitem;
	newitem->item = item;
	newitem->data = extradata;
	nItems_ ++;
	regenerate_ = 1;
	return newitem;
}

// Add item to start of list
template <class T, class D> Refitem<T,D> *Reflist<T,D>::addStart(T* item)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the beginning of the list
	newitem->next = listHead_;
	if (listHead_ != NULL) listHead_->prev = newitem;
	listHead_ = newitem;
	if (listTail_ == NULL) listTail_ = newitem;
	newitem->item = item;
	nItems_ ++;
	regenerate_ = 1;
	return newitem;
}

// Add reference after the specified item
template <class T, class D> Refitem<T,D> *Reflist<T,D>::addAfter(Refitem<T,D> *target, T *item)
{
	if (target == NULL) return add(item);
	else
	{
		Refitem<T,D> *newitem = new Refitem<T,D>;
		newitem->prev = target;
		newitem->next = target->next;
		if (target->next != NULL) target->next->prev = newitem;
		target->next = newitem;
		if (target == listTail_) listTail_ = newitem;
		newitem->item = item;
		nItems_ ++;
		regenerate_ = 1;
		return newitem;
	}
}

// Add item to start of list with extra data
template <class T, class D> Refitem<T,D> *Reflist<T,D>::addStart(T* item, D extradata)
{
	Refitem<T,D> *newitem = new Refitem<T,D>;
	// Add the pointer to the beginning of the list
	newitem->next = listHead_;
	listHead_ == NULL ? listHead_ = newitem : listHead_->prev = newitem;
	listHead_ = newitem;
	newitem->item = item;
	newitem->data = extradata;
	nItems_ ++;
	regenerate_ = 1;
	return newitem;
}

// Add unique item to list
template <class T, class D> Refitem<T,D> *Reflist<T,D>::addUnique(T* item)
{
	Refitem<T,D> *srch = contains(item);
	if (srch == NULL) return add(item);
	else return srch;
}

// Add unique item to list
template <class T, class D> Refitem<T,D> *Reflist<T,D>::addUnique(T* item, D extradata)
{
	Refitem<T,D> *srch = contains(item);
	if (srch == NULL) return add(item, extradata);
	else return srch;
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
	regenerate_ = 1;
}

// Remove Refitem from list
template <class T, class D> void Reflist<T,D>::remove(Refitem<T,D> *xitem)
{
	if (xitem == NULL)
	{
		printf("Internal Error: NULL pointer passed to Reflist<T,D>::remove().\n");
		return;
	}
	// Delete a specific Refitem from the list
	xitem->prev == NULL ? listHead_ = xitem->next : xitem->prev->next = xitem->next;
	xitem->next == NULL ? listTail_ = xitem->prev : xitem->next->prev = xitem->prev;
	delete xitem;
	nItems_ --;
	regenerate_ = 1;
}

// Remove first item from list
template <class T, class D> void Reflist<T,D>::removeFirst()
{
	if (listHead_ == NULL)
	{
		printf("Internal Error: No item to delete in  Reflist<T,D>::removeFirst().\n");
		return;
	}
	remove(listHead_);
	regenerate_ = 1;
}

// Remove last item from list
template <class T, class D> void Reflist<T,D>::removeLast()
{
	if (listTail_ == NULL)
	{
		printf("Internal Error: No item to delete in  Reflist<T,D>::removeFirst().\n");
		return;
	}
	remove(listTail_);
	regenerate_ = 1;
}

// Remove item from list
template <class T, class D> void Reflist<T,D>::remove(T *xitem)
{
	// Delete a specific item from the list
	Refitem<T,D> *r = contains(xitem);
	if (r != NULL) remove(r);
}

// Element access operator
template <class T, class D> Refitem<T,D>* Reflist<T,D>::operator[](int index)
{
	if ((index < 0) || (index >= nItems_))
	{
		printf("Internal Error: Array index (%i) out of bounds (0-%i) in Reflist<T,D>::operator[]\n",index,nItems_-1);
		return NULL;
	}
	// Use array() function to return item
	return array()[index];
}

// Search for item
template <class T, class D> Refitem<T,D>* Reflist<T,D>::contains(T *xitem)
{
	// Search references for specified item
	Refitem<T,D> *r;
	for (r = listHead_; r != NULL; r = r->next) if (r->item == xitem) break;
	return r;
}

// Search for data
template <class T, class D> Refitem<T,D>* Reflist<T,D>::containsData(D data)
{
	// Search references for specified data
	Refitem<T,D> *r;
	for (r = listHead_; r != NULL; r = r->next) if (r->data == data) break;
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
	// Delete static items array if its there
	if (items_ != NULL) delete[] items_;
	items_ = NULL;
	regenerate_ = 1;
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
		if (ri == NULL) printf("Internal Error: Not enough items in list (requested %i, had %i) in Reflist::fillArray()\n",n,nItems_);
	}
	regenerate_ = 1;
}

// Swap the two items specified
template <class T, class D> void Reflist<T,D>::swap(T *item1, T *item2)
{
	if ((item1 == NULL) || (item2 == NULL))
	{
		printf("Internal Error: NULL pointer(s) passed to Reflist<T,D>::swap().\n", item1, item2);
		return;
	}
	T *prev1 = item1->prev, *next1 = item1->next;
	item1->prev = item2->prev;
	item1->next = item2->next;
	item2->prev = prev1;
	item2->next = next1;
	regenerate_ = 1;
}

// Create (or just return) the item array
template <class T, class D> Refitem<T,D> **Reflist<T,D>::array()
{
	if (regenerate_ == 0) return items_;
	// Delete old atom list (if there is one)
	if (items_ != NULL) delete[] items_;
	// Create new list
	items_ = new Refitem<T,D>*[nItems_];
	// Fill in pointers
	int count = 0;
	for (Refitem<T,D> *ri = listHead_; ri != NULL; ri = ri->next) items_[count++] = ri;
	regenerate_ = 0;
	return items_;
}

#endif
