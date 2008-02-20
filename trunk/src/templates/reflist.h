/*
	*** Object reference list
	*** src/templates/reflist.h
	Copyright T. Youngs 2007

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

#ifndef H_REFLIST_H
#define H_REFLIST_H

#include "base/debug.h"
#include <stddef.h>
#include <stdio.h>

// Reference item
template <class T, class D> class refitem
{
	public:
	// Constructor / Destructor
	refitem<T,D>();
	~refitem();

	// List pointers
	refitem<T,D> *prev, *next;
	// Pointer to item
	T *item;
	// Additional temporary info stored in structure
	D data;
};

// Reference list
template <class T, class D> class reflist
{
	public:
	// Constructor / Destructor
	reflist<T,D>();
	~reflist();

	/*
	// Reference List of Items
	*/
	private:
	// Head and tail of reference items
	refitem<T,D> *items_head, *items_tail;
	// Number of items in list
	int nitems;

	public:
	// Returns the head of the atom list
	refitem<T,D> *first() { return items_head; }
	// Returns the last item in the list
	refitem<T,D> *last() { return items_tail; }
	// Returns the number of atoms in the list
	int size() { return nitems; }
	// Add reference to the list
	void add(T*);
	// Add reference to the list with extra data
	void add(T* x, D extradata);
	// Add reference to list, unless already there
	void add_unique(T*);
	// Delete the reference from the list
	void remove(refitem<T,D>*);
	// Delete the reference containing specified item from the list
	void remove(T*);
	// Element access operator
	refitem<T,D> *operator[](int);
	// Search references for item
	refitem<T,D> *search(T*);
	// Clear the list of all references
	void clear();
	// Move head of list to tail of list
	void move_head_to_tail();
	// Add references to contents of list (head of which is provided)
	void create_from_list(T*);
	// Fills the supplied array with pointer values to the reference items
	void fill_array(int, T**);
};

// Constructors
template <class T, class D> refitem<T,D>::refitem()
{
	item = NULL;
	next = NULL;
	prev = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_REFLISTITEM] ++;
	#endif
}

template <class T, class D> reflist<T,D>::reflist()
{
	items_head = NULL;
	items_tail = NULL;
	nitems = 0;
	#ifdef MEMDEBUG
		memdbg.create[MD_REFLIST] ++;
	#endif
}

// Destructors
template <class T, class D> reflist<T,D>::~reflist()
{
	clear();
	#ifdef MEMDEBUG
		memdbg.destroy[MD_REFLIST] ++;
	#endif
}

template <class T, class D> refitem<T,D>::~refitem()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_REFLISTITEM] ++;
	#endif
}

// Add item to list
template <class T, class D> void reflist<T,D>::add(T* target)
{
	refitem<T,D> *newitem = new refitem<T,D>;
	// Add the pointer to the list
	items_head == NULL ? items_head = newitem : items_tail->next = newitem;
	newitem->prev = items_tail;
	items_tail = newitem;
	newitem->item = target;
	nitems ++;
}

// Add item to list with extra data
template <class T, class D> void reflist<T,D>::add(T* target, D extradata)
{
	refitem<T,D> *newitem = new refitem<T,D>;
	// Add the pointer to the list
	items_head == NULL ? items_head = newitem : items_tail->next = newitem;
	newitem->prev = items_tail;
	items_tail = newitem;
	newitem->item = target;
	newitem->data = extradata;
	nitems ++;
}

// Add unique item to list
template <class T, class D> void reflist<T,D>::add_unique(T* target)
{
	if (search(target) == NULL) add(target);
}

// Remove refitem from list
template <class T, class D> void reflist<T,D>::remove(refitem<T,D> *xitem)
{
	// Delete a specific refitem from the list
	xitem->prev == NULL ? items_head = xitem->next : xitem->prev->next = xitem->next;
	xitem->next == NULL ? items_tail = xitem->prev : xitem->next->prev = xitem->prev;
	delete xitem;
	nitems --;
}

// Remove item from list
template <class T, class D> void reflist<T,D>::remove(T *xitem)
{
	// Delete a specific item from the list
	refitem<T,D> *r = search(xitem);
	if (r != NULL) remove(r);
}

// Element access operator
template <class T, class D> refitem<T,D>* reflist<T,D>::operator[](int index)
{
	if (index >= nitems)
	{
		printf("reflist::[] <<<< SEVERE - Array index (%i) out of bounds (0-%i) >>>>\n",index,nitems-1);
		return NULL;
	}
	// Scan through for element number 'index' in the list and return it
	refitem<T,D> *result = items_head;
	for (int i=0; i<index; i++) result = result->next;
	return result;
}

// Search for item
template <class T, class D> refitem<T,D>* reflist<T,D>::search(T *xitem)
{
	// Search references for specified item
	refitem<T,D> *result = NULL;
	refitem<T,D> *r = items_head;
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
template <class T, class D> void reflist<T,D>::clear()
{
	// Clear the list 
	refitem<T,D> *xitem = items_head;
	while (xitem != NULL)
	{
		remove(xitem);
		xitem = items_head;
	}
}

// Move head to tail
template <class T, class D> void reflist<T,D>::move_head_to_tail()
{
	// Add a new item to the list (a copy of the current head)
	add(items_head->item,items_head->data);
	// Delete head item
	remove(items_head);
}

// Create from list
template <class T, class D> void reflist<T,D>::create_from_list(T *xitem)
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
template <class T, class D> void reflist<T,D>::fill_array(int n, T **data)
{
	int count = 0;
	refitem<T,D> *ri = items_head;
	while (ri != NULL)
	{
		data[count] = ri->item;
		count ++;
		if (count == n) break;
		ri = ri->next;
		if (ri == NULL) printf("reflist::fill_array <<<< Not enough items in list - requested %i, had %i >>>>\n",n,nitems);
	}
}

#endif
