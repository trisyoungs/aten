/*
	*** Object reference list
	*** src/templates/reflist.h

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
template <class T> class refitem
{
	public:
	// List pointers
	refitem<T> *prev, *next;
	// Pointer to item
	T *item;
	// Additional temporary info stored in structure
	int data1, data2;
	// Constructor
	refitem<T>();
	// Destructor
	~refitem();
};

// Reference list
template <class T> class reflist
{
	public:
	// Constructor / Destructor
	reflist<T>();
	~reflist();

	/*
	// Reference List of Items
	*/
	private:
	// Head and tail of reference items
	refitem<T> *items_head, *items_tail;
	// Number of items in list
	int nitems;

	public:
	// Returns the head of the atom list
	refitem<T> *first() { return items_head; }
	// Returns the last item in the list
	refitem<T> *last() { return items_tail; }
	// Returns the number of atoms in the list
	int size() { return nitems; }
	// Add reference to the list with data1 == data2 == 0
	void add(T* x) { add(x,0,0); }
	// Add reference to the list with extra data
	void add(T*, int, int);
	// Add reference to list, unless already there
	void add_unique(T*, int, int);
	// Delete the reference from the list
	void remove(refitem<T>*);
	// Delete the reference containing specified item from the list
	void remove(T*);
	// Element access operator
	refitem<T> *operator[](int);
	// Search references for item
	refitem<T> *search(T*);
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
template <class T> refitem<T>::refitem()
{
	item = NULL;
	next = NULL;
	prev = NULL;
	data1 = 0;
	data2 = 0;
	#ifdef MEMDEBUG
	memdbg.create[MD_REFLISTITEM] ++;
	#endif
}

template <class T> reflist<T>::reflist()
{
	items_head = NULL;
	items_tail = NULL;
	nitems = 0;
	#ifdef MEMDEBUG
	memdbg.create[MD_REFLIST] ++;
	#endif
}

// Destructors
template <class T> reflist<T>::~reflist()
{
	clear();
	#ifdef MEMDEBUG
	memdbg.destroy[MD_REFLIST] ++;
	#endif
}

template <class T> refitem<T>::~refitem()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_REFLISTITEM] ++;
	#endif
}

// Add item to list
template <class T> void reflist<T>::add(T* target, int i, int j)
{
	refitem<T> *newitem = new refitem<T>;
	// Add the pointer to the list
	items_head == NULL ? items_head = newitem : items_tail->next = newitem;
	newitem->prev = items_tail;
	items_tail = newitem;
	newitem->item = target;
	newitem->data1 = i;
	newitem->data2 = j;
	nitems ++;
}

// Add unique item to list
template <class T> void reflist<T>::add_unique(T* target, int i, int j)
{
	if (search(target) == NULL) add(target,i,j);
}

// Remove refitem from list
template <class T> void reflist<T>::remove(refitem<T> *xitem)
{
	// Delete a specific refitem from the list
	xitem->prev == NULL ? items_head = xitem->next : xitem->prev->next = xitem->next;
	xitem->next == NULL ? items_tail = xitem->prev : xitem->next->prev = xitem->prev;
	delete xitem;
	nitems --;
}

// Remove item from list
template <class T> void reflist<T>::remove(T *xitem)
{
	// Delete a specific item from the list
	refitem<T> *r = search(xitem);
	if (r != NULL) remove(r);
}

// Element access operator
template <class T> refitem<T>* reflist<T>::operator[](int index)
{
	if (index >= nitems)
	{
		printf("reflist::[] <<<< SEVERE - Array index (%i) out of bounds (0-%i) >>>>\n",index,nitems-1);
		return NULL;
	}
	// Scan through for element number 'index' in the list and return it
	refitem<T> *result = items_head;
	for (int i=0; i<index; i++) result = result->next;
	return result;
}

// Search for item
template <class T> refitem<T>* reflist<T>::search(T *xitem)
{
	// Search references for specified item
	refitem<T> *result = NULL;
	refitem<T> *r = items_head;
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
template <class T> void reflist<T>::clear()
{
	// Clear the list 
	refitem<T> *xitem = items_head;
	while (xitem != NULL)
	{
		remove(xitem);
		xitem = items_head;
	}
}

// Move head to tail
template <class T> void reflist<T>::move_head_to_tail()
{
	// Add a new item to the list (a copy of the current head)
	add(items_tail->item,items_tail->data1,items_tail->data2);
	// Delete head item
	remove(items_head);
}

// Create from list
template <class T> void reflist<T>::create_from_list(T *xitem)
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
template <class T> void reflist<T>::fill_array(int n, T **data)
{
	int count = 0;
	refitem<T> *ri = items_head;
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
