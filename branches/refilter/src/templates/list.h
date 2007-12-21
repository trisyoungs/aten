/*
	*** Object list class
	*** src/templates/list.h
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

#ifndef H_LIST_H
#define H_LIST_H

#include "base/debug.h"
//#include "base/constants.h"
#include <stdlib.h>
#include <stdio.h>

// Template object for types not including prev/next pointers (e.g. PODs)
template <class T> class listitem
{
	public:
	// Data object
	T data;
	// List pointers
	listitem<T> *prev, *next;
	// Constructor
	listitem<T>();
};

// List structure for user-generated classes (containing prev/next pointers)
template <class T> class list
{
	public:
	// Constructor / Destructor
	list<T>();
	~list();

	/*
	// Item List
	*/
	private:
	// Pointers to head and tail of list
	T *list_head, *list_tail;
	// Number of items in list
	int nitems;
	// Static array of items
	T **items;
	// Array regeneration flag
	bool regenerate;

	public:
	// Returns the number of items in the list
	int size() { return nitems; }
	// Returns the list head
	T *first() { return list_head; }
	// Returns the list tail
	T *last() { return list_tail; }
	// Append an item to the list
	T *add();
	// Insert an item into the list (after supplied item)
	T *insert(T* before);
	// Element access operator
	T *operator[](int);
	// Add the item into this list
	void own(T*);
	// Remove an item from the list
	void remove(T*);
	// Remove an item from the list, and return the next in the list
	T* remove_and_get_next(T*);
	// Bridge items either side of the specified item
	void cut(T*);
	// Fills the supplied array with pointer values to the list items
	void fill_array(int, T**);
	// Clear the list
	void clear();
	// Create empty list of size N
	void create_empty(int);
	// Find list index of supplied item
	int index_of(T*);
	// Generate (if necessary) and return item array
	T **array();

	/*
	// Item Moves
	*/
	private:
	// Swap two items in list
	void swap(T*, T*);

	public:
	// Shift item up (towards head)
	void shift_up(T*);
	// Shift item down (towards tail)
	void shift_down(T*);
	// Move item to end of list
	void move_to_end(T*);
	// Move item to start of list
	void move_to_start(T*);
};

// Constructors
template <class T> list<T>::list()
{
	list_head = NULL;
	list_tail = NULL;
	nitems = 0;
	regenerate = 1;
	items = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_LIST] ++;
	#endif
}

template <class T> listitem<T>::listitem()
{
	prev = NULL;
	next = NULL;
}

// Destructor
template <class T> list<T>::~list()
{
	clear();
	if (items != NULL) delete[] items;
	#ifdef MEMDEBUG
		memdbg.destroy[MD_LIST] ++;
	#endif
}

// Add item to list
template <class T> T* list<T>::add()
{
	T *newitem = new T;
	// Add the pointer to the list
	list_head == NULL ? list_head = newitem : list_tail->next = newitem;
	newitem->prev = list_tail;
	list_tail = newitem;
	nitems ++;
	regenerate = 1;
	return newitem;
}

// Insert new item after supplied item
template <class T> T *list<T>::insert(T* newprev)
{
	T *newitem = new T;
	// Get pointer to next item after newprev (our newnext)
	T *newnext = (newprev == NULL ? list_head : newprev->next);
	// Re-point newprev and the new item
	if (newprev != NULL) newprev->next = newitem;
	else list_head = newitem;
	newitem->prev = newprev;
	// Re-point newnext and the new item
	if (newnext != NULL) newnext->prev = newitem;
	else list_tail = newitem;
	newitem->next = newnext;
	nitems ++;
	regenerate = 1;
	return newitem;
}

// Element access operator
template <class T> T *list<T>::operator[](int index)
{
	if ((index < 0) || (index >= nitems))
	{
		printf("list::[] <<<< SEVERE - Array index (%i) out of bounds (0-%i) >>>>\n",index,nitems-1);
		return NULL;
	}
	// Scan through for element number 'index' in the list and return it
	return array()[index];
}

// Own an existing item
template <class T> void list<T>::own(T* olditem)
{
	// In the interests of 'pointer cleanliness, refuse to own the item if its pointers are not NULL
	if ((olditem->next != NULL) || (olditem->prev != NULL))
	{
		printf("list::own <<<< List refused to own an item that still had ties >>>>\n");
		return;
	}
	list_head == NULL ? list_head = olditem : list_tail->next = olditem;
	olditem->prev = list_tail;
	olditem->next = NULL;
	list_tail = olditem;
	nitems ++;
	regenerate = 1;
}

// Remove the specified item from the list
template <class T> void list<T>::remove(T *xitem)
{
	// Delete a specific item from the list
	xitem->prev == NULL ? list_head = (T*) xitem->next : xitem->prev->next = (T*) xitem->next;
	xitem->next == NULL ? list_tail = (T*) xitem->prev : xitem->next->prev = (T*) xitem->prev;
	delete xitem;
	nitems --;
	regenerate = 1;
}

// Remove the specified item from the list, returning the next
template <class T> T* list<T>::remove_and_get_next(T *xitem)
{
	// Delete a specific item from the list, and return the next in the list
	T* result = xitem->next;
	xitem->prev == NULL ? list_head = (T*) xitem->next : xitem->prev->next = (T*) xitem->next;
	xitem->next == NULL ? list_tail = (T*) xitem->prev : xitem->next->prev = (T*) xitem->prev;
	delete xitem;
	nitems --;
	regenerate = 1;
	return result;
}

// Cut - Bridge items over specified item
template <class T> void list<T>::cut(T *item)
{
	T *prev, *next;
	prev = item->prev;
	next = item->next;
	if (prev == NULL) list_head = next;
	else prev->next = next;
	if (next == NULL) list_tail = prev;
	else next->prev = prev;
	item->next = NULL;
	item->prev = NULL;
	regenerate = 1;
}

// Fill array
template <class T> void list<T>::fill_array(int n, T **data)
{
	int count = 0;
	T *i = list_head;
	while (i != NULL)
	{
		data[count] = i->item;
		count ++;
		if (count == n) break;
		i = i->next;
		if (i == NULL) printf("list::fill_array <<<< Not enough items in list - requested %i, had %i >>>>\n",n,nitems);
	}
}

// Remove all items in the list
template <class T> void list<T>::clear()
{
	T *xitem = list_head;
	while (xitem != NULL)
	{
		remove(xitem);
		xitem = list_head;
	}
	regenerate = 1;
}

// Find index of supplied item
template <class T> int list<T>::index_of(T* item)
{
	int result = 0;
	for (T* i = list_head; i != NULL; i = i->next)
	{
		if (item == i) break;
		result ++;
	}
	if (result == nitems)
	{
		printf("list::index_of <<<< Supplied item is not in this list >>>>.\n");
		result = -1;
	}
	return result;
}

// Swap items
template <class T> void list<T>::swap(T* item1, T* item2)
{
	// If the items are adjacent, swap the pointers 'outside' the pair and swap the next/prev between them
	T *n1, *n2, *p1, *p2;
	if ((item1->next == item2) || (item2->next == item1))
	{
		// Order the pointers so that item1->next == item2
		if (item2->next == item1)
		{
			n1 = item2;
			item2 = item1;
			item1 = n1;
		}
		p1 = item1->prev; 
		n2 = item2->next;
		item2->prev = p1;
		item2->next = item1;
		if (p1 != NULL) p1->next = item2;
		else list_head = item2;
		item1->prev = item2;
		item1->next = n2;
		if (n2 != NULL) n2->prev = item1;
		else list_tail = item1;
	}
	else
	{
		// Store the list pointers of the two items
		//printf("Item 1 %li next %li prev %li\n",item1,item1->next,item1->prev);
		//printf("Item 2 %li next %li prev %li\n",item2,item2->next,item2->prev);
		//printf("Item 1 nextprev %li prevnext %li\n",item1->next->prev,item1->prev->next);
		//printf("Item 2 nextprev %li prevnext %li\n",item2->next->prev,item2->prev->next);
		n1 = item1->next;
		p1 = item1->prev;
		n2 = item2->next;
		p2 = item2->prev;
		// Set new values of swapped items
		item1->next = n2;
		item1->prev = p2;
		item2->next = n1;
		item2->prev = p1;
		//printf("Item 1 next %li prev %li\n",item1->next,item1->prev);
		//printf("Item 2 next %li prev %li\n",item2->next,item2->prev);
		// Set new values of items around swapped items
		if (item1->next != NULL) item1->next->prev = item1;
		else list_tail = item1;
		if (item1->prev != NULL) item1->prev->next = item1;
		else list_head = item1;
		if (item2->next != NULL) item2->next->prev = item2;
		else list_tail = item2;
		if (item2->prev != NULL) item2->prev->next = item2;
		else list_head = item2;
		//printf("Item 1 nextprev %li prevnext %li\n",item1->next->prev,item1->prev->next);
		//printf("Item 2 nextprev %li prevnext %li\n",item2->next->prev,item2->prev->next);
	}
	regenerate = 1;
}

// Shift item towards head
template <class T> void list<T>::shift_up(T* item)
{
	// If the item is already at the head of the list, return NULL.
	if (list_head == item) return;
	T* other = item->prev;
	swap(other,item);
	regenerate = 1;
}

// Shift item towards tail
template <class T> void list<T>::shift_down(T* item)
{
	// If the item is already at the tail of the list, return NULL.
	if (list_tail == item) return;
	T* other = item->next;
	swap(other,item);
	regenerate = 1;
}

// Move item to end
template <class T> void list<T>::move_to_end(T* item)
{
	// If the item is already at the tail, exit
	if (list_tail == item) return;
	T* oldtail = list_tail;
	cut(item);
	item->prev = list_tail;
	item->next = NULL;
	if (list_tail != NULL) list_tail->next = item;
	list_tail = item;
	regenerate = 1;
}

// Move item to start
template <class T> void list<T>::move_to_start(T* item)
{
	// If the item is already at the head, exit
	if (list_head == item) return;
	T* oldhead = list_head;
	cut(item);
	item->prev = NULL;
	item->next = list_head;
	if (list_head != NULL) list_head->prev = item;
	list_head = item;
	regenerate = 1;
}

// Create empty list
template <class T> void list<T>::create_empty(int newsize)
{
	clear();
	for (int n=0; n<newsize; n++) add();
	regenerate = 1;
}

// Create (or just return) the item array
template <class T> T **list<T>::array()
{
	if (regenerate == 0) return items;
	// Delete old atom list (if there is one)
	if (items != NULL) delete[] items;
	// Create new list
	items = new T*[nitems];
	// Fill in pointers
	int count = 0;
	for (T *i = list_head; i != NULL; i = i->next)
	{
	//printf("N=%i\n",count);
		items[count] = i;
		count ++;
	}
	regenerate = 0;
	return items;
}

#endif
