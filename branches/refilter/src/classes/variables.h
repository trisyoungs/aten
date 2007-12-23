/*
	*** Associative variable list
	*** src/classes/variables.h
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

#ifndef H_VARIABLES_H
#define H_VARIABLES_H

#include "templates/list.h"
#include "classes/dnchar.h"
#include "base/sysfunc.h"

// Variable Types
enum variable_type { VT_CHAR, VT_INT, VT_DOUBLE, VT_ATOM, VT_BOND, VT_PATTERN, VT_MODEL, VT_PATBOUND, VT_NITEMS };
const char *text_from_VT(variable_type);

// Forward Declarations
class atom;
class bond;
class unitcell;
class pattern;
class model;
class patbound;

// Variable
class variable
{
	public:
	// Constructor / Destructor
	variable();
	~variable();
	// List pointers
	variable *prev, *next;

	/*
	// Variable Contents
	*/
	private:
	// Name of the variable
	dnchar name;
	// Value of variable
	void *ptrvalue;
	dnchar charvalue;
	int intvalue;
	double doublevalue;
	// Content type of variable
	variable_type type;

	public:
	// Print contents of variable
	void print();
	// Clears value of variable
	void reset();
	// Set name of variable
	void set_name(const char* s) { name.set(s); }
	// Set to constant value
	void set_constant(const char*);
	// Set value of variable (char)
	void set(const char*);
	// Set value of variable (int)
	void set(int i);
	// Set value of variable (double)
	void set(double d);
	// Set value of variable (atom*)
	void set(atom*);
	// Set value of variable (pattern*)
	void set(pattern*);
	// Set value of variable (model*)
	void set(model*);
	// Set value of variable (patbound*)
	void set(patbound*);
	// Copy pointer contents of source variable
	void copy_pointer(variable *v) { ptrvalue = v->ptrvalue; }
	// Sets the content type of the variable
	void set_type(variable_type vt) { type = vt; }
	// Returns content type of the variable
	variable_type get_type() { return type; }
	// Get name of variable
	const char *get_name() { return name.get(); }
	// Get value of variable as character string
	const char *get_as_char();
	// Get value of variable as integer
	int get_as_int();
	// Get value of variable as double
	double get_as_double();
	// Get value of variable as float
	float get_as_float() { float(get_as_double()); }
	// Get value of variable as a boolean
	bool get_as_bool();
	// Get value of variable as gpointer (desired type is specified so a check can be made)
	void *get_as_pointer(variable_type);
	// Integer increase
	void increase(int);
	// Integer decrease
	void decrease(int);
};

// Variable list
class variable_list
{
	public:
	// Constructor / Destructor
	variable_list();
	~variable_list();

	/*
	// Variable List
	*/
	private:
	// List of variables
	list<variable> vars;

	public:
	// Set existing (or create new) variable
	void set(const char*, const char*);
	void set(const char*, int);
	void set(const char*, double);
	void set(const char*, const char*, const char*);
	void set(const char*, const char*, int);
	void set(const char*, const char*, double);
	// Retrieve (don't add) a named variable as a string
	const char *get_as_char(const char*);
	// Retrieve (don't add) a named variable as a double
	double get_as_double(const char*);
	// Retrieve (don't add) a named variable as an integer
	int get_as_int(const char*);
	// Retrieve (or add) a named/typed variable to the list
	variable *get(const char*);
	// Searches for a variable in the list
	variable *find(const char*);
	// Add a named variable to the list
	void add(const char* s) { variable *v = get(s); }
	// Add 'unnamed' variable to the list
	variable *add();
	// Add a number of variables to the list
	void batch_add(const char*, ...);
	// Reset values of all variables
	void reset_all();
	// Reset values of variable selection
	void reset(const char*, ...);
	// Print list of variables and their values
	void print();

	/*
	// Set Object Variables
	*/
	public:
	// Set model variables
	void set_model_variables(model*);
	// Set cell variables
	void set_cell_variables(unitcell*);
	// Set atom variables
	void set_atom_variables(const char*, atom*);
	void set_atom_variables(const char*, int);
	// Get atom variables
	void get_atom_variables(atom *i);
	// Set pattern variables
	void set_pattern_variables(const char*, pattern*);
	// Set pattern (ff) term variables
	void set_patbound_variables(const char*, patbound*);
};

#endif
