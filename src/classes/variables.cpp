/*
	*** Associative variable list
	*** src/classes/variables.cpp
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

#include "classes/variables.h"
#include "classes/atom.h"
#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "model/model.h"
#include "base/elements.h"
#include <string.h>
#include <stdarg.h>

// Variable Types
const char *VT_keywords[VT_NITEMS] = { "char", "int", "double", "atom*", "pattern*", "model*", "bond*", "angle*", "torsion*", "atomtype*" };
const char *text_from_VT(variable_type vt)
	{ return VT_keywords[vt]; }

// Constructors
variable::variable()
{
	prev = NULL;
	next = NULL;
	name.set("unnamed");
	type = VT_NITEMS;
	constant = FALSE;
	#ifdef MEMDEBUG
		memdbg.create[MD_VARIABLE] ++;
	#endif
}

variable_list::variable_list()
{
	#ifdef MEMDEBUG
		memdbg.create[MD_VARIABLELIST] ++;
	#endif
}

// Destructors
variable::~variable()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_VARIABLE] ++;
	#endif
}

variable_list::~variable_list()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_VARIABLELIST] ++;
	#endif
}

// Print
void variable::print()
{
	switch (type)
	{
		case (VT_CHAR):
			printf("Variable '%s', type 'char', value '%s'.\n", name.get(), get_as_char());
			break;
		case (VT_INTEGER):
			printf("Variable '%s', type 'int', value '%i'.\n", name.get(), get_as_int());
			break;
		case (VT_DOUBLE):
			printf("Variable '%s', type 'double', value '%f'.\n", name.get(), get_as_double());
			break;
		case (VT_ATOM):
		case (VT_MODEL):
		case (VT_PATTERN):
		case (VT_BOND):
		case (VT_ANGLE):
		case (VT_TORSION):
			printf("Variable '%s', type '%s', value '%li'.\n", name.get(), text_from_VT(type), ptrvalue);
			break;
		case (VT_ATOMTYPE):
			printf("Variable '%s', type 'atomtype', value '%i'.\n", name.get(), get_as_int());
			break;
	}
}

// Set (from char)
void variable::set(const char *s)
{
	if (type == VT_CHAR) charvalue.set(s);
	else if (type == VT_INTEGER) intvalue = atoi(s);
	else if (type == VT_DOUBLE) doublevalue = atof(s);
	else printf("variable::set <<<< Can't set variable '%s' which is of type '%s' from a character string >>>>\n", name.get(), text_from_VT(type));
}

// Set (int)
void variable::set(int i)
{
	if (type == VT_CHAR) charvalue.set(itoa(i));
	else if (type == VT_INTEGER) intvalue = i;
	else if (type == VT_DOUBLE) doublevalue = i;
	else printf("variable::set <<<< Can't set variable '%s' which is of type '%s' from an integer value >>>>\n", name.get(), text_from_VT(type));
}

// Set (double)
void variable::set(double d)
{
	if (type == VT_CHAR) charvalue.set(ftoa(d));
	else if (type == VT_INTEGER) intvalue = int(d);
	else if (type == VT_DOUBLE) doublevalue = d;
	else printf("variable::set <<<< Can't set variable '%s' which is of type '%s' from a double value >>>>\n", name.get(), text_from_VT(type));
}

// Set (atom*)
void variable::set(atom *i)
{
	if (type != VT_ATOM)
	{
		printf("variable::set <<<< Tried to set variable '%s' which is of type '%s' as if it were of type 'atom*' >>>>\n",name.get(), text_from_VT(type));
		return;
	}
	ptrvalue = i;
	msg(DM_VERBOSE,"Atom variable '%s' set to '%li' ('%s')\n",name.get(),i,(i == NULL ? "" : elements.symbol(i)));
}

// Set (pattern)
void variable::set(pattern *p)
{
	if (type != VT_PATTERN)
	{
		printf("variable::set <<<< Tried to set variable '%s' which is of type '%s' as if it were of type 'pattern*' >>>>\n",name.get(), text_from_VT(type));
		return;
	}
	ptrvalue = p;
	msg(DM_VERBOSE,"Pattern variable '%s' set to '%li' ('%s')\n",name.get(),p,(p == NULL ? "" : p->get_name()));
}

// Set (model)
void variable::set(model *m)
{
	if (type != VT_MODEL)
	{
		printf("variable::set <<<< Tried to set variable '%s' which is of type '%s' as if it were of type 'model*' >>>>\n",name.get(), text_from_VT(type));
		return;
	}
	ptrvalue = m;
	msg(DM_VERBOSE,"Model variable '%s' set to '%li' ('%s')\n",name.get(),m,(m == NULL ? "" : m->get_name()));
}

// Set (patbound)
void variable::set(patbound *pb)
{
	if (type < VT_BOND)
	{
		printf("variable::set <<<< Tried to set variable '%s' which is of type '%s' as if it were of type 'patbound*' >>>>\n",name.get(), text_from_VT(type));
		return;
	}
	ptrvalue = pb;
	msg(DM_VERBOSE,"PatBound variable '%s' set to '%li'\n",name.get(),pb);
}

// Get as char
const char *variable::get_as_char()
{
	switch (type)
	{
		case (VT_CHAR):
			return charvalue.get();
		case (VT_INTEGER):
			return itoa(intvalue);
		case (VT_DOUBLE):
			return ftoa(doublevalue);
		default:
			msg(DM_VERBOSE,"variable::get_as_char <<<< Tried to get variable '%s' which is of type '%s' >>>>\n", name.get(), text_from_VT(type));
	}
	return "";
}

// Get as int
int variable::get_as_int()
{
	switch (type)
	{
		case (VT_CHAR):
			return atoi(charvalue.get());
		case (VT_INTEGER):
			return intvalue;
		case (VT_DOUBLE):
			return int(doublevalue);
		default:
			msg(DM_VERBOSE,"variable::get_as_int <<<< Tried to get variable '%s' which is of type '%s' >>>>\n", name.get(), text_from_VT(type));
	}
	return 0;
}

// Get as double
double variable::get_as_double()
{
	switch (type)
	{
		case (VT_CHAR):
			return atof(charvalue.get());
		case (VT_INTEGER):
			return double(intvalue);
		case (VT_DOUBLE):
			return doublevalue;
		default:
			msg(DM_VERBOSE,"variable::get_as_double <<<< Tried to get variable '%s' which is of type '%s' >>>>\n", name.get(), text_from_VT(type));
	}
	return 0.0;
}

// Get as boolean
bool variable::get_as_bool()
{
	switch (type)
	{
		case (VT_CHAR):
			return charvalue.as_bool();
		case (VT_INTEGER):
			return (intvalue < 1 ? FALSE : TRUE);
		default:
			msg(DM_VERBOSE,"variable::get_as_bool <<<< Tried to get variable '%s' which is of type '%s' >>>>\n", name.get(), text_from_VT(type));
	}
	return FALSE;
}

// Reset
void variable::reset()
{
	switch (type)
	{
		case (VT_CHAR):
			charvalue.set("");
			break;
		case (VT_INTEGER):
			intvalue = 0;
			break;
		case (VT_DOUBLE):
			doublevalue = 0.0;
			break;
		case (VT_ATOM):
		case (VT_PATTERN):
		case (VT_MODEL):
		case (VT_BOND):
		case (VT_ANGLE):
		case (VT_TORSION):
		case (VT_ATOMTYPE):
			ptrvalue = NULL;
			break;
	}
}

// Integer increase
void variable::increase(int n)
{
	switch (type)
	{
		case (VT_INTEGER):
			intvalue ++;
			break;
		case (VT_DOUBLE):
			doublevalue += 1.0;
			break;
		case (VT_ATOM):
			ptrvalue = ( (atom*) ptrvalue)->next;
			break;
		case (VT_PATTERN):
			ptrvalue = ( (pattern*) ptrvalue)->next;
			break;
		case (VT_MODEL):
			ptrvalue = ( (model*) ptrvalue)->next;
			break;
		case (VT_BOND):
		case (VT_ANGLE):
		case (VT_TORSION):
			ptrvalue = ( (patbound*) ptrvalue)->next;
			break;
		case (VT_ATOMTYPE):
			ptrvalue = ( (ffatom*) ptrvalue)->next;
			break;
		default:
			printf("variable::increase <<<< Don't know how to increase variable '%s', type '%s' >>>>\n", name.get(), text_from_VT(type));
			break;
	}
}

// Integer Decrease
void variable::decrease(int n)
{
	switch (type)
	{
		case (VT_INTEGER):
			intvalue --;
			break;
		case (VT_DOUBLE):
			doublevalue -= 1.0;
			break;
		case (VT_ATOM):
			ptrvalue = ( (atom*) ptrvalue)->prev;
			break;
		case (VT_PATTERN):
			ptrvalue = ( (pattern*) ptrvalue)->prev;
			break;
		case (VT_MODEL):
			ptrvalue = ( (model*) ptrvalue)->prev;
			break;
		case (VT_BOND):
		case (VT_ANGLE):
		case (VT_TORSION):
			ptrvalue = ( (patbound*) ptrvalue)->prev;
			break;
		case (VT_ATOMTYPE):
			ptrvalue = ( (ffatom*) ptrvalue)->prev;
			break;
		default:
			printf("variable::decrease <<<< Don't know how to decrease variable '%s', type '%s' >>>>\n", name.get(), text_from_VT(type));
			break;
	}
}

// Retrieve named variable
variable* variable_list::get(const char *prefix, const char *suffix)
{
	static char name[128];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		strcat(name,".");
		strcat(name,suffix);
	}
	for (variable *v = vars.first(); v != NULL; v = v->next)
		if (strcmp(name,v->get_name()) == 0) return v;
	return NULL;
}

// Add named variable
variable *variable_list::add_variable(const char *prefix, const char *suffix, variable_type vt)
{
	static char name[128];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		strcat(name,".");
		strcat(name,suffix);
	}
	variable *result = vars.add();
	result->set_name(name);
	result->set_type(vt);
	return result;
}

// Create, don't set, named variable
variable *variable_list::create_variable(const char *prefix, const char *suffix, variable_type vt)
{
	// First, see if this variable already exists
	variable *result = get(prefix, suffix);
	if (result == NULL) result = add_variable(prefix, suffix, vt);
	else
	{
		// Check type of existing variable
		static char name[128];
		strcpy(name,prefix);
		if (suffix[0] != '\0')
		{
			strcat(name,".");
			strcat(name,suffix);
		}
		if (result->get_type() != vt)
		{
			printf("Variable '%s' already exists and is of type '%s'.\n", name, text_from_VT(vt));
			result = NULL;
		}
	}
	return result;
}

// Add constant
variable *variable_list::add_constant(const char *s)
{
	static char newname[24];
	// Sink name as static character value
	variable *result = vars.add();
	strcpy(newname,"_variable");
	strcat(newname,itoa(vars.size()));
	result->set_name(newname);
	// Try to determine type of the argument
	int i, ch, nn = 0, nch = 0, ndp = 0, npm = 0, ne = 0;
	for (i = 0; i < strlen(s); i++)
	{
		ch = s[i];
		if ((ch > 47) && (ch < 58)) nn ++;
		else if (ch == '.') ndp ++;
		else if ((ch == '-') || (ch == '+')) npm ++;
		else if ((ch == 'e') || (ch == 'E')) ne ++;
		else nch ++;
	}
	// Based on the numbers we calculated, try to determine its type
	if ((nch != 0) || (ndp > 1) || (npm > 2) || (ne > 1)) result->set_type(VT_CHAR);
	else if (ndp == 1) result->set_type(VT_DOUBLE);
	else result->set_type(VT_INTEGER);
	//printf("DETERMINED CONSTANT '%s' TO BE OF TYPE '%s'\n",s,text_from_VT(result->get_type()));
	result->set_constant();
	result->set(s);
	return result;
}

// Set existing variable (or add new and set) (VT_CHAR)
void variable_list::set(const char *prefix, const char *name, const char *value)
{
	static char newname[128];
	strcpy(newname,prefix);
	strcat(newname,".");
	strcat(newname,name);
	variable *v = get(newname);
	if (v == NULL) v = add_variable(newname, VT_CHAR);
	v->set(value);
}

// Set existing variable (or add new and set) (VT_INTEGER)
void variable_list::set(const char *prefix, const char *name, int value)
{
	static char newname[128];
	strcpy(newname,prefix);
	strcat(newname,".");
	strcat(newname,name);
	variable *v = get(newname);
	if (v == NULL) v = add_variable(newname, VT_INTEGER);
	v->set(value);
}

// Set existing variable (or add new and set) (VT_DOUBLE)
void variable_list::set(const char *prefix, const char *name, double value)
{
	static char newname[128];
	strcpy(newname,prefix);
	strcat(newname,".");
	strcat(newname,name);
	variable *v = get(newname);
	if (v == NULL) v = add_variable(newname, VT_DOUBLE);
	v->set(value);
}

// Print list of variables in list
void variable_list::print()
{
	for (variable *v = vars.first(); v != NULL; v = v->next)
		printf("VAR=[%s] (%li) VALUE=[%s]\n",v->get_name(),v,v->get_as_char());
}

// Clear all variable values
void variable_list::reset_all()
{
	dbg_begin(DM_CALLS,"variable_list::reset_all");
	for (variable *v = vars.first(); v != NULL; v = v->next) v->reset();
	dbg_end(DM_CALLS,"variable_list::reset_all");
}

// Clear list of variables
void variable_list::reset(const char *s, ...)
{
	dbg_begin(DM_CALLS,"variable_list::reset");
	// List of variables must be ended by "".
	static char name[64];
	va_list namelist;
	va_start(namelist,s);
	variable *v;
	// Reset 's' first
	get(s)->reset();
	do
	{
		strcpy(name,va_arg(namelist,char*));
		if (name[0] != '\0')
		{
			v = get(name);
			if (v == NULL) printf("variable_list::reset <<<< '%s' not in list >>>>\n",name);
			else v->reset();
		}
	} while (name[0] != '\0');
	dbg_end(DM_CALLS,"variable_list::reset");
}
