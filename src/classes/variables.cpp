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
const char *VT_keywords[VT_NITEMS] = { "undefined", "constant", "generic", "atom*", "bond*", "pattern*", "model*", "patbound*" };
const char *text_from_VT(variable_type vt)
	{ return VT_keywords[vt]; }

// Constructors
variable::variable()
{
	prev = NULL;
	next = NULL;
	name.set("unnamed");
	type = VT_UNDEFINED;
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
		case (VT_GENERIC):
		case (VT_GENERICCONSTANT):
			printf("Variable '%s', type '%s', value '%s'.\n", name.get(), text_from_VT(type), get_as_char());
			break;
		case (VT_ATOM):
		case (VT_BOND):
		case (VT_MODEL):
		case (VT_PATTERN):
		case (VT_PATBOUND):
			printf("Variable '%s', type '%s', value '%li'.\n", name.get(), text_from_VT(type), ptrvalue);
			break;
		case (VT_UNDEFINED):
			printf("Variable '%s', no type.\n", name.get());
			break;
	}
}

// Set constant
void variable::set_constant(const char *s)
{
	type = VT_GENERICCONSTANT;
	charvalue.set(s);
}

// Set (char)
void variable::set(const char *s)
{
	if (type == VT_UNDEFINED) type = VT_GENERIC;
	switch (type)
	{
		case (VT_GENERIC):
			charvalue.set(s);
			break;
		default:
			printf("variable::set <<<< Tried to set variable '%s' which is of type '%s' as if it were of type 'generic' >>>>\n", name.get(), text_from_VT(type));
	}
}

// Set (atom*)
void variable::set(atom *i)
{
	if (type == VT_UNDEFINED) type = VT_ATOM;
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
	if (type == VT_UNDEFINED) type = VT_PATTERN;
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
	if (type == VT_UNDEFINED) type = VT_MODEL;
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
	if (type == VT_UNDEFINED) type = VT_PATBOUND;
	if (type != VT_PATBOUND)
	{
		printf("variable::set <<<< Tried to set variable '%s' which is of type '%s' as if it were of type 'patbound*' >>>>\n",name.get(), text_from_VT(type));
		return;
	}
	ptrvalue = pb;
	msg(DM_VERBOSE,"PatBound variable '%s' set to '%li'\n",name.get(),pb);
}

// Set type
bool variable::set_type(variable_type vt)
{
	// Check previous type - if they're not the same (and isn't VT_UNDEFINED) raise an error.
	// We won't allow variables to be re-cast in the script
	if (type == VT_UNDEFINED) type = vt;
	else if (type != vt)
	{
		printf("variable::set_type <<<< Cannot re-cast variable '%s' from '%s' to '%s' >>>>\n", name.get(), text_from_VT(type), text_from_VT(vt));
		return FALSE;
	}
	type = vt;
	return TRUE;
}

// Get as char
const char *variable::get_as_char()
{
	switch (type)
	{
		case (VT_GENERICCONSTANT):
		case (VT_GENERIC):
			return charvalue.get();
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
		case (VT_GENERICCONSTANT):
		case (VT_GENERIC):
			return atoi(charvalue.get());
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
		case (VT_GENERICCONSTANT):
		case (VT_GENERIC):
			return atof(charvalue.get());
		default:
			msg(DM_VERBOSE,"variable::get_as_double <<<< Tried to get variable '%s' which is of type '%s' >>>>\n", name.get(), text_from_VT(type));
	}
	return 0.0;
}

// Get as float
float variable::get_as_float()
{
	switch (type)
	{
		case (VT_GENERICCONSTANT):
		case (VT_GENERIC):
			return float(atof(charvalue.get()));
		default:
			msg(DM_VERBOSE,"variable::get_as_float <<<< Tried to get variable '%s' which is of type '%s' >>>>\n", name.get(), text_from_VT(type));
	}
	return 0.0;
}

// Get as boolean
bool variable::get_as_bool()
{
	switch (type)
	{
		case (VT_GENERICCONSTANT):
		case (VT_GENERIC):
			return charvalue.as_bool();
			break;
		default:
			msg(DM_VERBOSE,"variable::get_as_bool <<<< Tried to get variable '%s' which is of type '%s' >>>>\n", name.get(), text_from_VT(type));
	}
	return FALSE;
}

// Get as gpointer
void *variable::get_as_pointer(variable_type vt)
{
	if ((type != vt) && (vt != VT_UNDEFINED))
	{
		printf("variable::get_as_pointer <<<< Tried to get variable '%s' which is of type '%s' but thought it was of type '%s' >>>>\n", name.get(), text_from_VT(type), text_from_VT(vt));
		return NULL;
	}
	else return ptrvalue;
}

// Reset
void variable::reset()
{
	switch (type)
	{
		case (VT_GENERIC):
			charvalue.set("");
			break;
		case (VT_ATOM):
		case (VT_BOND):
		case (VT_PATTERN):
		case (VT_MODEL):
		case (VT_PATBOUND):
			ptrvalue = NULL;
			break;
	}
}

// Integer increase
void variable::increase(int n)
{
	switch (type)
	{
		case (VT_GENERIC):
			charvalue.set(itoa(atoi(charvalue.get()) + 1));
			break;
		case (VT_ATOM):
			ptrvalue = ( (atom*) ptrvalue)->next;
			break;
		case (VT_BOND):
			printf("Can't increase with bond->next\n");
			break; //ptrvalue = ( (bond*) ptrvalue)->next; break;
		case (VT_PATTERN):
			ptrvalue = ( (pattern*) ptrvalue)->next;
			break;
		case (VT_MODEL):
			ptrvalue = ( (model*) ptrvalue)->next;
			break;
		case (VT_PATBOUND):
			ptrvalue = ( (patbound*) ptrvalue)->next;
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
		case (VT_GENERIC):
			charvalue.set(itoa(atoi(charvalue.get()) - 1));
			break;
		case (VT_ATOM):
			ptrvalue = ( (atom*) ptrvalue)->prev;
			break;
		case (VT_BOND):
			printf("Can't increase with bond->prev\n");
			//ptrvalue = ( (bond*) ptrvalue)->prev;
			break;
		case (VT_PATTERN):
			ptrvalue = ( (pattern*) ptrvalue)->prev;
			break;
		case (VT_MODEL):
			ptrvalue = ( (model*) ptrvalue)->prev;
			break;
		case (VT_PATBOUND):
			ptrvalue = ( (patbound*) ptrvalue)->prev;
			break;
		default:
			printf("variable::decrease <<<< Don't know how to decrease variable '%s', type '%s' >>>>\n", name.get(), text_from_VT(type));
			break;
	}
}

// Set existing (or create new) variable
void variable_list::set(const char *s, const char* val)
{
	variable *v = find(s);
	if (v == NULL) v = add();
	v->set_name(s);
	v->set(val);	
}

void variable_list::set(const char *s, int i)
{
	variable *v = find(s);
	if (v == NULL) v = add();
	v->set_name(s);
	v->set(i);	
}

void variable_list::set(const char *s, double d)
{
	variable *v = find(s);
	if (v == NULL) v = add();
	v->set_name(s);
	v->set(d);	
}

// Set existing or new variable with prefix/suffix namestyle (calls normal set methods)
void variable_list::set(const char *prefix, const char *suffix, const char *s)
{
	static char name[128];
	strcpy(name,prefix);
	strcat(name,".");
	strcat(name,suffix);
	set(name,s);
}

void variable_list::set(const char *prefix, const char *suffix, int i)
{
	static char name[128];
	strcpy(name,prefix);
	strcat(name,".");
	strcat(name,suffix);
	set(name,i);
}

void variable_list::set(const char *prefix, const char *suffix, double d)
{
	static char name[128];
	strcpy(name,prefix);
	strcat(name,".");
	strcat(name,suffix);
	set(name,d);
}

// Retrieve (or add) variable
variable *variable_list::get(const char *s)
{
	// Search list for existing variable by this name and type...
	variable *result;
	static char newname[24];
	// If string 's' is empty, return NULL
	if (s[0] == '\0') return NULL;
	// If the first character is '$' then it gets added or searched for as a variable.
	// If not, sink it as a value. If '*' is the first character, 'get' as a variable.
	if ((s[0] == '$') || (s[0] == '*'))
	{
		// A variable - see if it exists in the list already
		if (s[0] == '$') s++;
		result = find(s);
		if (result == NULL)
		{
			result = vars.add();
			result->set_name(s);
		}
		// If the variable is '*' (the 'discard' formatter) set it to char type
		if (s[0] == '*') result->set("");
	}
	else
	{
		result = vars.add();
		strcpy(newname,"_variable");
		strcat(newname,itoa(vars.size()));
		result->set_name(newname);
		result->set_constant(s);
	}
	return result;
}

// Retrieve (don't add) as double
double variable_list::get_as_double(const char *s)
{
	// Try to 'find' the variable, returning 0.0 if it's not in the list
	variable *v = find(s);
	return (v == NULL ? 0.0 : v->get_as_double());
}

// Retrieve (don't add) as float
float variable_list::get_as_float(const char *s)
{
	// Try to 'find' the variable, returning 0.0 if it's not in the list
	variable *v = find(s);
	return (v == NULL ? 0.0 : v->get_as_float());
}

// Retrieve (don't add) as integer
int variable_list::get_as_int(const char *s)
{
	// Try to 'find' the variable, returning 0 if it's not in the list
	variable *v = find(s);
	return (v == NULL ? 0 : v->get_as_int());
}

// Retrieve (don't add) as string
const char *variable_list::get_as_char(const char *s)
{
	// Try to 'find' the variable, returning "" if it's not in the list
	variable *v = find(s);
	return (v == NULL ? "" : v->get_as_char());
}

// Add 'unnamed' variable
variable* variable_list::add()
{
	static char newname[24];
	variable *result = vars.add();
	strcpy(newname,"_variable");
	strcat(newname,itoa(vars.size()));
	result->set_name(newname);
	return result;
}

// Add number of varaibles to the list
void variable_list::batch_add(const char *s, ...)
{
	dbg_begin(DM_CALLS,"variable_list::batch_add");
	// List of variables must be ended by "".
	static char name[64];
	va_list namelist;
	va_start(namelist,s);
	// Reset 's' first
	add(s);
	do
	{
		strcpy(name,va_arg(namelist,char*));
		if (name[0] != '\0') add(name);
	} while (name[0] != '\0');
	dbg_end(DM_CALLS,"variable_list::batch_add");
}

// Search for variable
variable* variable_list::find(const char *s)
{
	dbg_begin(DM_CALLS,"variable_list::find");
	variable *result;
	for (result = vars.first(); result != NULL; result = result->next)
		if (strcmp(s,result->get_name()) == 0) break;
	dbg_end(DM_CALLS,"variable_list::find");
	return result;
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
	// Reset 's' first
	get(s)->reset();
	do
	{
		strcpy(name,va_arg(namelist,char*));
		if (name[0] != '\0') get(name)->reset();
	} while (name[0] != '\0');
	dbg_end(DM_CALLS,"variable_list::reset");
}

// Set variables for model
void  variable_list::set_model_variables(model *m)
{
	dbg_begin(DM_CALLS,"variable_list::set_model_variables");
	if (m != NULL)
	{
		set("title",m->get_name());
		set("natoms",m->get_natoms());
	}
	else reset("title","natoms","");
	dbg_end(DM_CALLS,"variable_list::set_model_variables");
}

// Set variables for cell
void variable_list::set_cell_variables(unitcell *c)
{
	dbg_begin(DM_CALLS,"variable_list::set_cell_variables");
	mat3<double> mat;
	vec3<double> vec;
	if (c != NULL)
	{
		set("cell.type",lower_case(text_from_CT(c->get_type())));
		mat = c->get_axes_transpose();
		set("cell.a.x",mat.rows[0].x);
		set("cell.b.x",mat.rows[0].y);
		set("cell.c.x",mat.rows[0].z);
		set("cell.a.y",mat.rows[1].x);
		set("cell.b.y",mat.rows[1].y);
		set("cell.c.y",mat.rows[1].z);
		set("cell.a.z",mat.rows[2].x);
		set("cell.b.z",mat.rows[2].y);
		set("cell.c.z",mat.rows[2].z);
		vec = c->get_lengths();
		set("cell.a",vec.x);
		set("cell.b",vec.y);
		set("cell.c",vec.z);
		vec = c->get_angles();
		set("cell.alpha",vec.x);
		set("cell.beta",vec.y);
		set("cell.gamma",vec.z);
	}
	else
	{
		reset("cell.type","cell.a.x","cell.a.y","cell.a.z","cell.b.x","cell.b.y","cell.b.z","cell.c.x","cell.c.y","cell.c.z","");
		reset("cell.a","cell.b","cell.c","cell.alpha","cell.beta","cell.gamma","");
	}
	dbg_end(DM_CALLS,"variable_list::set_cell_variables");
}

// Set variable values for atom
void variable_list::set_atom_variables(const char *varname, atom *i)
{
	dbg_begin(DM_CALLS,"variable_list::set_atom_variables");
	vec3<double> v;
	if (i != NULL)
	{
		// Element and ff type
		set(varname,"symbol",elements.symbol(i));
		set(varname,"mass",elements.mass(i));
		set(varname,"name",elements.name(i));
		set(varname,"z",i->get_element());
		set(varname,"id",i->get_id()+1);
		ffatom *ffa = i->get_fftype();
		set(varname,"fftype",(ffa == NULL ? elements.symbol(i) : ffa->get_name()));
		set(varname,"ffequiv",(ffa == NULL ? elements.symbol(i) : ffa->get_equiv()));
		v = i->r;
		set(varname,"r.x",v.x);
		set(varname,"r.y",v.y);
		set(varname,"r.z",v.z);
		v = i->f;
		set(varname,"f.x",v.x);
		set(varname,"f.y",v.y);
		set(varname,"f.z",v.z);
		v = i->v;
		set(varname,"v.x",v.x);
		set(varname,"v.y",v.y);
		set(varname,"v.z",v.z);
		set(varname,"q",i->get_charge());
	}
	else
	{
		reset("symbol","mass","name","z","fftype","ffequiv","");
		reset("r.x","r.y","r.z","f.x","f.y","f.z","v.x","v.y","v.z","q","");
	}
	dbg_end(DM_CALLS,"variable_list::set_atom_variables");
}

// Set variables for pattern
void variable_list::set_pattern_variables(const char *varname, pattern *p)
{
	dbg_begin(DM_CALLS,"variable_list::set_pattern_variables");
	if (p != NULL)
	{
		set(varname,"name",p->get_name());
		set(varname,"nmols",p->get_nmols());
		set(varname,"nmolatoms",p->get_natoms());
		set(varname,"nbonds",p->bonds.size());
		set(varname,"nangles",p->angles.size());
		set(varname,"ntorsions",p->torsions.size());
	}
	else reset("patname","nmols","nmolatoms","nffbonds","nffangles","nfftorsions","");
	dbg_end(DM_CALLS,"variable_list::set_pattern_variables");
}

// Set variables for patbound
void variable_list::set_patbound_variables(const char *varname, patbound *pb)
{
	dbg_begin(DM_CALLS,"variable_list::set_patbound_variables");
	static ffparams ffp;
	static ffbound *ffb;
	static char parm[24];
	int i;
	if (pb != NULL)
	{
		// Grab ffbound pointer from pattern bound structure
		ffb = pb->get_data();
		// Set atom ids involved
		strcpy(parm,"id_X");
		for (i = 0; i < MAXFFBOUNDTYPES; i++)
		{

			parm[3] = 105 + i;
			set(varname,parm,pb->get_atomid(i)+1);
		}
		// Set type names involved
		strcpy(parm,"type_X");
		for (i = 0; i < MAXFFBOUNDTYPES; i++)
		{
			parm[5] = 105 + i;
			set(varname,parm,ffb->get_type(i));
		}
		// Grab ffparams data
		ffp = ffb->get_params();
		strcpy(parm,"param_X");
		for (int i = 0; i < MAXFFPARAMDATA; i++)
		{
			parm[6] = 97 + i;
			set(varname,parm,ffp.data[i]);
		}
		switch (ffb->get_type())
		{
			case (FFC_BOND):
				set(varname,"funcform",text_from_BF(ffb->get_funcform().bondfunc));
				break;
			case (FFC_ANGLE):
				set(varname,"funcform",text_from_AF(ffb->get_funcform().anglefunc));
				break;
			case (FFC_TORSION):
				set(varname,"funcform",text_from_TF(ffb->get_funcform().torsionfunc));
				break;
			default:	
				printf("variable_list::set_patbound_variables <<<< Funcform not defined >>>>\n");
		}
		
	}
	else reset("funcform","typei","typej","typek","typel","param_a","param_b","param_c","param_d","");
	dbg_end(DM_CALLS,"variable_list::set_patbound_variables");
}

// Get atom variables from list
void variable_list::get_atom_variables(atom *i)
{
	dbg_begin(DM_CALLS,"variable_list::get_atom_variables");
	variable *v;
	static vec3<double> vec1;
	// Element is not set here (needs too many other things to work)
	// Set charge
	v = find("q");
	if (v != NULL)
	{
		i->set_charge(v->get_as_double());
		v->reset();
	}
	// Set temporary atom ID
	v = find("id");
	if (v != NULL)
	{
		i->set_id(v->get_as_int());
		v->reset();
	}
	// Set positions
	v = find("r.x");
	vec1.set(0, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	v = find("r.y");
	vec1.set(1, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	v = find("r.z");
	vec1.set(2, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	i->r = vec1;
	// Set forces
	v = find("f.x");
	vec1.set(0, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	v = find("f.y");
	vec1.set(1, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	v = find("f.z");
	vec1.set(2, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	i->f = vec1;
	// Set velocities
	v = find("v.x");
	vec1.set(0, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	v = find("v.y");
	vec1.set(1, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	v = find("v.z");
	vec1.set(2, (v == NULL ? 0.0 : v->get_as_double()));
	if (v != NULL) v->reset();
	i->v = vec1;
	dbg_end(DM_CALLS,"variable_list::get_atom_variables");
}
