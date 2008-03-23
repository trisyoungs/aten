/*
	*** Associative variable list
	*** src/classes/variables.cpp
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

#include "classes/variables.h"
#include "classes/atom.h"
#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "model/model.h"
#include "base/elements.h"
#include <string.h>
#include <stdarg.h>

// Variable Types
const char *VT_keywords[VT_NITEMS] = { "char", "int", "double", "atom*", "pattern*", "model*", "bond*", "angle*", "torsion*", "atomtype_*" };
const char *text_from_VT(VariableType vt)
	{ return VT_keywords[vt]; }

// Constructor
Variable::Variable(VariableType vt)
{
	// Private variables
	name_.set("unnamed");
	type_ = vt;
	constant_ = FALSE;
	// Public variables
	prev = NULL;
	next = NULL;
}

// Set name of variable
void Variable::setName(const char* s)
{
	name_.set(s);
}

// Copy pointer contents of source variable
void Variable::copyPointer(Variable *v)
{
	ptrValue_ = v->ptrValue_;
}

// Sets the content type of the variable
void Variable::setType(VariableType vt)
{
	type_ = vt;
}

// Set the variable to be a constant
void Variable::setConstant()
{
	constant_ = 1;
}

// Returns content type of the variable
VariableType Variable::type()
{
	return type_;
}

// Get name of variable
const char *Variable::name()
{
	return name_.get();
}

// Get value of variable as float
float Variable::asFloat()
{
	return float(asDouble());
}

// Get value of variable as pointer
void *Variable::asPointer()
{
	return ptrValue_;
}

// Print
void Variable::print()
{
	switch (type_)
	{
		case (VT_CHAR):
			printf("Variable '%s', type_ 'char', value '%s'.\n", name_.get(), asCharacter());
			break;
		case (VT_INTEGER):
			printf("Variable '%s', type_ 'int', value '%i'.\n", name_.get(), asInteger());
			break;
		case (VT_DOUBLE):
			printf("Variable '%s', type_ 'double', value '%f'.\n", name_.get(), asDouble());
			break;
		case (VT_ATOM):
		case (VT_MODEL):
		case (VT_PATTERN):
		case (VT_BOND):
		case (VT_ANGLE):
		case (VT_TORSION):
			printf("Variable '%s', type_ '%s', value '%li'.\n", name_.get(), text_from_VT(type_), ptrValue_);
			break;
		case (VT_ATOMTYPE):
			printf("Variable '%s', type_ 'atomtype_', value '%i'.\n", name_.get(), asInteger());
			break;
	}
}

// Set (from char)
void Variable::set(const char *s)
{
	if (type_ == VT_CHAR) charValue_.set(s);
	else if (type_ == VT_INTEGER) intValue_ = atoi(s);
	else if (type_ == VT_DOUBLE) doubleValue_ = atof(s);
	else printf("Variable::set <<<< Can't set variable '%s' which is of type_ '%s' from a character string >>>>\n", name_.get(), text_from_VT(type_));
}

// Set (int)
void Variable::set(int i)
{
	if (type_ == VT_CHAR) charValue_.set(itoa(i));
	else if (type_ == VT_INTEGER) intValue_ = i;
	else if (type_ == VT_DOUBLE) doubleValue_ = i;
	else printf("Variable::set <<<< Can't set variable '%s' which is of type_ '%s' from an integer value >>>>\n", name_.get(), text_from_VT(type_));
}

// Set (double)
void Variable::set(double d)
{
	if (type_ == VT_CHAR) charValue_.set(ftoa(d));
	else if (type_ == VT_INTEGER) intValue_ = int(d);
	else if (type_ == VT_DOUBLE) doubleValue_ = d;
	else printf("Variable::set <<<< Can't set variable '%s' which is of type_ '%s' from a double value >>>>\n", name_.get(), text_from_VT(type_));
}

// Set (atom*)
void Variable::set(Atom *i)
{
	if (type_ != VT_ATOM)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'atom*' >>>>\n",name_.get(), text_from_VT(type_));
		return;
	}
	ptrValue_ = i;
	msg(DM_VERBOSE,"Atom variable '%s' set to '%li' ('%s')\n",name_.get(),i,(i == NULL ? "" : elements.symbol(i)));
}

// Set (pattern)
void Variable::set(Pattern *p)
{
	if (type_ != VT_PATTERN)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'pattern*' >>>>\n",name_.get(), text_from_VT(type_));
		return;
	}
	ptrValue_ = p;
	msg(DM_VERBOSE,"Pattern variable '%s' set to '%li' ('%s')\n",name_.get(),p,(p == NULL ? "" : p->name()));
}

// Set (model)
void Variable::set(Model *m)
{
	if (type_ != VT_MODEL)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'model*' >>>>\n",name_.get(), text_from_VT(type_));
		return;
	}
	ptrValue_ = m;
	msg(DM_VERBOSE,"Model variable '%s' set to '%li' ('%s')\n",name_.get(),m,(m == NULL ? "" : m->name()));
}

// Set (PatternBound)
void Variable::set(PatternBound *pb)
{
	if (type_ < VT_BOND)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'PatternBound*' >>>>\n",name_.get(), text_from_VT(type_));
		return;
	}
	ptrValue_ = pb;
	msg(DM_VERBOSE,"PatBound variable '%s' set to '%li'\n",name_.get(),pb);
}

// Set (ForcefieldAtom)
void Variable::set(ForcefieldAtom *ffa)
{
	if (type_ < VT_ATOMTYPE)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'ForcefieldAtom*' >>>>\n",name_.get(), text_from_VT(type_));
		return;
	}
	ptrValue_ = ffa;
	msg(DM_VERBOSE,"FFAtom variable '%s' set to '%li'\n",name_.get(),ffa);
}

// Get as char
const char *Variable::asCharacter()
{
	switch (type_)
	{
		case (VT_CHAR):
			return charValue_.get();
		case (VT_INTEGER):
			return itoa(intValue_);
		case (VT_DOUBLE):
			return ftoa(doubleValue_);
		default:
			msg(DM_VERBOSE,"Variable::asCharacter <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), text_from_VT(type_));
	}
	return "";
}

// Get as int
int Variable::asInteger()
{
	switch (type_)
	{
		case (VT_CHAR):
			return atoi(charValue_.get());
		case (VT_INTEGER):
			return intValue_;
		case (VT_DOUBLE):
			return int(doubleValue_);
		default:
			msg(DM_VERBOSE,"Variable::asInteger <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), text_from_VT(type_));
	}
	return 0;
}

// Get as double
double Variable::asDouble()
{
	switch (type_)
	{
		case (VT_CHAR):
			return atof(charValue_.get());
		case (VT_INTEGER):
			return double(intValue_);
		case (VT_DOUBLE):
			return doubleValue_;
		default:
			msg(DM_VERBOSE,"Variable::asDouble <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), text_from_VT(type_));
	}
	return 0.0;
}

// Get as boolean
bool Variable::asBool()
{
	switch (type_)
	{
		case (VT_CHAR):
			return charValue_.asBool();
		case (VT_INTEGER):
			return (intValue_ < 1 ? FALSE : TRUE);
		default:
			msg(DM_VERBOSE,"Variable::get_as_bool <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), text_from_VT(type_));
	}
	return FALSE;
}

// Reset
void Variable::reset()
{
	switch (type_)
	{
		case (VT_CHAR):
			charValue_.set("");
			break;
		case (VT_INTEGER):
			intValue_ = 0;
			break;
		case (VT_DOUBLE):
			doubleValue_ = 0.0;
			break;
		case (VT_ATOM):
		case (VT_PATTERN):
		case (VT_MODEL):
		case (VT_BOND):
		case (VT_ANGLE):
		case (VT_TORSION):
		case (VT_ATOMTYPE):
			ptrValue_ = NULL;
			break;
	}
}

// Integer increase
void Variable::increase(int n)
{
	switch (type_)
	{
		case (VT_INTEGER):
			intValue_ ++;
			break;
		case (VT_DOUBLE):
			doubleValue_ += 1.0;
			break;
		case (VT_ATOM):
			ptrValue_ = ( (Atom*) ptrValue_)->next;
			break;
		case (VT_PATTERN):
			ptrValue_ = ( (Pattern*) ptrValue_)->next;
			break;
		case (VT_MODEL):
			ptrValue_ = ( (Model*) ptrValue_)->next;
			break;
		case (VT_BOND):
		case (VT_ANGLE):
		case (VT_TORSION):
			ptrValue_ = ( (PatternBound*) ptrValue_)->next;
			break;
		case (VT_ATOMTYPE):
			ptrValue_ = ( (ForcefieldAtom*) ptrValue_)->next;
			break;
		default:
			printf("Variable::increase <<<< Don't know how to increase variable '%s', type_ '%s' >>>>\n", name_.get(), text_from_VT(type_));
			break;
	}
}

// Integer Decrease
void Variable::decrease(int n)
{
	switch (type_)
	{
		case (VT_INTEGER):
			intValue_ --;
			break;
		case (VT_DOUBLE):
			doubleValue_ -= 1.0;
			break;
		case (VT_ATOM):
			ptrValue_ = ( (Atom*) ptrValue_)->prev;
			break;
		case (VT_PATTERN):
			ptrValue_ = ( (Pattern*) ptrValue_)->prev;
			break;
		case (VT_MODEL):
			ptrValue_ = ( (Model*) ptrValue_)->prev;
			break;
		case (VT_BOND):
		case (VT_ANGLE):
		case (VT_TORSION):
			ptrValue_ = ( (PatternBound*) ptrValue_)->prev;
			break;
		case (VT_ATOMTYPE):
			ptrValue_ = ( (ForcefieldAtom*) ptrValue_)->prev;
			break;
		default:
			printf("Variable::decrease <<<< Don't know how to decrease variable '%s', type_ '%s' >>>>\n", name_.get(), text_from_VT(type_));
			break;
	}
}

// Retrieve named variable
Variable *VariableList::get(const char *prefix, const char *suffix)
{
	static char name[128];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		strcat(name,".");
		strcat(name,suffix);
	}
	for (Variable *v = vars_.first(); v != NULL; v = v->next)
		if (strcmp(name,v->name()) == 0) return v;
	return NULL;
}

// Add named variable
Variable *VariableList::addVariable(const char *prefix, const char *suffix, VariableType vt)
{
	static char name[128];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		strcat(name,".");
		strcat(name,suffix);
	}
	Variable *result = vars_.add();
	result->setName(name);
	result->setType(vt);
	return result;
}

// Create, don't set, named variable
Variable *VariableList::createVariable(const char *prefix, const char *suffix, VariableType vt)
{
	// First, see if this variable already exists
	Variable *result = get(prefix, suffix);
	if (result == NULL) result = addVariable(prefix, suffix, vt);
	else
	{
		// Check type_ of existing variable
		static char name[128];
		strcpy(name,prefix);
		if (suffix[0] != '\0')
		{
			strcat(name,".");
			strcat(name,suffix);
		}
		if (result->type() != vt)
		{
			printf("Variable '%s' already exists and is of type_ '%s'.\n", name, text_from_VT(vt));
			result = NULL;
		}
	}
	return result;
}

// Add constant
Variable *VariableList::addConstant(const char *s)
{
	static char newname[24];
	// Sink name as static character value
	Variable *result = vars_.add();
	strcpy(newname,"_variable");
	strcat(newname,itoa(vars_.nItems()));
	result->setName(newname);
	// Try to determine type_ of the argument
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
	// Based on the numbers we calculated, try to determine its type_
	if ((nch != 0) || (ndp > 1) || (npm > 2) || (ne > 1) | (nn == 0)) result->setType(VT_CHAR);
	else if (ndp == 1) result->setType(VT_DOUBLE);
	else result->setType(VT_INTEGER);
	//printf("DETERMINED CONSTANT '%s' TO BE OF TYPE '%s'\n",s,text_from_VT(result->get_type_()));
	result->setConstant();
	result->set(s);
	return result;
}

// Set existing variable (or add new and set) (VT_CHAR)
void VariableList::set(const char *prefix, const char *suffix, const char *value)
{
	static char newname[128];
	strcpy(newname,prefix);
	if (suffix[0] != '\0')
	{
		strcat(newname,".");
		strcat(newname,suffix);
	}
	Variable *v = get(newname);
	if (v == NULL) v = addVariable(newname, VT_CHAR);
	v->set(value);
}

// Set existing variable (or add new and set) (VT_INTEGER)
void VariableList::set(const char *prefix, const char *suffix, int value)
{
	static char newname[128];
	strcpy(newname,prefix);
	if (suffix[0] != '\0')
	{
		strcat(newname,".");
		strcat(newname,suffix);
	}
	Variable *v = get(newname);
	if (v == NULL) v = addVariable(newname, VT_INTEGER);
	v->set(value);
}

// Set existing variable (or add new and set) (VT_DOUBLE)
void VariableList::set(const char *prefix, const char *suffix, double value)
{
	static char newname[128];
	strcpy(newname,prefix);
	if (suffix[0] != '\0')
	{
		strcat(newname,".");
		strcat(newname,suffix);
	}
	Variable *v = get(newname);
	if (v == NULL) v = addVariable(newname, VT_DOUBLE);
	v->set(value);
}

// Print list of variables in list
void VariableList::print()
{
	for (Variable *v = vars_.first(); v != NULL; v = v->next)
		printf("VAR=[%s] (%li) VALUE=[%s]\n",v->name(),v,v->asCharacter());
}

// Clear all variable values
void VariableList::resetAll()
{
	dbgBegin(DM_CALLS,"VariableList::resetAll");
	for (Variable *v = vars_.first(); v != NULL; v = v->next) v->reset();
	dbgEnd(DM_CALLS,"VariableList::resetAll");
}

// Clear list of variables
void VariableList::reset(const char *s, ...)
{
	dbgBegin(DM_CALLS,"VariableList::reset");
	// List of variables must be ended by "".
	static char name[64];
	va_list namelist;
	va_start(namelist,s);
	Variable *v;
	// Reset 's' first
	get(s)->reset();
	do
	{
		strcpy(name,va_arg(namelist,char*));
		if (name[0] != '\0')
		{
			v = get(name);
			if (v == NULL) printf("VariableList::reset <<<< '%s' not in list >>>>\n",name);
			else v->reset();
		}
	} while (name[0] != '\0');
	dbgEnd(DM_CALLS,"VariableList::reset");
}
