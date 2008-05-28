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
const char *VariableTypeKeywords[Variable::nVariableTypes] = { "char", "int", "double", "atom*", "pattern*", "model*", "bond*", "angle*", "torsion*", "atomtype_*" };
const char *Variable::variableType(Variable::VariableType vt)
{
	return VariableTypeKeywords[vt];
}
Variable::VariableType Variable::determineType(const char *s)
{
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
	if ((nch != 0) || (ndp > 1) || (npm > 2) || (ne > 1) | (nn == 0)) return Variable::CharacterVariable;
	else if (ndp == 1) return Variable::FloatVariable;
	else return Variable::IntegerVariable;
}

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
Variable::VariableType Variable::type()
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
		case (Variable::CharacterVariable):
			printf("Variable '%s', type_ 'char', value '%s'.\n", name_.get(), asCharacter());
			break;
		case (Variable::IntegerVariable):
			printf("Variable '%s', type_ 'int', value '%i'.\n", name_.get(), asInteger());
			break;
		case (Variable::FloatVariable):
			printf("Variable '%s', type_ 'double', value '%f'.\n", name_.get(), asDouble());
			break;
		case (Variable::AtomVariable):
		case (Variable::ModelVariable):
		case (Variable::PatternVariable):
		case (Variable::BondVariable):
		case (Variable::AngleVariable):
		case (Variable::TorsionVariable):
			printf("Variable '%s', type_ '%s', value '%li'.\n", name_.get(), Variable::variableType(type_), ptrValue_);
			break;
		case (Variable::AtomtypeVariable):
			printf("Variable '%s', type_ 'atomtype_', value '%i'.\n", name_.get(), asInteger());
			break;
	}
}

// Set (from char)
void Variable::set(const char *s)
{
	if (type_ == Variable::CharacterVariable) charValue_.set(s);
	else if (type_ == Variable::IntegerVariable) intValue_ = atoi(s);
	else if (type_ == Variable::FloatVariable) doubleValue_ = atof(s);
	else printf("Variable::set <<<< Can't set variable '%s' which is of type_ '%s' from a character string >>>>\n", name_.get(), Variable::variableType(type_));
}

// Set (int)
void Variable::set(int i)
{
	if (type_ == Variable::CharacterVariable) charValue_.set(itoa(i));
	else if (type_ == Variable::IntegerVariable) intValue_ = i;
	else if (type_ == Variable::FloatVariable) doubleValue_ = i;
	else printf("Variable::set <<<< Can't set variable '%s' which is of type_ '%s' from an integer value >>>>\n", name_.get(), Variable::variableType(type_));
}

// Set (float (double))
void Variable::set(double d)
{
	if (type_ == Variable::CharacterVariable) charValue_.set(ftoa(d));
	else if (type_ == Variable::IntegerVariable) intValue_ = int(d);
	else if (type_ == Variable::FloatVariable) doubleValue_ = d;
	else printf("Variable::set <<<< Can't set variable '%s' which is of type_ '%s' from a double value >>>>\n", name_.get(), Variable::variableType(type_));
}

// Set (atom*)
void Variable::set(Atom *i)
{
	if (type_ != Variable::AtomVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'atom*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = i;
	msg(Debug::Verbose,"Atom variable '%s' set to '%li' ('%s')\n",name_.get(),i,(i == NULL ? "" : elements.symbol(i)));
}

// Set (pattern)
void Variable::set(Pattern *p)
{
	if (type_ != Variable::PatternVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'pattern*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = p;
	msg(Debug::Verbose,"Pattern variable '%s' set to '%li' ('%s')\n",name_.get(),p,(p == NULL ? "" : p->name()));
}

// Set (model)
void Variable::set(Model *m)
{
	if (type_ != Variable::ModelVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'model*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = m;
	msg(Debug::Verbose,"Model variable '%s' set to '%li' ('%s')\n",name_.get(),m,(m == NULL ? "" : m->name()));
}

// Set (PatternBound)
void Variable::set(PatternBound *pb)
{
	if (type_ < Variable::BondVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'PatternBound*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = pb;
	msg(Debug::Verbose,"PatBound variable '%s' set to '%li'\n",name_.get(),pb);
}

// Set (ForcefieldAtom)
void Variable::set(ForcefieldAtom *ffa)
{
	if (type_ < Variable::AtomtypeVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type_ 'ForcefieldAtom*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = ffa;
	msg(Debug::Verbose,"FFAtom variable '%s' set to '%li'\n",name_.get(),ffa);
}

// Get as char
const char *Variable::asCharacter()
{
	switch (type_)
	{
		case (Variable::CharacterVariable):
			return charValue_.get();
		case (Variable::IntegerVariable):
			return itoa(intValue_);
		case (Variable::FloatVariable):
			return ftoa(doubleValue_);
		default:
			msg(Debug::Verbose,"Variable::asCharacter <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
	}
	return "";
}

// Get as int
int Variable::asInteger()
{
	switch (type_)
	{
		case (Variable::CharacterVariable):
			return atoi(charValue_.get());
		case (Variable::IntegerVariable):
			return intValue_;
		case (Variable::FloatVariable):
			return int(doubleValue_);
		default:
			msg(Debug::Verbose,"Variable::asInteger <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
	}
	return 0;
}

// Get as double
double Variable::asDouble()
{
	switch (type_)
	{
		case (Variable::CharacterVariable):
			return atof(charValue_.get());
		case (Variable::IntegerVariable):
			return double(intValue_);
		case (Variable::FloatVariable):
			return doubleValue_;
		default:
			msg(Debug::Verbose,"Variable::asDouble <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
	}
	return 0.0;
}

// Get as boolean
bool Variable::asBool()
{
	switch (type_)
	{
		case (Variable::CharacterVariable):
			return charValue_.asBool();
		case (Variable::IntegerVariable):
			return (intValue_ < 1 ? FALSE : TRUE);
		default:
			msg(Debug::Verbose,"Variable::get_as_bool <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
	}
	return FALSE;
}

// Reset
void Variable::reset()
{
	switch (type_)
	{
		case (Variable::CharacterVariable):
			charValue_.set("");
			break;
		case (Variable::IntegerVariable):
			intValue_ = 0;
			break;
		case (Variable::FloatVariable):
			doubleValue_ = 0.0;
			break;
		case (Variable::AtomVariable):
		case (Variable::PatternVariable):
		case (Variable::ModelVariable):
		case (Variable::BondVariable):
		case (Variable::AngleVariable):
		case (Variable::TorsionVariable):
		case (Variable::AtomtypeVariable):
			ptrValue_ = NULL;
			break;
	}
}

// Integer increase
void Variable::increase(int n)
{
	switch (type_)
	{
		case (Variable::IntegerVariable):
			intValue_ ++;
			break;
		case (Variable::FloatVariable):
			doubleValue_ += 1.0;
			break;
		case (Variable::AtomVariable):
			ptrValue_ = ( (Atom*) ptrValue_)->next;
			break;
		case (Variable::PatternVariable):
			ptrValue_ = ( (Pattern*) ptrValue_)->next;
			break;
		case (Variable::ModelVariable):
			ptrValue_ = ( (Model*) ptrValue_)->next;
			break;
		case (Variable::BondVariable):
		case (Variable::AngleVariable):
		case (Variable::TorsionVariable):
			ptrValue_ = ( (PatternBound*) ptrValue_)->next;
			break;
		case (Variable::AtomtypeVariable):
			ptrValue_ = ( (ForcefieldAtom*) ptrValue_)->next;
			break;
		default:
			printf("Variable::increase <<<< Don't know how to increase variable '%s', type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
			break;
	}
}

// Integer Decrease
void Variable::decrease(int n)
{
	switch (type_)
	{
		case (Variable::IntegerVariable):
			intValue_ --;
			break;
		case (Variable::FloatVariable):
			doubleValue_ -= 1.0;
			break;
		case (Variable::AtomVariable):
			ptrValue_ = ( (Atom*) ptrValue_)->prev;
			break;
		case (Variable::PatternVariable):
			ptrValue_ = ( (Pattern*) ptrValue_)->prev;
			break;
		case (Variable::ModelVariable):
			ptrValue_ = ( (Model*) ptrValue_)->prev;
			break;
		case (Variable::BondVariable):
		case (Variable::AngleVariable):
		case (Variable::TorsionVariable):
			ptrValue_ = ( (PatternBound*) ptrValue_)->prev;
			break;
		case (Variable::AtomtypeVariable):
			ptrValue_ = ( (ForcefieldAtom*) ptrValue_)->prev;
			break;
		default:
			printf("Variable::decrease <<<< Don't know how to decrease variable '%s', type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
			break;
	}
}

/*
// Variable List
*/

// Return dummy variable
Variable *VariableList::dummy()
{
	return &dummy_;
}

// Retrieve named variable
Variable *VariableList::get(const char *name)
{
	return get(name,"");
}
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
Variable *VariableList::addVariable(const char *name, Variable::VariableType vt)
{
	return addVariable(name,"",vt);
}
Variable *VariableList::addVariable(const char *prefix, const char *suffix, Variable::VariableType vt)
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
Variable *VariableList::createVariable(const char *prefix, const char *suffix, Variable::VariableType vt)
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
			printf("Variable '%s' already exists and is of type_ '%s'.\n", name, Variable::variableType(vt));
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
	result->setType(Variable::determineType(s));
	result->setConstant();
	result->set(s);
	return result;
}

// Set existing variable (or add new and set) (Variable::CharacterVariable)
void VariableList::set(const char *name, const char *value)
{
	set(name,"",value);
}
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
	if (v == NULL) v = addVariable(newname, Variable::CharacterVariable);
	v->set(value);
}

// Set existing variable (or add new and set) (Variable::IntegerVariable)
void VariableList::set(const char *name, int value)
{
	set(name,"",value);
}

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
	if (v == NULL) v = addVariable(newname, Variable::IntegerVariable);
	v->set(value);
}

// Set existing variable (or add new and set) (Variable::FloatVariable)
void VariableList::set(const char *name, double value)
{
	set(name,"",value);
}
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
	if (v == NULL) v = addVariable(newname, Variable::FloatVariable);
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
	dbgBegin(Debug::Calls,"VariableList::resetAll");
	for (Variable *v = vars_.first(); v != NULL; v = v->next) v->reset();
	dbgEnd(Debug::Calls,"VariableList::resetAll");
}

// Clear list of variables
void VariableList::reset(const char *s, ...)
{
	dbgBegin(Debug::Calls,"VariableList::reset");
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
	dbgEnd(Debug::Calls,"VariableList::reset");
}
