/*
	*** Variable
	*** src/parse/variable.cpp
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

#include "parse/variable.h"
#include "parse/expression.h"
#include "classes/atom.h"
#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "model/model.h"
#include "base/elements.h"
#include <string.h>
#include <stdarg.h>

// Variable Types
const char *VariableTypeKeywords[Variable::nVariableTypes] = { "char", "int", "double", "atom*", "pattern*", "model*", "bond*", "angle*", "torsion*", "atomtype*", "expression" };
const char *Variable::variableType(Variable::VariableType vt)
{
	return VariableTypeKeywords[vt];
}
Variable::VariableType Variable::determineType(const char *s)
{
	// Try to determine type_ of the argument
	int ch, nn = 0, nch = 0, ndp = 0, npm = 0, ne = 0;
	unsigned int i;
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

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
Variable::~Variable()
{
	// Free expression object if one was created
	if ((type_ == Variable::ExpressionVariable) && (ptrValue_ != NULL)) delete (Expression*) ptrValue_;
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
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type 'atom*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = i;
	msg.print(Messenger::Verbose,"Atom variable '%s' has pointer '%li' ('%s')\n",name_.get(),i,(i == NULL ? "" : elements.symbol(i)));
}

// Set (bond*)
void Variable::set(Bond *b)
{
	if (type_ != Variable::BondVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type 'bond*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = b;
	msg.print(Messenger::Verbose,"Bond variable '%s' has pointer '%li')\n", name_.get(), b);
}

// Set (pattern)
void Variable::set(Pattern *p)
{
	if (type_ != Variable::PatternVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type 'pattern*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = p;
	msg.print(Messenger::Verbose,"Pattern variable '%s' has pointer '%li' ('%s')\n",name_.get(),p,(p == NULL ? "" : p->name()));
}

// Set (model)
void Variable::set(Model *m)
{
	if (type_ != Variable::ModelVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type 'model*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = m;
	msg.print(Messenger::Verbose,"Model variable '%s' has pointer '%li' ('%s')\n",name_.get(),m,(m == NULL ? "" : m->name()));
}

// Set (PatternBound)
void Variable::set(PatternBound *pb)
{
	if (type_ < Variable::BondVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type 'PatternBound*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = pb;
	msg.print(Messenger::Verbose,"PatBound variable '%s' has pointer '%li'\n",name_.get(),pb);
}

// Set (ForcefieldAtom)
void Variable::set(ForcefieldAtom *ffa)
{
	if (type_ < Variable::AtomtypeVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type 'ForcefieldAtom*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = ffa;
	msg.print(Messenger::Verbose,"FFAtom variable '%s' has pointer '%li'\n",name_.get(),ffa);
}

// Set (Expression)
void Variable::set(Expression *ex)
{
	if (type_ != Variable::ExpressionVariable)
	{
		printf("Variable::set <<<< Tried to set variable '%s' which is of type_ '%s' as if it were of type 'ForcefieldAtom*' >>>>\n",name_.get(), Variable::variableType(type_));
		return;
	}
	ptrValue_ = ex;
	msg.print(Messenger::Verbose,"Expression variable '%s' has pointer '%li'\n",name_.get(),ex);
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
			msg.print(Messenger::Verbose,"Variable::asCharacter <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
	}
	return "";
}

// Get as int
int Variable::asInteger()
{
	Expression *ex;
	switch (type_)
	{
		case (Variable::CharacterVariable):
			return atoi(charValue_.get());
		case (Variable::IntegerVariable):
			return intValue_;
		case (Variable::FloatVariable):
			return int(doubleValue_);
		case (Variable::ExpressionVariable):
			ex = (Expression*) ptrValue_;
			return int (ex->evaluateAsInteger());
		default:
			msg.print(Messenger::Verbose,"Variable::asInteger <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
	}
	return 0;
}

// Get as double
double Variable::asDouble()
{
	Expression *ex;
	switch (type_)
	{
		case (Variable::CharacterVariable):
			return atof(charValue_.get());
		case (Variable::IntegerVariable):
			return double(intValue_);
		case (Variable::FloatVariable):
			return doubleValue_;
		case (Variable::ExpressionVariable):
			ex = (Expression*) ptrValue_;
			return ex->evaluateAsDouble();
		default:
			msg.print(Messenger::Verbose,"Variable::asDouble <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
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
			msg.print(Messenger::Verbose,"Variable::asBool <<<< Tried to get variable '%s' which is of type_ '%s' >>>>\n", name_.get(), Variable::variableType(type_));
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
