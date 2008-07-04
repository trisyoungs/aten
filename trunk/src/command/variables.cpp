/*
	*** Variable command functions
	*** src/command/variables.cpp
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

#include "command/commandlist.h"

// Decrease variable by 1
int CommandData::function_CA_DECREASE(Command *&c, Bundle &obj)
{
	c->arg(0)->decrease(1);
	return CR_SUCCESS;
}

// Increase variable
int CommandData::function_CA_INCREASE(Command *&c, Bundle &obj)
{
	c->arg(0)->increase(1);
	return CR_SUCCESS;
}

// Set non-pointer or non-character variable to value, variable, or expression
int CommandData::function_CA_LET(Command *&c, Bundle &obj)
{
	// Our action depends on the type of the variable being assigned to
	Variable::VariableType type1 = c->argt(0);
	Variable::VariableType type2 = c->argt(2);
	// Integer and real variables may only be set from character, integer, real, or expression variables
	switch (type1)
	{
		case (Variable::IntegerVariable):
			if ((type2 > Variable::FloatVariable) && (type2 < Variable::ExpressionVariable))
			{
				msg.print( "Cannot set integer variable '%s' from pointer variable '%s'.\n", c->arg(0)->name(), c->arg(2)->name());
				return CR_FAIL;
			}
			break;
		case (Variable::FloatVariable):
			if ((type2 > Variable::FloatVariable) && (type2 < Variable::ExpressionVariable))
			{
				msg.print( "Cannot set real variable '%s' from pointer variable '%s'.\n", c->arg(0)->name(), c->arg(2)->name());
				return CR_FAIL;
			}
			break;
		// All other types are pointers - the second argument must also then be a pointer
		default:
			printf("CA_LET doesn't know how to handle variable assignments of type '%s'\n", Variable::variableType(c->argt(0)));
			return CR_FAIL;
			break;
	}
	// Perform assignment operation requested
	switch (c->argi(1))
	{
		case (AssignOp::Equals):
			c->arg(0)->set( type1 == Variable::IntegerVariable ? c->arg(2)->asInteger() : c->arg(2)->asDouble() );
			break;
		case (AssignOp::MinusEquals):
			c->arg(0)->set( type1 == Variable::IntegerVariable ? c->arg(0)->asInteger() - c->arg(2)->asInteger() : c->arg(0)->asDouble() - c->arg(2)->asDouble() );
			break;
		case (AssignOp::PlusEquals):
			if (type1 == Variable::IntegerVariable) c->arg(0)->set( c->arg(0)->asInteger() + c->arg(2)->asInteger() );
			else c->arg(0)->set( c->arg(0)->asDouble() + c->arg(2)->asDouble() );
			break;
		case (AssignOp::DivideEquals):
			c->arg(0)->set( type1 == Variable::IntegerVariable ? c->arg(0)->asInteger() / c->arg(2)->asInteger() : c->arg(0)->asDouble() / c->arg(2)->asDouble() );
			break;
		case (AssignOp::MultiplyEquals):
			c->arg(0)->set( type1 == Variable::IntegerVariable ? c->arg(0)->asInteger() * c->arg(2)->asInteger() : c->arg(0)->asDouble() * c->arg(2)->asDouble() );
			break;
	}
	return CR_SUCCESS;
}

// Assign string/variable to character variable only
int CommandData::function_CA_LETCHAR(Command *&c, Bundle &obj)
{
	// Our action depends on the operator provided which we cast from the second argument
	switch (c->argi(1))
	{
		// Straight assigment
		case (AssignOp::Equals):
			c->arg(0)->set(c->argc(2));
			break;
		// Concatenation
		case (AssignOp::PlusEquals):
			c->arg(0)->set(c->argc(2));
			break;
		default:
			printf("Operator given to CA_LETCHAR (%i) that we don't know how to handle.\n", c->argi(1));
			break;
	}
	return CR_SUCCESS;
}

// Assign pointer variable to another pointer variable
int CommandData::function_CA_LETPTR(Command *&c, Bundle &obj)
{
	if (c->argt(0) != c->argt(2))
	{
		msg.print( "Incompatible pointer types for variable assignment of contents of '%s' to '%s'.\n", c->arg(0)->name(), c->arg(2)->name());
		return CR_FAIL;
	}
	else c->arg(0)->copyPointer(c->arg(2));
	return CR_SUCCESS;
}
