/*
	*** Arithmetic Expression node
	*** src/parse/expressionnode.cpp
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

#include "base/constants.h"
#include "base/messenger.h"
#include "parse/expression.h"
#include "parse/variablelist.h"
#include <math.h>
#include <string.h>

// Operator Tokens
char OperatorTypeKeywords[ExpressionNode::nOperatorTypes] = { '%', '^', '*', '/', '+', '-' };
char ExpressionNode::operatorType(ExpressionNode::OperatorType ot)
{
	return OperatorTypeKeywords[ot];
}
ExpressionNode::OperatorType ExpressionNode::operatorType(char c)
{
	int result;
	for (result=0; result<ExpressionNode::nOperatorTypes; result++) if (c == OperatorTypeKeywords[result]) break;
	return (ExpressionNode::OperatorType) result;
}

// Function Tokens
const char *FunctionTypeKeywords[ExpressionNode::nFunctionTypes] = { "-", "sqrt", "cos", "sin", "tan", "abs" };
const char *ExpressionNode::functionType(ExpressionNode::FunctionType ft)
{
	return FunctionTypeKeywords[ft];
}
ExpressionNode::FunctionType ExpressionNode::functionType(const char *s)
{
	return (ExpressionNode::FunctionType) enumSearch("function type", ExpressionNode::nFunctionTypes, FunctionTypeKeywords, s);
}

// Constructor
ExpressionNode::ExpressionNode()
{
	// Private Variables
	type_ = ExpressionNode::nTokenTypes;
	persistentType_ = ExpressionNode::nTokenTypes;
	operator_ = ExpressionNode::nOperatorTypes;
	function_ = ExpressionNode::nFunctionTypes;
	bracket_ = ExpressionNode::nBracketTypes;
	used_ = FALSE;
	doubleValue_ = 0.0;
	intValue_ = 0;
	floatType_ = TRUE;
	reducedDoubleValue_ = 0.0;
	reducedIntValue_ = 0;
	variable_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Reset expression node
void ExpressionNode::reset()
{
	used_ = FALSE;
	reducedDoubleValue_ = 0.0;
	reducedIntValue_ = 0;
	type_ = persistentType_;
	floatType_ = persistentFloat_;
}

// Set general type of node
void ExpressionNode::setType(ExpressionNode::TokenType tt)
{
	type_ = tt;
}

// Set operator type
void ExpressionNode::setOperatorType(ExpressionNode::OperatorType ot)
{
	operator_ = ot;
}

// Set function type
void ExpressionNode::setFunctionType(ExpressionNode::FunctionType ft)
{
	function_ = ft;
}

// Set bracket type
void ExpressionNode::setBracketType(ExpressionNode::BracketType bt)
{
	bracket_ = bt;
}

// Return general node type
ExpressionNode::TokenType ExpressionNode::type()
{
	return type_;
}

// Return operator type
ExpressionNode::OperatorType ExpressionNode::operatorType()
{
	return operator_;
}

// Return function type
ExpressionNode::FunctionType ExpressionNode::functionType()
{
	return function_;
}

// Return bracket type
ExpressionNode::BracketType ExpressionNode::bracketType()
{
	return bracket_;
}

// Set original type
void ExpressionNode::setPersistentType(ExpressionNode::TokenType tt, bool isfloat)
{
	type_ = tt;
	persistentType_ = tt;
	persistentFloat_ = isfloat;
}

// Change the node into a double value node
void ExpressionNode::makeValue(double value)
{
	reducedDoubleValue_ = value;
	type_ = ExpressionNode::ValueToken;
	floatType_ = TRUE;
}

// Change the node into an integer value node
void ExpressionNode::makeValue(int value)
{
	reducedIntValue_ = value;
	type_ = ExpressionNode::ValueToken;
	floatType_ = FALSE;
}

// Flag the node as used
void ExpressionNode::setUsed()
{
	used_ = TRUE;
}

// Return whether the node has been used already in the current evaluation
bool ExpressionNode::used()
{
	return used_;
}

// Return if the node is a floating point number
bool ExpressionNode::isFloat()
{
	return floatType_;
}

// Set value (double)
void ExpressionNode::setValue(double value)
{
	doubleValue_ = value;
}

// Set value (integer)
void ExpressionNode::setValue(int value)
{
	intValue_ = value;
}

// Return numerical value
double ExpressionNode::asDouble()
{
	if (type_ != ExpressionNode::ValueToken)
	{
		printf("Tried to get a value from an expression token that doesn't have one.\n");
		return 0.0;
	}
	if (type_ != persistentType_) return (floatType_ ? reducedDoubleValue_ : (double) reducedIntValue_);
	else if (variable_ == NULL) return (floatType_ ? doubleValue_ : (double) intValue_);
	else return variable_->asDouble();
}

// Return numerical value as integer
int ExpressionNode::asInteger()
{
	if (type_ != ExpressionNode::ValueToken)
	{
		printf("Tried to get a value from an expression token that doesn't have one.\n");
		return 0;
	}
	if (type_ != persistentType_) return (floatType_ ? (int) reducedDoubleValue_ : reducedIntValue_);
	else if (variable_ == NULL) return (floatType_ ? (int) doubleValue_ : intValue_);
	else return variable_->asInteger();
}

// Set variable
void ExpressionNode::setVariable(Variable *var)
{
	variable_ = var;
}

// Return variable
Variable *ExpressionNode::variable()
{
	return variable_;
}

// Return next unused node in this node's list
ExpressionNode *ExpressionNode::nextUnused(ExpressionNode *limit)
{
	ExpressionNode *result = this;
	do
	{
		if (result == limit)
		{
			result = NULL;
			break;
		}
		result = result->next;
	} while (result->used());
	return result;
}
