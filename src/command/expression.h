/*
	*** Arithmetic Expression
	*** src/parse/expression.h
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

#ifndef ATEN_EXPRESSION_H
#define ATEN_EXPRESSION_H

#include "base/constants.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "parse/expressionnode.h"

// Forward Declarations
class VariableList;

// Expression
class Expression
{
	public:
	// Constructor
	Expression();

	private:
	// The actual expression
	List<ExpressionNode> expression_;
	// The VariableList against which the expression was created
	VariableList *vars_;
	// Type of final result, whether double (TRUE) or integer (FALSE)
	bool evaluatesToFloat_;
	// Reflist of all bracketed pairs (to speed up execution)
	Reflist<ExpressionNode,ExpressionNode*> brackets_;
	// Add long (non-operator)
	ExpressionNode::TokenType addLongToken(const char *s);
	// Validate expression
	bool validate();
	// Create expression plan
	bool createPlan();
	// Evaluate subexpression
	void evaluate(ExpressionNode *left, ExpressionNode *right);
	// Evaluate whole expression (reduce to one node)
	ExpressionNode *evaluate();

	public:
	// Set expression from string
	bool set(const char *s, VariableList *vars);
	// Return whether the result of the expression is a floating point (or integer)
	bool evaluatesToFloat();
	// Evaluate expression and return value
	double evaluateAsDouble();
	int evaluateAsInteger();
	// Print expression
	void print(ExpressionNode *highlight = NULL, bool showUsed = TRUE);
};

#endif
