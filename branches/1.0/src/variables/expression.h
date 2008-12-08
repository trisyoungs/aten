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

#include "variables/expressionnode.h"
#include "variables/variable.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "templates/vobject.h"

// Forward Declarations
class VariableList;

// Expression Variable
class ExpressionVariable : public Variable, VObject<int>
{
	public:
	// Constructor / Destructor
	ExpressionVariable();
	~ExpressionVariable();

	/*
	// Set / Get
	*/
	public:
	// Clears value of variable
	//void reset();
	// Set value of variable (char)
	bool set(const char*);
	// Set value of variable (int)
	bool set(int i);
	// Set value of variable (double)
	bool set(double d);
	// Set value of variable (pointer)
	bool set(void *ptr, VTypes::DataType type);
	// Get value of variable as integer
	int asInteger(Variable *index = NULL);
	// Get value of variable as double
	double asDouble(Variable *index = NULL);

	/*
	// Expression Data
	*/
	private:
	// The actual expression
	List<ExpressionNode> expression_;
	// Type of final result, whether real (TRUE) or integer (FALSE)
	bool evaluatesToReal_;
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
	// Evaluate the return type of the expression
	void determineType();
	// Evaluate type of expression part
	bool isReal(ExpressionNode *left, ExpressionNode *right);

	public:
	// Set expression from string
	bool initialise(const char *s);
	// Return whether the result of the expression is a floating point (or integer)
	bool evaluatesToReal();
	// Print expression
	void print(ExpressionNode *highlight = NULL, bool showUsed = TRUE);
};

#endif
