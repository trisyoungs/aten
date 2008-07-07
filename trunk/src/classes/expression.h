/*
	*** Arithmetic Expression
	*** src/classes/expression.h
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

// Forward Declarations
class Variable;
class VariableList;

// Expression Node
class ExpressionNode
{
	public:
	// Constructor
	ExpressionNode();
	// Node types
	enum TokenType { ValueToken, OperatorToken, FunctionToken, BracketToken, nTokenTypes };
	// Operator tokens
	enum OperatorType { ModulusOperator, PowerOperator, MultiplyOperator, DivideOperator, AddOperator, SubtractOperator, nOperatorTypes };
	static char operatorType(ExpressionNode::OperatorType ot);
	static ExpressionNode::OperatorType operatorType(char s);
	// Function tokens
	enum FunctionType { NegateFunction, SqrtFunction, CosFunction, SinFunction, TanFunction, nFunctionTypes };
	static const char *functionType(ExpressionNode::FunctionType ft);
	static ExpressionNode::FunctionType functionType(const char *s);
	// Bracket tokens
	enum BracketType { LeftBracket, RightBracket, nBracketTypes };

	// List pointers
	ExpressionNode *prev, *next;
	
	private:
	// Current type of node
	ExpressionNode::TokenType type_;
	// Original (persistent) type of node
	ExpressionNode::TokenType persistentType_;
	// Sub-type of node
	OperatorType operator_;
	FunctionType function_;
	BracketType bracket_;
	// Whether this node has already been used up in the current evaluation
	bool used_;
	// Numeric value (if variable_ == NULL)
	double value_;
	// Variable pointer
	Variable *variable_;
	// 'Reduced' value of node (when used_ = TRUE)
	double reducedValue_;

	public:
	// Reset node
	void reset();
	// Set type of node
	void setType(ExpressionNode::TokenType);
	void setOperatorType(ExpressionNode::OperatorType);
	void setFunctionType(ExpressionNode::FunctionType);
	void setBracketType(ExpressionNode::BracketType);
	// Return node type
	ExpressionNode::TokenType type();
	ExpressionNode::OperatorType operatorType();
	ExpressionNode::FunctionType functionType();
	ExpressionNode::BracketType bracketType();
	// Set persistent type
	void setPersistentType(ExpressionNode::TokenType);
	// Change the node to a plain value node
	void makeValue(double value);
	// Flag the node as used
	void setUsed();
	// Return whether the node has been used already in the current evaluation
	bool used();
	// Set value
	void setValue(double value);
	// Set variable
	void setVariable(Variable *var);
	// Return variable
	Variable *variable();
	// Return numerical value (may be value_ variable_->asDouble(), or reducedValue_.
	double value();
	// Return next unused node in this node's list
	ExpressionNode *nextUnused(ExpressionNode *limit = NULL);
};

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
	// Reflist of all bracketed pairs (to speed up execution)
	Reflist<ExpressionNode,ExpressionNode*> brackets_;
	// Add long operator
	ExpressionNode::TokenType addLongOperator(const char *s);
	// Validate expression
	bool validate();
	// Create expression plan
	bool createPlan();
	// Evaluate subexpression
	void evaluate(ExpressionNode *left, ExpressionNode *right);

	public:
	// Set expression from string
	bool set(const char *s, VariableList *vars);
	// Evaluate expression and return value
	double evaluate();
	// Print expression
	void print(ExpressionNode *highlight = NULL, bool showUsed = TRUE);
};

#endif
