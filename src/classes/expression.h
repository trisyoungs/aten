/*
	*** Cached Arithmetic Expression
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

#include "templates/list.h"

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
	enum OperatorType { MinusOperator, PlusOperator, DivideOperator, MultiplyOperator, PowerOperator, ModulusOperator, nOperatorTypes };
	static char operatorType(ExpressionNode::OperatorType ot);
	static ExpressionNode::OperatorType operatorType(char s);
	// Function tokens
	enum FunctionType { SqrtFunction, CosFunction, SinFunction, nFunctionTypes };
	static const char *functionType(ExpressionNode::FunctionType ft);
	static ExpressionNode::FunctionType functionType(const char *s);
	// Bracket tokens
	enum BracketType { LeftBracket, RightBracket, nBracketTypes };

	// List pointers
	ExpressionNode *prev, *next;
	
	private:
	// Type of node
	ExpressionNode::TokenType type_;
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
	// Set whether the node has been used already
	void setUsed(bool used);
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
	// Add long operator
	bool addLongOperator(const char *s);
	// Validate expression
	bool validate();
	// Create expression plan
	void createPlan();
	// Evaluate subexpression
	double evaluate(ExpressionNode *left, ExpressionNode *right);

	public:
	// Set expression from string
	bool set(const char *s, VariableList *vars);
	// Evaluate expression and return value
	double evaluate();
	// Print expression
	void print();
};

#endif
