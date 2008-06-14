/*
	*** Arithmetic Expression
	*** src/classes/expression.cpp
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

#include "classes/expression.h"
#include "base/constants.h"
#include "base/debug.h"
#include "classes/variablelist.h"
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
const char *FunctionTypeKeywords[ExpressionNode::nFunctionTypes] = { "-", "sqrt", "cos", "sin" };
const char *ExpressionNode::functionType(ExpressionNode::FunctionType ft)
{
	return FunctionTypeKeywords[ft];
}
ExpressionNode::FunctionType ExpressionNode::functionType(const char *s)
{
	return (ExpressionNode::FunctionType) enumSearch("function type", ExpressionNode::nFunctionTypes, FunctionTypeKeywords, s);
}

// Constructors
ExpressionNode::ExpressionNode()
{
	// Private Variables
	type_ = ExpressionNode::nTokenTypes;
	persistentType_ = ExpressionNode::nTokenTypes;
	operator_ = ExpressionNode::nOperatorTypes;
	function_ = ExpressionNode::nFunctionTypes;
	bracket_ = ExpressionNode::nBracketTypes;
	used_ = FALSE;
	value_ = 0.0;
	reducedValue_ = 0.0;
	variable_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

Expression::Expression()
{
	// Private variables
	vars_ = NULL;
}

// Reset expression node
void ExpressionNode::reset()
{
	used_ = FALSE;
	reducedValue_ = 0.0;
	type_ = persistentType_;
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
void ExpressionNode::setPersistentType(ExpressionNode::TokenType tt)
{
	type_ = tt;
	persistentType_ = tt;
}

// Change the node into a value node
void ExpressionNode::makeValue(double value)
{
	reducedValue_ = value;
	type_ = ExpressionNode::ValueToken;
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

// Set value
void ExpressionNode::setValue(double value)
{
	value_ = value;
}

// Return numerical value
double ExpressionNode::value()
{
	if (type_ != ExpressionNode::ValueToken)
	{
		printf("Tried to get a value from an expression token that doesn't have one.\n");
		return 0.0;
	}
	if (type_ != persistentType_) return reducedValue_;
	else if (variable_ == NULL) return value_;
	else return variable_->asDouble();
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

// Validate expression
bool Expression::validate()
{
	dbgBegin(Debug::Calls,"Expression::validate");
	ExpressionNode::TokenType ltoken, rtoken;
	int count = 0;
	// Here we check the grammar of the expression.
	for (ExpressionNode *ex = expression_.first(); ex != NULL; ex = ex->next)
	{
		count ++;
		// Grab adjacent nodes
		ltoken = (ex->prev == NULL ? ExpressionNode::nTokenTypes : ex->prev->type());
		rtoken = (ex->next == NULL ? ExpressionNode::nTokenTypes : ex->next->type());

		// 1. Adjacency to end of expression check
		switch (ex->type())
		{
			// Operators can never appear at the beginning or end of the expression
			case (ExpressionNode::OperatorToken):
				if ((ltoken == ExpressionNode::nTokenTypes) || (rtoken == ExpressionNode::nTokenTypes))
				{
					msg(Debug::None, "A token must appear either side of this operator:\n");
					print(ex);
					return FALSE;
				}
			// Function tokens can appear at the start, but not the end
			case (ExpressionNode::FunctionToken):
				if ((ltoken == ExpressionNode::nTokenTypes) || (rtoken == ExpressionNode::nTokenTypes))
				{
					msg(Debug::None, "A token must appear to the right of this function:\n");
					print(ex);
					return FALSE;
				}
		}

		// 2. Bracket check - disallow values, operators, and functions immediately following or preceeding certain brackets, and reject empty pairs of brackets
		switch (ex->type())
		{
			// Operators cannot follow left-brackets or preceed right-brackets
			case (ExpressionNode::OperatorToken):
				if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::LeftBracket))
				{
					msg(Debug::None, "Operator immediately follows an opening parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::RightBracket))
				{
					msg(Debug::None, "Operator immediately preceeds a closing parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Values cannot follow right-brackets or proceed left-brackets
			case (ExpressionNode::ValueToken):
				if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::RightBracket))
				{
					msg(Debug::None, "Value immediately follows a closing parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::LeftBracket))
				{
					msg(Debug::None, "Value immediately preceeds an opening parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Functions cannot follow right-brackets
			case (ExpressionNode::FunctionToken):
				if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::RightBracket))
				{
					msg(Debug::None, "Function immediately follows a closing parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Opposing brackets may not immediately follow each other
			case (ExpressionNode::BracketToken):
				if (ex->bracketType() == ExpressionNode::LeftBracket)
				{
					if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::RightBracket))
					{
						msg(Debug::None, "Opposing parenthesis may not be adjacent in expressions:\n");
						print(ex);
						return FALSE;
					}
					else if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::RightBracket))
					{
						msg(Debug::None, "Empty parentheses in expressions are not allowed:\n");
						print(ex);
						return FALSE;
					}
				}
				else
				{
					if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::LeftBracket))
					{
						msg(Debug::None, "Empty parentheses in expressions are not allowed:\n");
						print(ex);
						return FALSE;
					}
					else if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::LeftBracket))
					{
						msg(Debug::None, "Opposing parenthesis may not be adjacent in expressions:\n");
						print(ex);
						return FALSE;
					}
				}
				break;
		}

		// 2. Double-token check - disallow two adjacent values, operators, or functions 
		if (ex->type() != ExpressionNode::BracketToken)
		{
			if ((ltoken == ex->type()) || (rtoken == ex->type()))
			{
				msg(Debug::None, "Adjacent values, operators, or functions in expression:\n");
				print(ex);
				return FALSE;
			}
		}

		// 4. Placement of other adjacent operators 
		switch (ex->type())
		{
			// Operators must have values (or bracketed parts) either side (or functions to the right
			case (ExpressionNode::OperatorToken):
				if ((ltoken != ExpressionNode::ValueToken) && (ltoken != ExpressionNode::BracketToken))
				{
					msg(Debug::None, "Invalid token to left of operator in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::OperatorToken) || (rtoken == ExpressionNode::nTokenTypes))
				{
					msg(Debug::None, "Invalid token to right of operator in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Functions must have operators or brackets to the left, and values or brackets to the right
			case (ExpressionNode::FunctionToken):
				if ((ltoken != ExpressionNode::OperatorToken) && (ltoken != ExpressionNode::BracketToken))
				{
					msg(Debug::None, "Invalid token to left of function in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken != ExpressionNode::ValueToken) && (rtoken != ExpressionNode::BracketToken))
				{
					msg(Debug::None, "Invalid token to right of function in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Values must have operators, functions, or brackets to the left, and operators or brackets to the right
			case (ExpressionNode::ValueToken):
				// N.B. this first test should be unnecessary since it is accounted for above...
				if (ltoken == ExpressionNode::ValueToken)
				{
					msg(Debug::None, "Invalid token to left of function in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::FunctionToken) || (rtoken == ExpressionNode::ValueToken))
				{
					msg(Debug::None, "Invalid token to right of value in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

		}
	}
	dbgEnd(Debug::Calls,"Expression::validate");
	return TRUE;
}

// Set expression from supplied string
bool Expression::set(const char *s, VariableList *vars)
{
	dbgBegin(Debug::Calls,"Expression::set");
	// To cache an expression we parse the expression into tokens and then create a list of enumarated items in the same order.
	int n, arglen;
	char arg[64];
	const char *c;
	ExpressionNode *ex;
	ExpressionNode::TokenType prevToken = ExpressionNode::nTokenTypes;
	ExpressionNode::OperatorType ot;
	arglen = 0;
	// Clear list and set variable list
	expression_.clear();
	vars_ = vars;
	for (c = &s[0]; *c != '\0'; c++)
	{
		switch (*c)
		{
			// Skip delimiters
			case (9):	// Horizontal Tab
			case (' '):	// Space
				// These symbols may mark the termination of longer operators, constants, or variables
				if (arglen != 0)
				{
					arg[arglen] = '\0';
					prevToken = addLongOperator(arg);
					if (prevToken == ExpressionNode::nTokenTypes)
					{
						dbgEnd(Debug::Calls,"Expression::set");
						return FALSE;
					}
					arglen = 0;
				}
				break;
			// Brackets
			case ('('):
			case (')'):
				// These symbols may mark the termination of longer operators, constants, or variables
				if (arglen != 0)
				{
					arg[arglen] = '\0';
					prevToken = addLongOperator(arg);
					if (prevToken == ExpressionNode::nTokenTypes)
					{
						dbgEnd(Debug::Calls,"Expression::set");
						return FALSE;
					}
					arglen = 0;
				}
				ex = expression_.add();
				ex->setPersistentType(ExpressionNode::BracketToken);
				ex->setBracketType(*c == '(' ? ExpressionNode::LeftBracket : ExpressionNode::RightBracket);
				break;
			// Single-character operators
			case ('+'):
			case ('/'):
			case ('*'):
			case ('^'):
			case ('%'):
			case ('-'):
				// These symbols may mark the termination of longer operators, constants, or variables
				if (arglen != 0)
				{
					arg[arglen] = '\0';
					prevToken = addLongOperator(arg);
					if (prevToken == ExpressionNode::nTokenTypes)
					{
						dbgEnd(Debug::Calls,"Expression::set");
						return FALSE;
					}
					arglen = 0;
				}
				// For the minus symbol we check to see if its a unary minus first...
				if (*c == '-')
				{
					// If previous token is operator, bracket, or nothing at all then it must be a unary minus
					if ((prevToken != ExpressionNode::ValueToken) && (prevToken != ExpressionNode::FunctionToken))
					{
						ex = expression_.add();
						ex->setPersistentType(ExpressionNode::FunctionToken);
						ex->setFunctionType(ExpressionNode::NegateFunction);
						//printf("Added negate function\n");
						break;
					}
				}
				// Determine operator type
				ot = ExpressionNode::operatorType(*c);
				if (ot == ExpressionNode::nOperatorTypes)
				{
					msg(Debug::None, "Unrecognised operator '%c'.\n", *c);
					dbgEnd(Debug::Calls,"Expression::set");
					return FALSE;
				}
				ex = expression_.add();
				ex->setPersistentType(ExpressionNode::OperatorToken);
				ex->setOperatorType(ot);
				prevToken = ExpressionNode::OperatorToken;
				//printf("Added operator %i (%c)\n",ot,*c);
				break;
			// Normal character
			default: 
				arg[arglen] = *c;
				arglen ++;
				break;
		}
	}
	// There may be a variable / value / long operator left in arg[]...
	if (arglen != 0)
	{
		arg[arglen] = '\0';
		if (addLongOperator(arg) == ExpressionNode::nTokenTypes)
		{
			dbgEnd(Debug::Calls,"Expression::set");
			return FALSE;
		}
	}
	// Validate the expression and create a bracket plan...
	if (!validate() || !createPlan())
	{
		dbgEnd(Debug::Calls,"Expression::set");
		return FALSE;
	}
	dbgEnd(Debug::Calls,"Expression::set");
	return TRUE;
}

// Add long operator
ExpressionNode::TokenType Expression::addLongOperator(const char *s)
{
	dbgBegin(Debug::Calls,"Expression::addLongOperator");
	// This gets called on any chunk of parsed expression that isn't a single-character operator.
	ExpressionNode *ex = NULL;
	// First check is for a variable (begins with '$'), then for a value (contains at least one numeral), and then for long operators.
	if (s[0] == '$')
	{
		// Check to see that this variable already exists
		Variable *v = vars_->get(&s[1]);
		if (v != NULL)
		{
			ex = expression_.add();
			ex->setPersistentType(ExpressionNode::ValueToken);
			ex->setVariable(v);
		}
	}
	else
	{
		// Check for numeral
		Variable::VariableType vt = Variable::determineType(s);
		if ((vt == Variable::IntegerVariable) || (vt == Variable::FloatVariable))
		{
			ex = expression_.add();
			ex->setPersistentType(ExpressionNode::ValueToken);
			ex->setValue(atof(s));
		}
		else
		{
			ExpressionNode::FunctionType ft = ExpressionNode::functionType(s);
			if (ft == ExpressionNode::nFunctionTypes) msg(Debug::None, "Unrecognised expression function '%s'.\n", s);
			else
			{
				ex = expression_.add();
				ex->setPersistentType(ExpressionNode::FunctionToken);
				ex->setFunctionType(ft);
				//printf("Adding function token '%s' (%i)\n", ExpressionNode::functionType(ft),ft);
			}
		}
	}
	dbgEnd(Debug::Calls,"Expression::addLongOperator");
	return (ex == NULL ? ExpressionNode::nTokenTypes : ex->type());
}

// Create bracket solution plan
bool Expression::createPlan()
{
	dbgBegin(Debug::Calls,"Expression::createPlan");
	brackets_.clear();
	Reflist<ExpressionNode,int> bracketStack;
	// Go through expression and locate matching pairs of brackets
	for (ExpressionNode *ex = expression_.first(); ex != NULL; ex = ex->next)
	{
		// If this node is not a bracket, continue
		if (ex->type() != ExpressionNode::BracketToken) continue;
		// If this node is a LeftBracket, add it to the stack of open brackets.
		// Otherwise its a RightBracket, so pair it with the topmost LeftBracket on the bracketStack, placing it in the main brackets_ reflist.
		if (ex->bracketType() == ExpressionNode::LeftBracket) bracketStack.add(ex);
		else
		{
			// If bracketStack is empty, then we've encountered an extra right-bracket...
			if (bracketStack.nItems() == 0)
			{
				msg(Debug::None, "Extra right parenthesis found in expression:\n");
				print(ex);
				return FALSE;
			}
			// Create a new matching pair on the main brackets_ reflist
			brackets_.add(bracketStack.last()->item, ex);
			bracketStack.removeLast();
		}
	}
	// If we have left brackets left on the bracketStack then we are unbalanced....
	if (bracketStack.nItems() != 0)
	{
		msg(Debug::None, "Extra left parenthesis found in expression:\n");
		print(bracketStack.last()->item);
		return FALSE;
	}
	dbgEnd(Debug::Calls,"Expression::createPlan");
	return TRUE;
}

// Evaluate subexpression
void Expression::evaluate(ExpressionNode *left, ExpressionNode *right)
{
	// Evaluate the expression up to and including the node limits passed.
	double result;
	ExpressionNode *leftLastUnused;
	// Simplify functions first....
	leftLastUnused = (left->used() ? left->nextUnused(right) : left);
	for (ExpressionNode *ex = leftLastUnused; ex != NULL; ex = ex->nextUnused(right))
	{
		if (ex->type() != ExpressionNode::FunctionToken) continue;
		switch (ex->functionType())
		{
			case (ExpressionNode::NegateFunction):
				result = -ex->nextUnused()->value();
				break;
			case (ExpressionNode::SqrtFunction):
				result = sqrt(ex->nextUnused()->value());
				break;
			case (ExpressionNode::CosFunction):
				result = cos(ex->nextUnused()->value());
				break;
			case (ExpressionNode::SinFunction):
				result = sin(ex->nextUnused()->value());
				break;
		}
		// All functions nodes get replaced with their result, and the right-hand argument is set to nothing
		ex->makeValue(result);
		ex->nextUnused()->setUsed();
	}
	// Now do operators in order of precedence.
	for (int op = 0; op < ExpressionNode::nOperatorTypes; op++)
	{
		//printf("Doing operator %i\n", op);
		for (ExpressionNode *ex = left; ex != right->next; ex = ex->next)
		{
			// Skip used nodes
			if (ex->used()) continue;
			if ((ex->type() != ExpressionNode::OperatorToken) || (ex->operatorType() != op))
			{
				leftLastUnused = ex;
				continue;
			}
			if (ex->operatorType() != op) continue;
			switch (ex->operatorType())
			{
				case (ExpressionNode::ModulusOperator):
					result = int(leftLastUnused->value()) % int(ex->nextUnused()->value());
					break;
				case (ExpressionNode::PowerOperator):
					result = pow(leftLastUnused->value(), ex->nextUnused()->value());
					break;
				case (ExpressionNode::MultiplyOperator):
					result = leftLastUnused->value() * ex->nextUnused()->value();
					break;
				case (ExpressionNode::DivideOperator):
					result = leftLastUnused->value() / ex->nextUnused()->value();
					break;
				case (ExpressionNode::AddOperator):
					result = leftLastUnused->value() + ex->nextUnused()->value();
					break;
				case (ExpressionNode::SubtractOperator):
					result = leftLastUnused->value() - ex->nextUnused()->value();
					break;
			}
			// All operator nodes get replaced with their result, and the left and right-hand arguments are set to nothing
			//printf("Result of operator is %f\n",result);
			ex->makeValue(result);
			ex->nextUnused()->setUsed();
			leftLastUnused->setUsed();
			leftLastUnused = ex;
		}
		print(NULL, FALSE);
	}
}

// Evaluate expression
double Expression::evaluate()
{
	dbgBegin(Debug::Calls,"Expression::evaluate");
	ExpressionNode *ex;
	// Reset all nodes in expression
	for (ex = expression_.first(); ex != NULL; ex = ex->next) ex->reset();
	// Evaluate all subexpressions defined in the bracket_ list
	for (Refitem<ExpressionNode,ExpressionNode*> *ri = brackets_.first(); ri != NULL; ri = ri->next)
	{
		evaluate(ri->item->next, ri->data->prev);
		ri->item->setUsed();
		ri->data->setUsed();
	}
	// Lastly, evaluate whole expression
	evaluate(expression_.first(), expression_.last());
	//printf("Reduced expression is:\n");
	//print(NULL, FALSE);
	// Find last remaining unused node and return its value
	ex = expression_.first();
	if (ex->used()) ex = ex->nextUnused();
	dbgEnd(Debug::Calls,"Expression::evaluate");
	return ex->value();
}

// Print expression
void Expression::print(ExpressionNode *highlight, bool showUsed)
{
	ExpressionNode::TokenType tt;
	char s[512], bit[128];
	int length, hlength, i;
	s[0] = '\0';
	for (ExpressionNode *ex = expression_.first(); ex != NULL; ex = ex->next)
	{
		if ((!showUsed) && (ex->used())) continue;
		tt = ex->type();
		if (ex == highlight) length = strlen(s);
		switch (tt)
		{
			case (ExpressionNode::ValueToken):
				if (ex->used()) strcpy(bit, ftoa(ex->value()));
				else if (ex->variable() == NULL) strcpy(bit,ftoa(ex->value()));
				else sprintf(bit,"$%s", ex->variable()->name());
				break;
			case (ExpressionNode::FunctionToken):
				strcpy(bit, FunctionTypeKeywords[ex->functionType()]);
				break;
			case (ExpressionNode::OperatorToken):
				bit[0] = OperatorTypeKeywords[ex->operatorType()];
				bit[1] = '\0';
				break;
			case (ExpressionNode::BracketToken):
				bit[0] = (ex->bracketType() == ExpressionNode::LeftBracket ?  '(' : ')');
				bit[1] = '\0';
				break;
		}
		if (ex == highlight) hlength = strlen(bit);
		strcat(s, bit);
		strcat(s," ");
	}
	// Print it
	printf(">>>>  %s\n", s);
	// If a highlight was requested, construct the string...
	if (highlight != NULL)
	{
		for (i=0; i<length; i++) s[i] = ' ';
		for (i=length; i<length+hlength; i++) s[i] = '^';
		s[length+hlength] = '\0';
		printf(">>>>  %s\n", s);
	}
}

