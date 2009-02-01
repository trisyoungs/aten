/*
	*** Arithmetic Expression
	*** src/parse/expression.cpp
	Copyright T. Youngs 2007-2009

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
#include "parse/expressionnode.h"
#include "parse/variablelist.h"
#include "base/mathfunc.h"
#include <math.h>
#include <string.h>

// Constructor
Expression::Expression()
{
	// Private variables
	evaluatesToFloat_ = TRUE;
	vars_ = NULL;
}

// Return whether expression returns a floating-point result
bool Expression::evaluatesToFloat()
{
	return evaluatesToFloat_;
}

// Validate expression
bool Expression::validate()
{
	msg.enter("Expression::validate");
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
					msg.print( "A token must appear either side of this operator:\n");
					print(ex);
					return FALSE;
				}
			// Function tokens can appear at the start, but not the end
			case (ExpressionNode::FunctionToken):
				if (rtoken == ExpressionNode::nTokenTypes)
				{
					msg.print( "A token must appear to the right of this function:\n");
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
					msg.print( "Operator immediately follows an opening parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::RightBracket))
				{
					msg.print( "Operator immediately preceeds a closing parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Values cannot follow right-brackets or proceed left-brackets
			case (ExpressionNode::ValueToken):
				if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::RightBracket))
				{
					msg.print( "Value immediately follows a closing parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::LeftBracket))
				{
					msg.print( "Value immediately preceeds an opening parenthesis in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Functions cannot follow right-brackets
			case (ExpressionNode::FunctionToken):
				if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::RightBracket))
				{
					msg.print( "Function immediately follows a closing parenthesis in expression:\n");
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
						msg.print( "Opposing parenthesis may not be adjacent in expressions:\n");
						print(ex);
						return FALSE;
					}
					else if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::RightBracket))
					{
						msg.print( "Empty parentheses in expressions are not allowed:\n");
						print(ex);
						return FALSE;
					}
				}
				else
				{
					if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::LeftBracket))
					{
						msg.print( "Empty parentheses in expressions are not allowed:\n");
						print(ex);
						return FALSE;
					}
					else if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::LeftBracket))
					{
						msg.print( "Opposing parenthesis may not be adjacent in expressions:\n");
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
				msg.print( "Adjacent values, operators, or functions in expression:\n");
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
					msg.print( "Invalid token to left of operator in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::OperatorToken) || (rtoken == ExpressionNode::nTokenTypes))
				{
					msg.print( "Invalid token to right of operator in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Functions must have operators or brackets to the left, and values or brackets to the right
			case (ExpressionNode::FunctionToken):
				if ((ltoken != ExpressionNode::OperatorToken) && (ltoken != ExpressionNode::BracketToken) && (ltoken != ExpressionNode::nTokenTypes))
				{
					msg.print( "Invalid token to left of function in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken != ExpressionNode::ValueToken) && (rtoken != ExpressionNode::BracketToken))
				{
					msg.print( "Invalid token to right of function in expression:\n");
					print(ex);
					return FALSE;
				}
				break;

			// Values must have operators, functions, or brackets to the left, and operators or brackets to the right
			case (ExpressionNode::ValueToken):
				// N.B. this first test should be unnecessary since it is accounted for above...
				if (ltoken == ExpressionNode::ValueToken)
				{
					msg.print( "Invalid token to left of function in expression:\n");
					print(ex);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::FunctionToken) || (rtoken == ExpressionNode::ValueToken))
				{
					msg.print( "Invalid token to right of value in expression:\n");
					print(ex);
					return FALSE;
				}
				break;
		}
	}
	msg.exit("Expression::validate");
	return TRUE;
}

// Set expression from supplied string
bool Expression::set(const char *s, VariableList *vars)
{
	msg.enter("Expression::set");
	// To cache an expression we parse the expression into tokens and then create a list of enumarated items in the same order.
	int arglen;
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
					prevToken = addLongToken(arg);
					if (prevToken == ExpressionNode::nTokenTypes)
					{
						msg.exit("Expression::set");
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
					prevToken = addLongToken(arg);
					if (prevToken == ExpressionNode::nTokenTypes)
					{
						msg.exit("Expression::set");
						return FALSE;
					}
					arglen = 0;
				}
				ex = expression_.add();
				ex->setPersistentType(ExpressionNode::BracketToken);
				ex->setBracketType(*c == '(' ? ExpressionNode::LeftBracket : ExpressionNode::RightBracket);
				prevToken = ExpressionNode::BracketToken;
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
					prevToken = addLongToken(arg);
					if (prevToken == ExpressionNode::nTokenTypes)
					{
						msg.exit("Expression::set");
						return FALSE;
					}
					arglen = 0;
				}
				// For the minus symbol we check to see if its a unary minus first...
				if (*c == '-')
				{
					// If previous token is an operator, left bracket, or nothing at all then it must be a unary minus (function)
					if ((prevToken == ExpressionNode::OperatorToken) || ((prevToken == ExpressionNode::BracketToken) && (ex->bracketType() == ExpressionNode::LeftBracket)) || (prevToken == ExpressionNode::nTokenTypes))
					//if ((prevToken != ExpressionNode::ValueToken) && (prevToken != ExpressionNode::FunctionToken) && (prevToken != ExpressionNode::BracketToken))
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
					msg.print( "Unrecognised operator '%c'.\n", *c);
					msg.exit("Expression::set");
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
		if (addLongToken(arg) == ExpressionNode::nTokenTypes)
		{
			msg.exit("Expression::set");
			return FALSE;
		}
	}
	// Validate the expression and create a bracket plan...
	if (!validate() || !createPlan())
	{
		msg.exit("Expression::set");
		return FALSE;
	}
	// Lastly, determine the return type of the expression by running it...
	ex = evaluate();
	evaluatesToFloat_ = ex->isFloat();
	msg.print(Messenger::Expressions, "Expression '%s' reduces to %s.\n", s, ex->isFloat() ? "a float" : "an integer");
	msg.exit("Expression::set");
	return TRUE;
}

// Add long (non-operator) token
ExpressionNode::TokenType Expression::addLongToken(const char *s)
{
	msg.enter("Expression::addLongToken");
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
			ex->setPersistentType(ExpressionNode::ValueToken, v->type() == Variable::FloatVariable);
			ex->setVariable(v);
		}
		else msg.print("Variable '%s' in expression has not been declared.\n", &s[1]);
	}
	else
	{
		// Check for numeral
		Variable::VariableType vt = Variable::determineType(s);
		if ((vt == Variable::IntegerVariable) || (vt == Variable::FloatVariable))
		{
			ex = expression_.add();
			ex->setPersistentType(ExpressionNode::ValueToken, vt == Variable::FloatVariable);
			if (vt == Variable::IntegerVariable) ex->setValue(atoi(s));
			else ex->setValue(atof(s));
		}
		else
		{
			ExpressionNode::FunctionType ft = ExpressionNode::functionType(s);
			if (ft == ExpressionNode::nFunctionTypes) msg.print( "Unrecognised expression function '%s'.\n", s);
			else
			{
				ex = expression_.add();
				ex->setPersistentType(ExpressionNode::FunctionToken);
				ex->setFunctionType(ft);
				//printf("Adding function token '%s' (%i)\n", ExpressionNode::functionType(ft),ft);
			}
		}
	}
	msg.exit("Expression::addLongToken");
	return (ex == NULL ? ExpressionNode::nTokenTypes : ex->type());
}

// Create bracket solution plan
bool Expression::createPlan()
{
	msg.enter("Expression::createPlan");
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
				msg.print( "Extra right parenthesis found in expression:\n");
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
		msg.print( "Extra left parenthesis found in expression:\n");
		print(bracketStack.last()->item);
		return FALSE;
	}
	msg.exit("Expression::createPlan");
	return TRUE;
}

// Evaluate subexpression
void Expression::evaluate(ExpressionNode *left, ExpressionNode *right)
{
	// Evaluate the expression up to and including the node limits passed.
	double result;
	int iresult;
	bool floatResult = TRUE, leftfloat, rightfloat;
	ExpressionNode *leftLastUnused, *rightNode;
	// Simplify functions first....
	leftLastUnused = (left->used() ? left->nextUnused(right) : left);
	for (ExpressionNode *ex = leftLastUnused; ex != NULL; ex = ex->nextUnused(right))
	{
		if (ex->type() != ExpressionNode::FunctionToken) continue;
		floatResult = TRUE;
		rightNode = ex->nextUnused();
		rightfloat = rightNode->isFloat();
		switch (ex->functionType())
		{
			case (ExpressionNode::NegateFunction):
				if (rightfloat) result = -ex->nextUnused()->asDouble();
				else
				{
					iresult = -ex->nextUnused()->asInteger();
					floatResult = FALSE;
				}
				break;
			case (ExpressionNode::SqrtFunction):
				result = sqrt(rightNode->asDouble());
				break;
			case (ExpressionNode::CosFunction):
				result = cos(rightNode->asDouble() / DEGRAD);
				break;
			case (ExpressionNode::SinFunction):
				result = sin(rightNode->asDouble() / DEGRAD);
				break;
			case (ExpressionNode::TanFunction):
				result = tan(rightNode->asDouble() / DEGRAD);
				break;
			case (ExpressionNode::AbsFunction):
				if (rightfloat) result = fabs(rightNode->asDouble());
				else
				{
					iresult = abs(rightNode->asInteger());
					floatResult = FALSE;
				}
				break;
			//case (expressionNode::INTEGER):
				// floatResult = FALSE;
				//iresult = 0;
				break;
		}
		// All function nodes get replaced with their result, and the right-hand argument is subsequently ignored
		if (floatResult) ex->makeValue(result);
		else ex->makeValue(iresult);
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
			rightNode = ex->nextUnused();
			// Check which variable types are involved
			leftfloat = leftLastUnused->isFloat();
			rightfloat = rightNode->isFloat();
			// If both sides are integers do integer arithmetic
			if ((!leftfloat) && (!rightfloat))
			{
				floatResult = FALSE;
				switch (ex->operatorType())
				{
					case (ExpressionNode::ModulusOperator):
						iresult = leftLastUnused->asInteger() % rightNode->asInteger();
						break;
					case (ExpressionNode::PowerOperator):
						iresult = power(leftLastUnused->asInteger(), rightNode->asInteger());
						break;
					case (ExpressionNode::MultiplyOperator):
						iresult = leftLastUnused->asInteger() * rightNode->asInteger();
						break;
					case (ExpressionNode::DivideOperator):
						iresult = leftLastUnused->asInteger() / rightNode->asInteger();
						break;
					case (ExpressionNode::AddOperator):
						iresult = leftLastUnused->asInteger() + rightNode->asInteger();
						break;
					case (ExpressionNode::SubtractOperator):
						iresult = leftLastUnused->asInteger() - rightNode->asInteger();
						break;
				}
			}
			else
			{
				floatResult = TRUE;
				switch (ex->operatorType())
				{
					case (ExpressionNode::ModulusOperator):
						floatResult = FALSE;
						iresult = leftLastUnused->asInteger() % rightNode->asInteger();
						break;
					case (ExpressionNode::PowerOperator):
						result = pow(leftLastUnused->asDouble(), rightNode->asDouble());
						break;
					case (ExpressionNode::MultiplyOperator):
						result = leftLastUnused->asDouble() * rightNode->asDouble();
						break;
					case (ExpressionNode::DivideOperator):
						result = leftLastUnused->asDouble() / rightNode->asDouble();
						break;
					case (ExpressionNode::AddOperator):
						result = leftLastUnused->asDouble() + rightNode->asDouble();
						break;
					case (ExpressionNode::SubtractOperator):
						result = leftLastUnused->asDouble() - rightNode->asDouble();
						break;
				}
			}
			// All operator nodes get replaced with their result, and the left and right-hand arguments are set to nothing
			//printf("Result of operator is %f\n",result);
			if (floatResult) ex->makeValue(result);
			else ex->makeValue(iresult);
			ex->nextUnused()->setUsed();
			leftLastUnused->setUsed();
			leftLastUnused = ex;
		}
		if (msg.isOutputActive(Messenger::Expressions)) print(NULL, FALSE);
	}
}

// Evaluate expression, returning result node
ExpressionNode *Expression::evaluate()
{
	msg.enter("Expression::evaluate");
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
	msg.print(Messenger::Expressions, "Reduced expression is:\n");
	if (msg.isOutputActive(Messenger::Expressions)) print(NULL, FALSE);
	// Find last remaining unused node and return its value
	ex = expression_.first();
	if (ex->used()) ex = ex->nextUnused();
	msg.exit("Expression::evaluate");
	return ex;
}

// Evaluate integer expression
int Expression::evaluateAsInteger()
{
	//if (evaluatesToFloat_) printf("This is a floating-point expression. Why request an integer?\n");
	return evaluate()->asInteger();
}

// Evaluate float expression
double Expression::evaluateAsDouble()
{
	//if (!evaluatesToFloat_) printf("This is an integer expression. Why request a double?\n");
	return evaluate()->asDouble();
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
				if (ex->used())
				{
					if (ex->isFloat()) strcpy(bit, ftoa(ex->asDouble()));
					else strcpy(bit, itoa(ex->asInteger()));
				}
				else if (ex->variable() == NULL) strcpy(bit,ftoa(ex->asDouble()));
				else sprintf(bit,"$%s", ex->variable()->name());
				break;
			case (ExpressionNode::FunctionToken):
				strcpy(bit, ExpressionNode::functionType(ex->functionType()));
				break;
			case (ExpressionNode::OperatorToken):
				bit[0] = ExpressionNode::operatorType(ex->operatorType());
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

