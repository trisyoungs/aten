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
char OperatorTypeKeywords[ExpressionNode::nOperatorTypes] = { '-', '+', '/', '*', '^', '%' };
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
const char *FunctionTypeKeywords[ExpressionNode::nFunctionTypes] = { "sqrt", "cos", "sin" };
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
}

// Set type of node
void ExpressionNode::setType(ExpressionNode::TokenType tt)
{
	type_ = tt;
}

// Return node type
ExpressionNode::TokenType ExpressionNode::type()
{
	return type_;
}

// Set whether the node has been used already
void ExpressionNode::setUsed(bool used)
{
	used_ = used;
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
	if (used_) return reducedValue_;
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
	int nLeft = 0, nRight = 0;
	ExpressionNode::TokenType ltoken, rtoken;
	int count = 0;
	// Here we check the grammar of the expression.
	for (ExpressionNode *ex = expression_.first(); ex != NULL; ex = ex->next)
	{
		count ++;
		// Grab adjacent nodes
		ltoken = (ex->prev == NULL ? ExpressionNode::nTokenTypes : ex->prev->type());
		rtoken = (ex->next == NULL ? ExpressionNode::nTokenTypes : ex->next->type());
		switch (ex->type())
		{
			// Value tokens cannot be adjacent to each other, have right-brackets to the left, or left-brackets to the right
			case (ExpressionNode::ValueToken):
				if ((ltoken == ExpressionNode::ValueToken) || (rtoken == ExpressionNode::ValueToken))
				{
					msg(Debug::None, "Adjacent values in expression (position %i).\n", count);
					return FALSE;
				}
				if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::RightBracket))
				{
					msg(Debug::None, "Value following right bracket in expression (position %i).\n", count);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::LeftBracket))
				{
					msg(Debug::None, "Value following left bracket in expression (position %i).\n", count);
					return FALSE;
				}
				break;
			// Operator tokens cannot be adjacent to each other, have right-brackets to the left, or left-brackets to the right
			case (ExpressionNode::OperatorToken):
				if ((ltoken == ExpressionNode::OperatorToken) || (rtoken == ExpressionNode::OperatorToken))
				{
					msg(Debug::None, "Adjacent oprtators in expression (position %i).\n", count);
					return FALSE;
				}
				if ((ltoken == ExpressionNode::BracketToken) && (ex->prev->bracketType() == ExpressionNode::RightBracket))
				{
					msg(Debug::None, "Operator following right bracket in expression (position %i).\n", count);
					return FALSE;
				}
				if ((rtoken == ExpressionNode::BracketToken) && (ex->next->bracketType() == ExpressionNode::LeftBracket))
				{
					msg(Debug::None, "Operator following left bracket in expression (position %i).\n", count);
					return FALSE;
				}
				break;
		}
	}
	dbgEnd(Debug::Calls,"Expression::validate");
}

// Set expression from supplied string
bool Expression::set(const char *s, VariableList *vars)
{
	dbgBegin(Debug::Calls,"Expression::set");
	// To cache an expression we parse the expression into tokens and then create a list of enumarated items in the same order.
	static int n, arglen;
	static char arg[64];
	static const char *c;
	ExpressionNode *ex;
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
				break;
			// Single-character operators / symbols (except minus sign)
			case ('('):
			case (')'):
			case ('+'):
			case ('/'):
			case ('*'):
			case ('^'):
			case ('%'):
				// These symbols may mark the termination of longer operators, constants, or variables
				if (arglen != 0)
				{
					arg[arglen] = '\0';
					if (!addLongOperator(arg))
					{
						dbgEnd(Debug::Calls,"Expression::set");
						return FALSE;
					}
					arglen = 0;
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
				ex->setType(ExpressionNode::OperatorToken);
				ex->setOperatorType(ot);
				printf("Added operator %i (%c)\n",ot,*c);
				break;
			// Minus operator (or unary minus?)
			case ('-'):
				// If 
dsfdf
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
		if (!addLongOperator(arg))
		{
			dbgEnd(Debug::Calls,"Expression::set");
			return FALSE;
		}
	}
	print();
	// Validate the expression...
	if (validate()) createPlan();
	else
	{
		dbgEnd(Debug::Calls,"Expression::set");
		return TRUE;
	}
	dbgEnd(Debug::Calls,"Expression::set");
	return TRUE;
}

// Add long operator
bool Expression::addLongOperator(const char *s)
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
			ex->setType(ExpressionNode::VariableToken);
			ex->setVariable(v);
		}
	}
	else
	{
		// Check for numeral
		bool wasnumber = FALSE;
		for (int n=0; n<strlen(s); n++)
		{
			if ((s[n] > 47) && (s[n] < 58))
			{
				wasnumber = TRUE;
				ex = expression_.add();
				ex->setType(ExpressionNode::ValueToken);
				ex->setValue(atof(s));
				break;
			}
		}
		if (!wasnumber)
		{
			ExpressionNode::TokenType tt = ExpressionNode::tokenType(s);
			if (tt == ExpressionNode::nTokenTypes) msg(Debug::None, "Unrecognised expression token '%s'.\n", s);
			else
			{
				ex = expression_.add();
				ex->setType(tt);
				printf("Adding long token %i\n",tt);
			}
		}
	}
	dbgEnd(Debug::Calls,"Expression::addLongOperator");
	return (ex == NULL ? FALSE : TRUE);
}

// Evaluate numerical expression and return simplified result
// This is a recursive routine, to be called on sub-expressions (i.e. bracketed parts)
char operators[] = "^*/%+-";
// const char *evaluate(const char *s, VariableList *vars)
// {
// 	int lbracket, rbracket, n, m, leftarg, rightarg;
// 	char scopy[128], substr[128], subresult[128], arg[128];
// 	char *c;
// 	static double a, b, x;
// 	static bool isop;
// 	Variable *v;
// 	// Grab original string and work on scopy from now on
// 	strcpy(scopy,s);
// 	// Resolve brackets into results
// 	do
// 	{
// 		lbracket = -1;
// 		rbracket = -1;
// 		n=0;
// 		// Store position of each '(' we find, and break when we get the first ')'
// 		for (c = scopy; *c != '\0'; c++)
// 		{
// 			if (*c == '(') lbracket = n;
// 			if (*c == ')')
// 			{
// 				rbracket = n;
// 				break;
// 			}
// 			n++;
// 		}
// 		// Check for mis-matched brackets
// 		if (((lbracket == -1) && (rbracket != -1)) || ((lbracket != -1) && (rbracket == -1)))
// 		{
// 			printf("evaluate - Mis-matched brackets in expression.\n");
// 			return "0";
// 		}
// 		// Get substring and evaluate it
// 		m = rbracket-lbracket;
// 		//printf("Bracketpos = %i %i len=%i\n",lbracket,rbracket, m);
// 		// If we've found brackets, recursively evaluate the sub expression first
// 		if ((lbracket != -1) && (rbracket != -1))
// 		{
// 			strncpy(substr, &scopy[lbracket+1], m);
// 			substr[(rbracket-lbracket)-1] = '\0';
// 			//printf("SUBSTRING to evaluate = %s\n",substr);
// 			strcpy(subresult, evaluate(substr, vars));
// 			//printf("RESULT of SUBSTRING EVALUATE = %s\n",subresult);
// 			// Grab part of expression before left bracket
// 			strncpy(substr, scopy, lbracket);
// 			substr[lbracket] = '\0';
// 			strcat(substr, subresult);
// 			strcat(substr, &scopy[rbracket+1]);
// 			// Copy final string back to scopy
// 			strcpy(scopy, substr);
// 		}
// 	} while ((lbracket != -1) && (rbracket != -1));
// 	// Evaluate now bracketless expression
// 	// Parse the string into operators and values
// 	if (!parser.getArgsExpression(scopy))
// 	{
// 		printf("evaluate - Error parsing expression.\n");
// 		return "0";
// 	}
// 	// Cycle over operators (in precedence) and replace with evaluated result.
// 	// Store position of last non-blank argument we encounter to use with token
// 	for (c = operators; *c != '\0'; c++)
// 	{
// 		leftarg = -1;
// 		for (n=0; n<parser.nArgs()-1; n++)
// 		{
// 			isop = parser.isOperator(n, *c);
// 			if ((!parser.isBlank(n)) && (!isop)) leftarg = n;
// 			if (!isop) continue;
// 			// Find next non-blank argument
// 			rightarg = -1;
// 			for (m=n+1; m<parser.nArgs(); m++)
// 				if (!parser.isBlank(m))
// 				{
// 					rightarg = m;
// 					break;
// 				}
// 			// Check argument availability
// 			if ((leftarg != -1) && (rightarg != -1))
// 			{
// 				//printf("left / right args %i %i\n",leftarg,rightarg);
// 				// If either argument is a variable, grab its value
// 				strcpy(arg,parser.argc(leftarg));
// 				if (arg[0] == '$')
// 				{
// 					v = vars->get(&arg[1]);
// 					a = (v == NULL ? 0.0 : v->asDouble());
// 					if (v == NULL) msg(Debug::None,"Warning: Unrecognised variable '%s' in expression - using zero...\n",&arg[1]);
// 				}
// 				else a = atof(arg);
// 				strcpy(arg,parser.argc(rightarg));
// 				if (arg[0] == '$')
// 				{
// 					v = vars->get(&arg[1]);
// 					b = (v == NULL ? 0.0 : v->asDouble());
// 					if (v == NULL) msg(Debug::None,"Warning: Unrecognised variable '%s' in expression - using zero...\n",&arg[1]);
// 				}
// 				else b = atof(arg);
// 				switch (*c)
// 				{
// 					case ('^'):
// 						x = pow(a,b);
// 						break;
// 					case ('*'):
// 						x = a * b;
// 						break;
// 					case ('/'):
// 						x = a / b;
// 						break;
// 					case ('%'):
// 						x = int(a)%(int(b));
// 						break;
// 					case ('+'):
// 						x = a + b;
// 						break;
// 					case ('-'):
// 						x = a - b;
// 						break;
// 				}
// 				// Store result and blank arguments
// 				parser.setArg(n,ftoa(x));
// 				parser.setArg(leftarg,"");
// 				parser.setArg(rightarg,"");
// 				// Set new leftmost and rightmost arguments
// 				leftarg = n;
// 				rightarg = -1;
// 			}
// 			else
// 			{
// 				printf("evaluate - Operator was missing an argument.\n");
// 				return "0";
// 			} 
// 		}
// 	}
// 	// Look for non-blank argument and set it as result
// 	for (n=0; n<parser.nArgs(); n++) if (!parser.isBlank(n)) break;
// 	if (n < parser.nArgs()) return parser.argc(n);
// 	// Failed to find a return value!
// 	printf("evaluate - failed to find return argument.\n");
// 	return ".0";
// }

// Evaluate subexpression
double Expression::evaluate(ExpressionNode *left, ExpressionNode *right)
{
	// Check for brackets in this part of the subexpression - work them out until there are none left.
	
}

// Evaluate expression
double Expression::evaluate()
{
	// Reset all nodes in expression
	for (ExpressionNode *ex = expression_.first(); ex != NULL; ex = ex->next) ex->reset();
	return evaluate(expression_.first(), expression_.last());
}

// Print expression
void Expression::print()
{
	ExpressionNode::TokenType tt;
	char s[512];
	s[0] = '\0';
	for (ExpressionNode *ex = expression_.first(); ex != NULL; ex = ex->next)
	{
		tt = ex->type();
		switch (tt)
		{
			case (ExpressionNode::ValueToken):
				printf("Value.\n");
				strcat(s, ftoa(ex->value()));
				break;
			case (ExpressionNode::VariableToken):
				printf("Variable\n");
				strcat(s, ex->variable()->name());
				break;
			default:
				printf("Token %i.\n",tt);
				strcat(s, TokenTypeKeywords[tt]);
				break;
		}
		strcat(s," ");
	}
	// Print it
	printf("Expression is: '%s'\n", s);
}

