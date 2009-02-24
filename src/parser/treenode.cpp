/*
	*** Tree Node
	*** src/parser/treenode.cpp
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

#include "parser/treenode.h"
#include "variables/accesspath.h"
#include "base/sysfunc.h"

// Constructors
TreeNode::TreeNode()
{
	// Private variables
	function_ = Command::CA_ROOTNODE;
	parent_ = NULL;
	ptr_ = NULL;
	branch_ = NULL;
	format_ = NULL;
	loopActive_ = FALSE;
	variableList_ = NULL;
	ifTest_ = IfTests::EqualTo;

	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructor
TreeNode::~TreeNode()
{
}

/*
// Execute
*/


/*
// Argument Creation
*/


/*
// Argument Access
*/

// Return variable argument
Variable *TreeNode::arg(int argno)
{
	Refitem<Variable,int> *ri = args_[argno];
	return ri->item;
}

// Return argument as character
const char *TreeNode::argc(int argno)
{
	Refitem<Variable,int> *ri = args_[argno];
	return (ri == NULL ?  "NULL" : ri->item->asCharacter());
}

// Return argument as integer
int TreeNode::argi(int argno)
{
	Refitem<Variable,int> *ri = args_[argno];
	return (ri == NULL ?  0 : ri->item->asInteger());
}

// Return argument as double
double TreeNode::argd(int argno)
{
	Refitem<Variable,int> *ri = args_[argno];
	return (ri == NULL ? 0.0 : ri->item->asDouble());
}

// Return argument as float
float TreeNode::argf(int argno)
{
	return (float) argd(argno);
}

// Return argument as bool
bool TreeNode::argb(int argno)
{
	Refitem<Variable,int> *ri = args_[argno];
	return (ri == NULL ? -1 : ri->item->asBool());
}

// Return argument as pointer
void *TreeNode::argp(int argno, VTypes::DataType dt)
{
	Refitem<Variable,int> *ri = args_[argno];
	return (ri == NULL ? NULL : ri->item->asPointer(dt));
}

// Returns whether argument 'n' was provided
bool TreeNode::hasArg(int argno)
{
	return ((argno+1) > args_.nItems() ? FALSE : TRUE);
}

// Return variable type of argument
VTypes::DataType TreeNode::argt(int argno)
{
	Refitem<Variable,int> *ri = args_[argno];
	return (ri == NULL ? VTypes::NoData : ri->item->type());
}

// Print data variables
void TreeNode::printArgs()
{
	msg.enter("TreeNode::printArgs");
	int i = 0;
	for (Refitem<Variable,int> *ri = args_.first(); ri != NULL; ri = ri->next)
	{
		printf("%2i %20li", i, ri);
		//printf("%12s [%10s]", v->name(), VTypes::dataType(v->type()));
		if (ri->item->type() < VTypes::AtomData) printf("%20s\n", ri->item->asCharacter());
 		else printf("%li\n", ri->item->asPointer(ri->item->type()));
		i++;
	}
	msg.exit("TreeNode::printArgs");
}

// Return arguments as Vec3<double>
Vec3<double> TreeNode::arg3d(int i)
{
	msg.enter("TreeNode::arg3d");
        static Vec3<double> result;
        if (i > (args_.nItems()-3)) printf("TreeNode::arg3d - Starting point too close to end of argument list.\n");
        result.set(args_[i]->item->asDouble(), args_[i+1]->item->asDouble(), args_[i+2]->item->asDouble());
	msg.exit("TreeNode::arg3d");
        return result;
}

// Return arguments as Vec3<float>
Vec3<float> TreeNode::arg3f(int i)
{
	msg.enter("TreeNode::arg3f");
        static Vec3<float> result;
        if (i > (args_.nItems()-3)) printf("TreeNode::arg3f - Starting point too close to end of argument list.\n");
        result.set(args_[i]->item->asFloat(), args_[i+1]->item->asFloat(), args_[i+2]->item->asFloat());
	msg.exit("TreeNode::arg3f");
        return result;
}

// Return arguments as Vec3<int>
Vec3<int> TreeNode::arg3i(int i)
{
	msg.enter("TreeNode::arg3i");
	static Vec3<int> result;
	if (i > (args_.nItems()-3)) printf("TreeNode::arg3i - Starting point too close to end of argument list.\n");
        result.set(args_[i]->item->asInteger(), args_[i+1]->item->asInteger(), args_[i+2]->item->asInteger());
	msg.exit("TreeNode::arg3i");
	return result;
}

// Create branch
List<TreeNode> *TreeNode::createBranch()
{
	msg.enter("TreeNode::createBranch");
	if (branch_ != NULL) printf("TreeNode::createBranch <<<< Already has a branch >>>>\n");
	branch_ = new List<TreeNode>;
	msg.exit("TreeNode::createBranch");
	return branch_;
}

// Create branch
bool TreeNode::createFormat(const char *s, bool delimited)
{
	msg.enter("TreeNode::createFormat");
	bool result = FALSE;
	if (format_ != NULL) printf("TreeNode::createFormat <<<< Already has a format >>>>\n");
	else
	{
		format_ = new Format;
		result = format_->create(s, *variableList_, delimited);
	}
	msg.exit("TreeNode::createFormat");
	return result;
}

// Set if condition test
bool TreeNode::setIfTest(const char *s)
{
	msg.enter("TreeNode::setIfTest");
	bool result = TRUE;
	int n, m;
	m = 0;
	for (n=0; s[n] != '\0'; n++)
		switch (s[n])
		{
			case ('='):
				m += 1;
				break;
			case ('<'):
				m += 2;
				break;
			case ('>'):
				m += 4;
				break;
			default:
				msg.print( "Unrecognised character '%c' in 'if' condition\n", s[n]);
				result = FALSE;
				break;
		}
	if (m > IfTests::NotEqualTo) result = FALSE;
	else ifTest_ = (IfTests::IfTest) m;
	msg.exit("TreeNode::setIfTest");
	return result;
}

// Evaluate condition
bool TreeNode::ifEvaluate()
{
	msg.enter("TreeNode::ifEvaluate");
	bool result;
	static Variable *v1, *v2;
	static char string1[512], string2[512];
	static double d1, d2;
	VTypes::DataType vt1, vt2;
	static int i1, i2;
	v1 = args_[0]->item;
	v2 = args_[2]->item;
	// Determine how to do the comparison
	// Consider pointers as integers...
	vt1 = v1->type();
	if (vt1 > VTypes::RealData) vt1 = VTypes::IntegerData;
	vt2 = v2->type();
	if (vt2 > VTypes::RealData) vt2 = VTypes::IntegerData;
	if (vt2 > vt1) vt1 = vt2;
	if (vt1 == VTypes::CharacterData)
	{
		strcpy(string1, v1->asCharacter());
		strcpy(string2, v2->asCharacter());
		msg.print(Messenger::Commands, "If Test: var1(%s)=[%s] (%s) var2(%s)=[%s]\n", v1->name(), string1, IfTests::ifTest(ifTest_), v2->name(), string2);

		switch (ifTest_)
		{
			case (IfTests::EqualTo):
				result = (strcmp(string1,string2) == 0 ? TRUE : FALSE);
				break;
			case (IfTests::LessThan):
				result = (strcmp(string1,string2) < 0 ? TRUE : FALSE);
				break;
			case (IfTests::LessThanEqualTo):
				result = (strcmp(string1,string2) <= 0 ? TRUE : FALSE);
				break;
			case (IfTests::GreaterThan):
				result = (strcmp(string1,string2) > 0 ? TRUE : FALSE);
				break;
			case (IfTests::GreaterThanEqualTo):
				result = (strcmp(string1,string2) >= 0 ? TRUE : FALSE);
				break;
			case (IfTests::NotEqualTo):
				result = (strcmp(string1,string2) != 0 ? TRUE : FALSE);
				break;
		}
	}
	else if (vt1 == VTypes::IntegerData)
	{
		i1 = v1->asInteger();
		i2 = v2->asInteger();
		msg.print(Messenger::Commands, "If Test: var1(%s)=[%i] (%s) var2(%s)=[%i] : %s\n", v1->name(), i1, IfTests::ifTest(ifTest_), v2->name(), i2, result ? "True" : "False");
		switch (ifTest_)
		{
			case (IfTests::EqualTo):
				result = (i1 == i2 ? TRUE : FALSE);
				break;
			case (IfTests::LessThan):
				result = (i1 < i2 ? TRUE : FALSE);
				break;
			case (IfTests::LessThanEqualTo):
				result = (i1 <= i2 ? TRUE : FALSE);
				break;
			case (IfTests::GreaterThan):
				result = (i1 > i2 ? TRUE : FALSE);
				break;
			case (IfTests::GreaterThanEqualTo):
				result = (i1 >= i2 ? TRUE : FALSE);
				break;
			case (IfTests::NotEqualTo):
				result = (i1 != i2 ? TRUE : FALSE);
				break;
		}
	}
	else
	{
		d1 = v1->asDouble();
		d2 = v2->asDouble();
		msg.print(Messenger::Commands, "If Test: var1(%s)=[%f] (%s) var2(%s)=[%f] : %s\n", v1->name(), d1, IfTests::ifTest(ifTest_), v2->name(), d2, result ? "True" : "False");
		// Do comparison
		switch (ifTest_)
		{
			case (IfTests::EqualTo):
				msg.print("Warning: Comparing between floating point values...\n");
				result = (d1 == d2 ? TRUE : FALSE);
				break;
			case (IfTests::LessThan):
				result = (d1 < d2 ? TRUE : FALSE);
				break;
			case (IfTests::LessThanEqualTo):
				result = (d1 <= d2 ? TRUE : FALSE);
				break;
			case (IfTests::GreaterThan):
				result = (d1 > d2 ? TRUE : FALSE);
				break;
			case (IfTests::GreaterThanEqualTo):
				result = (d1 >= d2 ? TRUE : FALSE);
				break;
			case (IfTests::NotEqualTo):
				msg.print("Warning: Comparing between floating point values...\n");
				result = (d1 != d2 ? TRUE : FALSE);
				break;
		}
		msg.print(Messenger::Commands, "If Test: var1(%s)=[%f] (%s) var2(%s)=[%f] : %s\n", v1->name(), d1, IfTests::ifTest(ifTest_), v2->name(), d2, result ? "True" : "False");
	}
	msg.exit("TreeNode::ifEvaluate");
	return result;
}

/*
// Command Variables
*/

// Add variable/constant/expression/path to reference list, given the name
bool TreeNode::addArgument(int argid, Parser::ArgumentForm form)
{
	msg.enter("TreeNode::addArgument");
	Variable *v;
	bool result = TRUE;
	// If argument form wasn't provided, attempt to work it out.
	Parser::ArgumentForm af = (form == Parser::UnknownForm ? parser.argumentForm(argid) : form);
// 	printf("Adding argument '%s', form = %i...\n", parser.argc(argid), af);
	// Now we have the argument form, get/create a suitable variable
	switch (af)
	{
		case (Parser::ConstantForm):
			addConstant(parser.argc(argid));
			break;
		case (Parser::ExpressionForm):
			// Attempt to construct expression
			v = variableList_->addExpression(parser.argc(argid));
			if (v == NULL) result = FALSE;
// 			else printf("Expression added.... %li\n", v);
			args_.add(v);
			break;
		case (Parser::VariableForm):
		case (Parser::VariablePathForm):
			v = variableList_->addPath(parser.argc(argid));
			if (v == NULL) result = FALSE;
			args_.add(v);
			break;
	}
	msg.exit("TreeNode::addArgument");
	return result;
}

// Add constant to reference list
void TreeNode::addConstant(const char *s, bool forcechar)
{
	msg.enter("TreeNode::addConstant");
	Variable *v = variableList_->addConstant(s, forcechar);
	args_.add(v);
	msg.exit("TreeNode::addConstant");
}

// Add integer constant to reference list
void TreeNode::addConstant(int i)
{
	msg.enter("TreeNode::addConstant[int]");
	Variable *v = variableList_->addConstant(i);
	args_.add(v);
	msg.exit("TreeNode::addConstant[int]");
}

// Add real constant to reference list
void TreeNode::addConstant(double d)
{
	msg.enter("TreeNode::addConstant[double]");
	Variable *v = variableList_->addConstant(d);
	args_.add(v);
	msg.exit("TreeNode::addConstant[double]");
}

// Set variable list manually
void TreeNode::setVariableList(VariableList *varlist)
{
	variableList_ = varlist;
}

// Add single argument manually
void TreeNode::addArgument(Variable *v)
{
	args_.add(v);
}

// Add variables to command
bool TreeNode::setArguments(const char *cmdname, const char *specifiers, VariableList *sourcevars)
{
	msg.enter("TreeNode::setArguments");
	bool required = TRUE, repeat = FALSE, failed = FALSE;
	int n, m, argcount, last = -1;
	Variable *var;
	VTypes::DataType vt;
	AssignOps::AssignOp ao;
	Parser::ArgumentForm af;
	argcount = 0;
	n = 0;
	// Store reference to source variablelist
	variableList_ = sourcevars;
	while (specifiers[n] != '\0')
	{
		// Check for lowercase letter (optional argument)
		required = ((specifiers[n] > 90) || (specifiers[n] == '*') ? FALSE : TRUE);
		
		// Move on to next argument.
		argcount ++;

		//printf("Adding variable %c which should have value %s\n", specifiers[n], parser.argc(argcount));
		// Is this a required argument?
		if (argcount > (parser.nArgs() - 1))
		{
			if (required && (!repeat))
			{
				msg.print("Error: '%s' requires argument %i\n", cmdname, argcount);
				failed = TRUE;
				break;
			}
			else break;	// No more arguments, so may as well quit.
		}
		// Go through possible specifiers
		switch (specifiers[n])
		{
			// Formats (delimited)
			case ('f'):
			case ('F'):
				if (!createFormat(parser.argc(argcount), TRUE)) failed = TRUE;
				break;
			// Formats (exact)
			case ('g'):
			case ('G'):
				if (!createFormat(parser.argc(argcount), FALSE)) failed = TRUE;
				break;
			// Delimited (J) / exact (K) format *or* variable
			case ('J'):
			case ('K'):
				if (!parser.wasQuoted(argcount))
				{
					if (!addArgument(argcount)) failed = TRUE;
				}
				else if (!createFormat(parser.argc(argcount), specifiers[n] == 'J' ? TRUE : FALSE)) failed = TRUE;
				break;
			// Discard
			case ('x'):
			case ('X'):
				args_.add(NULL);
				break;
			// String as-is
			case ('s'):
			case ('S'):
				addConstant(parser.argc(argcount), TRUE);
				break;
			// Operators
			case ('O'):
			case ('~'):
			case ('='):
				// Get operator enum
				ao = AssignOps::assignOp(parser.argc(argcount));
				if (ao == AssignOps::nAssignOps)
				{
					msg.print("Error: Unrecognised assignment operator '%s'.\n", parser.argc(argcount));
					failed = TRUE;
					break;
				}
				// Whether we accept the operator we found depends on the specifier
				switch (specifiers[n])
				{
					// 'O' - accept any
					case ('O'):
						break;
					// '=' - accept '=' only
					case ('='):
						if (ao != AssignOps::Equals) 
						{
							msg.print("Error: Expected '=' as argument %i for command '%s'.\n", argcount, cmdname);
							failed = TRUE;
						}
						break;
					// '~' - accept '=' and '+=' only
					case ('~'):
						if ((ao != AssignOps::Equals) && (ao != AssignOps::PlusEquals)) 
						{
							msg.print("Error: Expected '=' or '+=' as argument %i for command '%s'.\n", argcount, cmdname);
							failed = TRUE;
						}
						break;
				}
				// Add operator as an integer variable
				if (!failed) addConstant(ao);
				break;
			// Variable, expression, or constant
			case ('e'):
			case ('E'):
				if (!addArgument(argcount)) failed = TRUE;
				break;
			// Normal, non-expression variable or constant (Q forces constant type to be Character)
			case ('n'):
			case ('N'):
			case ('q'):
			case ('Q'):
				// Check for some kind of variable/path
				af = parser.argumentForm(argcount);
				if (af > Parser::VariablePathForm)
				{
					if ((specifiers[n] == 'q') || (specifiers[n] == 'Q')) addConstant(parser.argc(argcount), TRUE); 
					else addConstant(parser.argc(argcount)); 
				}
				else if (!addArgument(argcount)) failed = TRUE;
				break;
			// Variable
			case ('v'):
			case ('V'):
				// Get form of argument in parser object
				af = parser.argumentForm(argcount);
				if (af == Parser::ExpressionForm)
				{
					msg.print("Error: argument %i to '%s' cannot be an expression (found '%s').\n", argcount, cmdname, parser.argc(argcount));
					failed = TRUE;
				}
				else if (af <= Parser::VariablePathForm)
				{
					if (!addArgument(argcount, af)) failed = TRUE;
				}
				else
				{
					msg.print("Error: '%s' expected a variable for argument %i, but found '%s' instead.\n", cmdname, argcount, parser.argc(argcount));
					failed = TRUE;
				}
				break;
			// Pointer-style variable (that also need to create subvariables)
			case ('A'):
			case ('a'):
			case ('P'):
			case ('p'):
			case ('M'):
			case ('m'):
				switch (specifiers[n])
				{
					case ('A'):
					case ('a'):
						vt = VTypes::AtomData;
						break;
					case ('P'):
					case ('p'):
						vt = VTypes::PatternData;
						break;
					case ('M'):
					case ('m'):
						vt = VTypes::ModelData;
						break;
					default:
						vt = VTypes::nDataTypes;
						break;
				}
				af = parser.argumentForm(argcount);
				if (af > Parser::VariablePathForm)
				{
					msg.print( "Error: '%s' expected a variable, but found '%s' instead.\n", cmdname, parser.argc(argcount));
					failed = TRUE;
					break;
				}
				// Add argument and check return type value
				if (!addArgument(argcount, af)) failed = TRUE;
				else if (args_.last()->item->type() != vt)
				{
					msg.print( "Error: '%s' expected a variable of type '%s', but found '%s' instead which is of type '%s'.\n", cmdname, VTypes::dataType(vt), parser.argc(argcount), VTypes::dataType(args_.last()->item->type()));
					failed = TRUE;
				}
				break;
			// Character variable
			case ('C'):
				af = parser.argumentForm(argcount);
				if (af <= Parser::VariablePathForm)
				{
					if (!addArgument(argcount, af)) failed = TRUE;
					// Must also check return value of variable
					else if (args_.last()->item->type() != VTypes::CharacterData)
					{
						msg.print("Error: '%s' expected a variable of type 'character', but found '%s' instead.\n", cmdname, parser.argc(argcount));
						failed = TRUE;
					}
				}
				else failed = TRUE;
				break;
			// Repeat as many of the last variable type as possible
			case ('*'):
				if (n == 0)
				{
					printf("Internal error: Repeat specifier given to command arguments list without prior specifier.\n");
					failed = TRUE;
				}
				// Set the repeat flag to TRUE, and go back to last parameter type considered
				repeat = TRUE;
				n --;
				// Decrement argcount so we consider again the current argument
				argcount --;
				break;
		}
		// Check for failure
		if (failed) break;
		// Go to next character (if we're not repeating)
		if (!repeat) n++;
	}
	// Are there still unused arguments in the parser?
	if ((argcount < (parser.nArgs() - 1)) && (!failed))
	{
		msg.print("Error: Unexpected argument '%s' given to command '%s'.\n", parser.argc(++argcount), cmdname);
		msg.exit("TreeNode::setArguments");
		return FALSE;
	}
	msg.exit("TreeNode::setArguments");
	return (failed ? FALSE : TRUE);
}

// Return number of arguments given to command
int TreeNode::nArgs()
{
	return args_.nItems();
}

// Execute command
int TreeNode::execute(TreeNode *&c, bool noflow)
{
	// Make sure the current rendersource is up-to-date
	aten.current.rs = (aten.current.m == NULL ? NULL : aten.current.m->renderSource());
	// Check whether to disregard flow control nodes
	if (noflow && (function_ >= Command::CA_BREAK) && (function_ <= Command::CA_TERMINATE))
	{
		msg.print("This command '%s' may change the flow of a CommandList, but flow control has been disallowed.\n", commands.data[c->function()].keyword);
		return Command::Fail;
	}
	return commands.call(function_, c);
}
