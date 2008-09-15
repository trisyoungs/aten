/*
	*** Command
	*** src/command/command.cpp
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

#include "command/command.h"
#include "command/format.h"
#include "variables/accesspath.h"
#include "variables/expression.h"
#include "main/aten.h"
#include "model/model.h"
#include "base/sysfunc.h"

// If Conditions
const char *IfTestStrings[6] = { "eq", "l", "le", "g", "ge", "neq" };
const char *IfTests::ifTest(IfTest i)
{
	return IfTestStrings[i-1];
}
// Variable Operators
const char *AssignOpKeywords[AssignOps::nAssignOps] = { "=", "+=", "-=", "/=", "*=" };
AssignOps::AssignOp AssignOps::assignOp(const char *s)
{
	return (AssignOps::AssignOp) enumSearch("assignment operator", AssignOps::nAssignOps, AssignOpKeywords, s);
}
const char *AssignOps::assignOp(AssignOps::AssignOp ao)
{
	return AssignOpKeywords[ao];
}

// Constructor
Command::Command()
{
	// Private variables
	action_ = CA_ROOTNODE;
	parent_ = NULL;
	function_ = NULL;
	ptr_ = NULL;
	branch_ = NULL;
	format_ = NULL;
	loopActive_ = FALSE;
	variableList_ = NULL;

	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructor
Command::~Command()
{
	if (branch_ != NULL) delete branch_;
	if (format_ != NULL) delete format_;
}

/*
// Command
*/

// Set parent CommandList
void Command::setParent(CommandList *cl)
{
	parent_ = cl;
}

// Get parent CommandList
CommandList *Command::parent()
{
	return parent_;
}

// Get command
CommandAction Command::command()
{
	return action_;
}

// Returns the formatter
Format *Command::format()
{
	return format_;
}

// Delete the associated format
void Command::deleteFormat()
{
	if (format_ != NULL) delete format_;
	format_ = NULL;
}

// Set status of loop
void Command::setLoopActive(bool b)
{
	loopActive_ = b;
}

// Get status of loop
bool Command::isLoopActive()
{
	return loopActive_;
}

// Set iteration count
void Command::setLoopIterations(int n)
{
	loopIterations_ = n;
}

// Get iteration count
int Command::loopIterations()
{
	return loopIterations_;
}

// Increase interation count
void Command::increaseIterations()
{
	loopIterations_ ++;
}

// Returns branch list structure
List<Command> *Command::branch()
{
	return branch_;
}

// Returns first item in branch 
Command *Command::branchCommands()
{
	return (branch_ != NULL ? branch_->first() : NULL);
}

// Set FormatNode pointer variable
void Command::setPointer(Command *f)
{
	ptr_ = f;
}

// Return FormatNode pointer variable
Command *Command::pointer()
{
	return ptr_;
}

// Return variable argument
AccessPath *Command::arg(int argno)
{
	AccessPath *ap = args_[argno];
	return ap;
}

// Return argument as character
const char *Command::argc(int argno)
{
	AccessPath *ap = args_[argno];
	printf("argc pointer is %li\n",ap);
	return (ap == NULL ?  "NULL" : ap->asCharacter());
}

// Return argument as integer
int Command::argi(int argno)
{
	AccessPath *ap = args_[argno];
	return (ap == NULL ?  0 : ap->asInteger());
}

// Return argument as double
double Command::argd(int argno)
{
	AccessPath *ap = args_[argno];
	return (ap == NULL ? 0.0 : ap->asDouble());
}

// Return argument as float
float Command::argf(int argno)
{
	return (float) argd(argno);
}

// Return argument as bool
bool Command::argb(int argno)
{
	AccessPath *ap = args_[argno];
	return (ap == NULL ? -1 : ap->asBool());
}

// Return argument as pointer
void *Command::argp(int argno, VTypes::DataType dt)
{
	AccessPath *ap = args_[argno];
	return (ap == NULL ? NULL : ap->asPointer(dt));
}

// // Return argument as pattern pointer
// Pattern *Command::argp(int argno)
// {
// 	AccessPath *ap = args_[argno];
// 	if (ap == NULL) return NULL;
// 	return (Pattern*) rv->item->asPointer(VTypes::PatternData);
// }

// // Return argument as grid pointer
// Grid *Command::argg(int argno)
// {
// 	Refitem<Variable,Variable*> *rv = args_[argno];
// 	if (rv == NULL) return NULL;
// 	return (Grid*) rv->item->asPointer(VTypes::GridData);
// }

// // Return argument as model pointer
// Model *Command::argm(int argno)
// {
// 	Refitem<Variable,Variable*> *rv = args_[argno];
// 	if (rv == NULL) return NULL;
// 	return (Model*) rv->item->asPointer(VTypes::ModelData);
// }

// // Return argument as PatternBound pointer
// PatternBound *Command::argpb(int argno)
// {
// 	Refitem<Variable,Variable*> *rv = args_[argno];
// 	if (rv == NULL) return NULL;
// 	else if ((rv->item->type() < VTypes::BondData) || (rv->item->type() > VTypes::TorsionData)) msg.print("Command can't convert variable '%s' into a PatternBound\n.", rv->item->name());
// 	else return (PatternBound*) rv->item->asPointer(VTypes::BondData);
// 	return NULL;
// }

// // Return argument as ForcefieldAtom pointer
// ForcefieldAtom *Command::argffa(int argno)
// {
// 	Refitem<Variable,Variable*> *rv = args_[argno];
// 	if (rv == NULL) return NULL;
// 	return (ForcefieldAtom*) rv->item->asPointer(VTypes::AtomtypeData);
// }

// Returns whether argument 'n' was provided
bool Command::hasArg(int argno)
{
	return ((argno+1) > args_.nItems() ? FALSE : TRUE);
}

// Return variable type of argument
VTypes::DataType Command::argt(int argno)
{
	AccessPath *ap = args_[argno];
	return (ap == NULL ? VTypes::NoData : ap->returnType());
}

// Set command and function
void Command::setCommand(CommandAction ca)
{
	action_ = ca;
	function_ = CA_data[ca].function;
}

// Print data variables
void Command::printArgs()
{
	msg.enter("Command::printArgs");
	int i = 0;
	for (AccessPath *ap = args_.first(); ap != NULL; ap = ap->next)
	{
		printf("%2i %20li", i, ap);
		//printf("%12s [%10s]", v->name(), VTypes::dataType(v->type()));
		if (ap->returnType() < VTypes::AtomData) printf("%20s\n", ap->asCharacter());
 		else printf("%li\n", ap->asPointer(ap->returnType()));
		i++;
	}
	msg.exit("Command::printArgs");
}

// Return arguments as Vec3<double>
Vec3<double> Command::arg3d(int i)
{
	msg.enter("Command::arg3d");
        static Vec3<double> result;
        if (i > (args_.nItems()-3)) printf("Command::arg3d - Starting point too close to end of argument list.\n");
        result.set(args_[i]->asDouble(), args_[i+1]->asDouble(), args_[i+2]->asDouble());
	msg.exit("Command::arg3d");
        return result;
}

// Return arguments as Vec3<float>
Vec3<float> Command::arg3f(int i)
{
	msg.enter("Command::arg3f");
        static Vec3<float> result;
        if (i > (args_.nItems()-3)) printf("Command::arg3f - Starting point too close to end of argument list.\n");
        result.set(args_[i]->asFloat(), args_[i+1]->asFloat(), args_[i+2]->asFloat());
	msg.exit("Command::arg3f");
        return result;
}

// Return arguments as Vec3<int>
Vec3<int> Command::arg3i(int i)
{
	msg.enter("Command::arg3i");
	static Vec3<int> result;
	if (i > (args_.nItems()-3)) printf("Command::arg3i - Starting point too close to end of argument list.\n");
        result.set(args_[i]->asInteger(), args_[i+1]->asInteger(), args_[i+2]->asInteger());
	msg.exit("Command::arg3i");
	return result;
}

// Create branch
List<Command> *Command::createBranch()
{
	msg.enter("Command::createBranch");
	if (branch_ != NULL) printf("Command::createBranch <<<< Already has a branch >>>>\n");
	branch_ = new List<Command>;
	msg.exit("Command::createBranch");
	return branch_;	// Get return value as double
	double asDouble();
}

// Create branch
bool Command::createFormat(const char *s, bool delimited)
{
	msg.enter("Command::createFormat");
	bool result = FALSE;
	if (format_ != NULL) printf("Command::createFormat <<<< Already has a format >>>>\n");
	else
	{
		format_ = new Format;
		result = format_->create(s, *variableList_, delimited);
	}
	msg.exit("Command::createFormat");
	return result;
}

// Set if condition test
bool Command::setIfTest(const char *s)
{
	msg.enter("Command::setIfTest");
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
	msg.exit("Command::setIfTest");
	return result;
}

// Evaluate condition
bool Command::ifEvaluate()
{
	msg.enter("Command::ifEvaluate");
	bool result;
	static AccessPath *ap1, *ap2;
	static char string1[512], string2[512];
	static double d1, d2;
	VTypes::DataType vt1, vt2;
	static int i1, i2;
	ap1 = args_[0];
	ap2 = args_[2];
	// Determine how to do the comparison
	vt1 = ap1->returnType();
	vt2 = ap2->returnType();
	if (vt2 > vt1) vt1 = vt2;
	if (vt1 == VTypes::CharacterData)
	{
		strcpy(string1, ap1->asCharacter());
		strcpy(string2, ap2->asCharacter());
		msg.print(Messenger::Commands, "If Test: var1(%s)=[%s] (%s) var2(%s)=[%s]\n", ap1->originalPath(), string1, IfTests::ifTest(ifTest_), ap2->originalPath(), string2);

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
		i1 = ap1->asInteger();
		i2 = ap2->asInteger();
		msg.print(Messenger::Commands, "If Test: var1(%s)=[%i] (%s) var2(%s)=[%i] : %s\n", ap1->originalPath(), i1, IfTests::ifTest(ifTest_), ap2->originalPath(), i2, result ? "True" : "False");
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
		d1 = ap1->asDouble();
		d2 = ap2->asDouble();
		msg.print(Messenger::Commands, "If Test: var1(%s)=[%f] (%s) var2(%s)=[%f] : %s\n", ap1->originalPath(), d1, IfTests::ifTest(ifTest_), ap2->originalPath(), d2, result ? "True" : "False");
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
		msg.print(Messenger::Commands, "If Test: var1(%s)=[%f] (%s) var2(%s)=[%f] : %s\n", ap1->originalPath(), d1, IfTests::ifTest(ifTest_), ap2->originalPath(), d2, result ? "True" : "False");
	}
	//printf("IF TEST : [%s] [%i] [%s] = %s\n",value1,type,value2,(result ? "TRUE" : "FALSE"));
	msg.exit("Command::ifEvaluate");
	return result;
}

/*
// Command Variables
*/

// Add variable to reference list, given the name (minus the 'dollar')
bool Command::addArgument(const char *varname, VariableList *sourcevars, Parser::ArgumentForm form)
{
	msg.enter("Command::addArgument");
	bool result;
	variableList_ = sourcevars;
	// If argument form wasn't provided, attempt to work it out.
	Parser::ArgumentForm af = (form == Parser::UnknownForm ? parser.argumentForm(varname) : form);
	printf("Adding argument '%s', form = %i...\n", varname, form);
	// Now we have the argument form, attempt to create an access path from the string
	AccessPath *ap = args_.add();
	// We must remove the leading '$' from variable/path-type arguments
	result = ap->setPath(af <= Parser::VariablePathForm ? varname : &varname[1], sourcevars, form);
	msg.exit("Command::addArgument");
	return result;
}

// Add constant to reference list
void Command::addConstant(const char *s, VariableList *sourcevars, bool forcechar)
{
	msg.enter("Command::addConstant");
	printf("Adding constant '%s'...\n", s);
	variableList_ = sourcevars;
	Variable *v = variableList_->addConstant(s, forcechar);
	printf("...constant value set is '%s'\n", v->asCharacter());
	AccessPath *ap = args_.add();
	ap->setPath(v);
	msg.exit("Command::addConstant");
}

// Add constant to reference list
void Command::addConstant(int i, VariableList *sourcevars)
{
	msg.enter("Command::addConstant[int]");
	printf("Adding constant integer '%d'...\n", i);
	variableList_ = sourcevars;
	Variable *v = variableList_->addConstant(i);
	AccessPath *ap = args_.add();
	ap->setPath(v);
	msg.exit("Command::addConstant[int]");
}

// Add variables to command
bool Command::setArguments(const char *cmdname, const char *specifiers, VariableList *sourcevars)
{
	msg.enter("Command::setArguments");
	bool required = TRUE, repeat = FALSE;
	int n, m, argcount, last = -1;
	Variable *var;
	VTypes::DataType vt;
	AssignOps::AssignOp ao;
	Parser::ArgumentForm af;
	static char arg[512];
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

		printf("Adding variable %c which should have value %s\n", specifiers[n], parser.argc(argcount));
		// Is this a required argument?
		if (argcount > (parser.nArgs() - 1))
		{
			if (required && (!repeat))
			{
				msg.print("Error: '%s' requires argument %i\n", cmdname, argcount);
				msg.exit("Command::setArguments");
				return FALSE;
			}
			else break;	// No more arguments, so may as well quit.
		}
		strcpy(arg,parser.argc(argcount));
		// Go through possible specifiers
		switch (specifiers[n])
		{
			// Formats (delimited)
			case ('f'):
			case ('F'):
				if (!createFormat(arg, TRUE)) return FALSE;
				break;
			// Formats (exact)
			case ('g'):
			case ('G'):
				if (!createFormat(arg, FALSE)) return FALSE;
				break;
			// Delimited (J) / exact (K) format *or* variable
			case ('J'):
			case ('K'):
// 				if (!parser.wasQuoted(argcount))
// 				{
// 					// See if it has been declared
// 					var = sourcevars.get(&arg[1]);
// 					if (var == NULL)
// 					{
// 						msg.print("Error: Variable '%s' has not been declared.\n", &arg[1]);
// 						return FALSE;
// 					}
// 					else args_.add(var);
// 				}
// 				else
				if (!createFormat(arg, specifiers[n] == 'J' ? TRUE : FALSE)) return FALSE;
				break;
			// Discard
			case ('x'):
			case ('X'):
				args_.add();
				break;
			// String as-is
			case ('s'):
			case ('S'):
				addConstant(arg, sourcevars, TRUE);
				break;
			// Operators
			case ('O'):
			case ('~'):
			case ('='):
				// Get operator enum
				ao = AssignOps::assignOp(&arg[0]);
				if (ao == AssignOps::nAssignOps)
				{
					msg.print("Error: Unrecognised assignment operator '%s'.\n", &arg[0]);
					msg.exit("Command::setArguments");
					return FALSE;
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
							msg.exit("Command::setArguments");
							return FALSE;
						}
						break;
					// '~' - accept '=' and '+=' only
					case ('~'):
						if ((ao != AssignOps::Equals) && (ao != AssignOps::PlusEquals)) 
						{
							msg.print("Error: Expected '=' or '+=' as argument %i for command '%s'.\n", argcount, cmdname);
							msg.exit("Command::setArguments");
							return FALSE;
						}
						break;
				}
				// Add operator as an integer variable
				addConstant(ao, sourcevars);
// 				args_.add(parent_->variables.addConstant(ao));
				break;
			// Variable, expression, or constant
			case ('e'):
			case ('E'):
				if (!addArgument(arg, sourcevars))
				{
					msg.exit("Command::setArguments");
					return FALSE;
				}
				break;
			// Normal, non-expression variable or constant (Q forces constant type to be Character)
			case ('n'):
			case ('N'):
			case ('q'):
			case ('Q'):
				// Check for some kind of variable/path
				if (arg[0] == '$')
				{
					if (!addArgument(arg, sourcevars))
					{
						msg.print("Error: Variable '%s' has not been declared.\n", &arg[1]);
						msg.exit("Command::setArguments");
						return FALSE;
					}
				}
				else
				{
					if ((specifiers[n] == 'q') || (specifiers[n] == 'Q')) addConstant(arg, sourcevars, TRUE); 
					else addConstant(arg, sourcevars); 
				}
				break;
			// Variable
			case ('v'):
			case ('V'):
				// Get form of argument in parser object
				af = parser.argumentForm(argcount);
				if (af == Parser::ExpressionForm)
				{
					msg.print("Error: argument %i to '%s' cannot be an expression (found '%s').\n", argcount, cmdname, arg);
					msg.exit("Command::setArguments");
					return FALSE;
				}
				else if (af <= Parser::VariablePathForm)
				{
					if (!addArgument(arg, sourcevars, af))
					{
						//msg.print( "Error: Variable '%s' has not been declared.\n", &arg[1]);
						msg.exit("Command::setArguments");
						return FALSE;
					}
				}
				else
				{
					msg.print("Error: '%s' expected a variable for argument %i, but found '%s' instead.\n", cmdname, argcount, arg);
					return FALSE;
				}
				break;
			// Pointer-style variable (that also need to create subvariables)
			case ('A'):
			case ('a'):
			case ('B'):
			case ('b'):
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
					case ('B'):
					case ('b'):
						vt = VTypes::BondData;
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
				if (af <= Parser::VariablePathForm)
				{
					if (!addArgument(arg, sourcevars, af))
					{
						msg.exit("Command::setArguments");
						return FALSE;
					}
					// Must also check return value of variable
				}
				else
				{
					msg.print( "Error: '%s' expected a variable of type '%s', but found '%s' instead.\n", cmdname, VTypes::dataType(vt), arg);
					return FALSE;
				}
				break;
			// Character variable
			case ('C'):
				af = parser.argumentForm(argcount);
				if (af <= Parser::VariablePathForm)
				{
					if (!addArgument(arg, sourcevars, af))
					{
						msg.exit("Command::setArguments");
						return FALSE;
					}
					// Must also check return value of variable
					if (args_.last()->returnType() != VTypes::CharacterData)
					{
						msg.print("Error: '%s' expected a variable of type 'character', but found '%s' instead.\n", cmdname, arg);
						msg.exit("Command::setArguments");
						return FALSE;
					}
				}
				else
				{
					msg.exit("Command::setArguments");
					return FALSE;
				}
				break;
			// Repeat as many of the last variable type as possible
			case ('*'):
				if (n == 0)
				{
					printf("Internal error: Repeat specifier given to command arguments list without prior specifier.\n");
					msg.exit("Command::setArguments");
					return FALSE;
				}
				// Set the repeat flag to TRUE, and go back to last parameter type considered
				repeat = TRUE;
				n --;
				// Decrement argcount so we consider again the current argument
				argcount --;
				break;
		}
		// Go to next character (if we're not repeating)
		if (!repeat) n++;
	}
	// Are there still unused arguments in the parser?
	if (argcount < (parser.nArgs() - 1))
	{
		msg.print("Error: Unexpected argument '%s' given to command '%s'.\n", parser.argc(++argcount), cmdname);
		msg.exit("Command::setArguments");
		return FALSE;
	}
	msg.exit("Command::setArguments");
	return TRUE;
}

// Return number of arguments given to command
int Command::nArgs()
{
	return args_.nItems();
}

// Execute command
int Command::execute(Command *&c)
{
	// Make sure the current rendersource is up-to-date
	aten.current.rs = (aten.current.m == NULL ? NULL : aten.current.m->renderSource());
	return CALL_COMMAND(CA_data[action_],function_)(c, aten.current);
}

