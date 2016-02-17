/*
	*** Tree Node
	*** src/parser/treenode.cpp
	Copyright T. Youngs 2007-2016

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
#include "parser/variable.h"
#include "parser/variablenode.h"
#include "templates/namemap.h"
#include "base/atom.h"

ATEN_USING_NAMESPACE

// Constructors
TreeNode::TreeNode() : ListItem<TreeNode>()
{
	// Private variables
	returnType_ = VTypes::NoData;
	returnsArray_ = false;
	readOnly_ = true;
	parent_ = NULL;
	nodeType_ = TreeNode::BasicNode;

	// Public variables
	nextArgument = NULL;
	prevArgument = NULL;
}

// Destructor
TreeNode::~TreeNode()
{
}

// Copy data
void TreeNode::copy(TreeNode* source)
{
	nodeType_ = source->nodeType_;
	parent_ = source->parent_;
	args_ = source->args_;
	returnType_ = source->returnType_;
	readOnly_ = source->readOnly_;
	returnsArray_ = source->returnsArray_;
}

// Retrieve node type
TreeNode::NodeType TreeNode::nodeType() const
{
	return nodeType_;
}

// Set parent 
void TreeNode::setParent(Tree* parent)
{
	parent_ = parent;
}

// Retrieve parent
Tree* TreeNode::parent() const
{
	return parent_;
}

// Sets the content type of the variable
void TreeNode::setReturnType(VTypes::DataType dt)
{
	returnType_ = dt;
}

// Returns content type of the variable
VTypes::DataType TreeNode::returnType() const
{
	return returnType_;
}

// Return readonly status
bool TreeNode::readOnly() const
{
	return readOnly_;
}

// Set the readonly status of the variable to true
void TreeNode::setReadOnly()
{
	readOnly_ = true;
}

// Set whether an array of values is returned
void TreeNode::setReturnsArray(bool b)
{
	returnsArray_ = b;
}

// Return whether an array of values is returned
bool TreeNode::returnsArray() const
{
	return returnsArray_;
}

// Return number of arguments currently assigned to node
int TreeNode::nArgs() const
{
	return args_.nItems();
}

// Return datatype of nth argument
VTypes::DataType TreeNode::argType(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argType : Argument index %i is out of range (node = %p).\n", i, this);
		return VTypes::NoData;
	}
	return args_[i]->item->returnType();
}


// Set argument specified
bool TreeNode::setArg(int i, ReturnValue& rv)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::setArg : Argument index %i is out of range (node = %p).\n", i, this);
		return false;
	}
	// Check readonly attribute of argument
	if (args_[i]->item->readOnly())
	{
		args_[i]->item->nodePrint(0);
		Messenger::print("Argument %i is read-only and can't be set.", i);
		return false;
	}
	return args_[i]->item->set(rv);
}

// Return whether argument i was given
bool TreeNode::hasArg(int i)
{
	return (i < args_.nItems());
}

// Add list of arguments formas as a plain List<TreeNode>, beginning from supplied list head
void TreeNode::addListArguments(TreeNode* leaf)
{
	for (TreeNode* node = leaf; node != NULL; node = node->next) args_.add(node);
}

// Add list of arguments formed as a linked TreeNode list
void TreeNode::addJoinedArguments(TreeNode* lastleaf)
{
	/*
	The supplied leaf may be a single node, or it may be a list of nodes beginning at the *last* node (this is the case if Joined by the parser)
	Therefore, must walk backwards through the list first to get to the head...
	*/
	TreeNode* first;
	for (first = lastleaf; first != NULL; first = first->prevArgument) if (first->prevArgument == NULL) break;
	for (TreeNode* node = first; node != NULL; node = node->nextArgument) args_.add(node);
}

// Add multiple arguments to node
void TreeNode::addArguments(int nargs, ...)
{
	// Create variable argument parser
	va_list vars;
	va_start(vars,nargs);
	// Add arguments in the order they were provided
	for (int n=0; n<nargs; n++) addArgument(va_arg(vars, TreeNode*));
	va_end(vars);
	Messenger::print(Messenger::Parse,"Node %p now has %i arguments.", this, args_.nItems());
}

// Add multiple arguments to node
void TreeNode::addArgument(TreeNode* arg)
{
	args_.add(arg);
}

// Check validity of supplied arguments
bool TreeNode::checkArguments(const char* argList, const char* funcname)
{
	Messenger::enter("TreeNode::checkArguments");
	Messenger::print(Messenger::Parse, "Checking the %i argument(s) given to function '%s'...", args_.nItems(), funcname);
	const char* c = NULL, *altargs = argList;
	Messenger::print(Messenger::Parse, "...argument list is [%s]", altargs);
	char upc;
	int count = 0, ngroup = -1, repeat = 0;
	bool optional = false, requirevar = false, result, cluster = false, array = false, reset = true;
	VTypes::DataType rtype;
	// If the argument list begins with '_', arguments will have already been checked and added elsewhere...
	if (*altargs == '_')
	{
		Messenger::exit("TreeNode::checkArguments");
		return true;
	}
	// Search for an alternative set of arguments
	result = true;
	do
	{
		if (reset)
		{
			c = altargs;
			if (*c == '|') ++c;
			altargs = strchr(c, '|');
			repeat = 0;
			cluster = false;
			array = false;
			count = 0;
			reset = false;
		}
		if (*c == '\0') break;
		upc = *c;
		if (*c == '|')
		{
			// This is the start of a new set of argument specifiers - does the current set of arguments 'fit'?
			if (args_.nItems() != count)
			{
// 				printf("Number of arguments (%i) doesn't match number in this set (%i) - next!\n", args_.nItems(), count);
				reset = true;
				continue;
			}
			Messenger::exit("TreeNode::checkArguments");
			return true;
		}
		// Retain previous information if this is a repeat, but make it an optional argument
		if (*c == '*') optional = true;
		else if (repeat == 0)
		{
			// Reset modifier values
			requirevar = false;
			array = false;
			repeat = 1;
			// Find next alpha character (and accompanying modifiers)
			while (!isalpha(*c) && (*c != '|') && (*c != '\0') )
			{
				switch (*c)
				{
					// Require variable
					case ('^'):	requirevar = true; break;
					// Clustering
					case ('['):	cluster = true; ngroup = 0; break;
					case (']'):	cluster = false; ngroup = -1; break;
					// Require array
					case ('&'):	array = true; break;
					case ('2'):
					case ('3'):
					case ('4'):
					case ('5'):
					case ('6'):
					case ('7'):
					case ('8'):
					case ('9'):	repeat = *c - '0'; break;
					default:
						printf("BAD CHARACTER (%c) IN COMMAND ARGUMENTS\n", *c);
						break;
				}
				c++;
			}
			if (*c == '|')
			{
				// This is the start of a new set of argument specifiers - does the current set of arguments 'fit'?
				if (args_.nItems() != count)
				{
					printf("Number of arguments (%i) doesn't match number in this set (%i) - next!\n", args_.nItems(), count);
					reset = true;
					continue;
				}
				Messenger::exit("TreeNode::checkArguments");
				return true;
			}
			// Convert character to upper case if necessary
			if ((*c > 96) && (*c < 123))
			{
				upc = *c - 32;
				optional = true;
			}
			else
			{
				upc = *c;
				optional = false;
			}
		}
		if (*c == '\0') break;
		Messenger::print(Messenger::Parse,"...next/current argument token is '%c', opt=%s, reqvar=%s, repeat=%i, ngroup=%i", *c, optional ? "true" : "false", requirevar ? "true" : "false", repeat, ngroup);
		// If we have gone over the number of arguments provided, is this an optional argument?
		if (count >= args_.nItems())
		{
			if (!optional)
			{
				// If an alternative argument list is present, check this before we fail...
				if (altargs != NULL) { reset = true; continue; }
				Messenger::print("Error: The function '%s' requires argument %i.", funcname, count+1);
// 				Messenger::print("       Command syntax is '%s(%s)'.", funcname, Commands::data[function_].argText);
				Messenger::exit("TreeNode::checkArguments");
				return false;
			}
			else if (cluster && (ngroup != 0))
			{
				Messenger::print("Error: The optional argument %i to function '%s' is part of a group and must be specified.", count+1, funcname);
// 				Messenger::print("       Command syntax is '%s(%s)'.", funcname, argList);
				Messenger::exit("TreeNode::checkArguments");
				return false;
			}
			else
			{
				Messenger::exit("TreeNode::checkArguments");
				return true;
			}
		}
		// Check argument type
		rtype = argType(count);
		result = true;
		switch (upc)
		{
			// Atom/Id		(IntegerData, AtomData)
			case ('A'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::AtomData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an int or an Atom.", count+1, funcname);
					result = false;
				}
				break;
			// Boolean		(Any Except NoData)
			case ('B'):
				if (rtype == VTypes::NoData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must return something!", count+1, funcname);
					result = false;
				}
				break;
			// Character		(StringData)
			case ('C'):
				if (rtype != VTypes::StringData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a character string.", count+1, funcname);
					result = false;
				}
				break;	
			// Double		(DoubleData)
			case ('D'):
				if (rtype != VTypes::DoubleData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a double.", count+1, funcname);
					result = false;
				}
				break;
			// Element		(StringData, DoubleData, IntegerData, AtomData, ElementData)
			case ('E'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::AtomData) && (rtype != VTypes::DoubleData) && (rtype != VTypes::StringData) && (rtype != VTypes::ElementData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an int, double, string or an Atom.", count+1, funcname);
					result = false;
				}
				break;
			// Forcefield/ID/Name	(ForcefieldData, StringData, IntegerData)
			case ('F'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::ForcefieldData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an int, a Forcefield or a string.", count+1, funcname);
					result = false;
				}
				break;
			// Grid/ID/Name	(GridData, StringData, IntegerData)
			case ('G'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::GridData) ) //&& (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an int or a Grid.", count+1, funcname);
					result = false;
				}
				break;
			// Bond		(BondData)
			case ('H'):
				if (rtype != VTypes::BondData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a Bond.", count+1, funcname);
					result = false;
				}
				break;
			// Integer		(IntegerData)
			case ('I'):
				if (rtype != VTypes::IntegerData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an int.", count+1, funcname);
					result = false;
				}
				break;
			// Atom			(AtomData)
			case ('J'):
				if (rtype != VTypes::AtomData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an Atom.", count+1, funcname);
					result = false;
				}
				break;
			// UnitCell		(CellData)
			case ('K'):
				if (rtype != VTypes::CellData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a UnitCell.", count+1, funcname);
					result = false;
				}
				break;
			// Model/ID/Name	(ModelData, StringData, IntegerData)
			case ('M'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::ModelData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an int, a Model or a string.", count+1, funcname);
					result = false;
				}
				break;
			// Number		(IntegerData, DoubleData)
			case ('N'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::DoubleData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a number.", count+1, funcname);
					result = false;
				}
				break;
			// FFAtom (ForcefieldAtomData)
			case ('O'):
				if (rtype != VTypes::ForcefieldAtomData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an FFAtom.", count+1, funcname);
					result = false;
				}
				break;
			// Pattern/ID/Name	(PatternData, StringData, IntegerData)
			case ('P'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::PatternData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be an int, a Patternor a string.", count+1, funcname);
					result = false;
				}
				break;
			// Any Simple		(IntegerData, RealData, StringData)
			case ('S'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::DoubleData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a number or a string.", count+1, funcname);
					result = false;
				}
				break;
			// Exact Simple		(IntegerData, StringData)
			case ('T'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a number or a string.", count+1, funcname);
					result = false;
				}
				break;
			// Vector		(VectorData)
			case ('U'):
				if (rtype != VTypes::VectorData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a vector.", count+1, funcname);
					result = false;
				}
				break;	
			// Variable of any type (but not a path)
			case ('V'):
				if (argNode(count)->nodeType() != TreeNode::VarWrapperNode)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a variable of some kind.", count+1, funcname);
					result = false;
				}
				break;
			// Atom/Vector		(AtomData, VectorData)
			case ('W'):
				if ((rtype != VTypes::AtomData) && (rtype != VTypes::VectorData))
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a vector or an Atom.", count+1, funcname);
					result = false;
				}
				break;
			// Pointer		(Any pointer (void*) object)
			case ('X'):
				if (rtype < VTypes::AtomData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a reference of some kind.", count+1, funcname);
					result = false;
				}
				break;
			// Widget		(WidgetData)
			case ('Y'):
				if (rtype != VTypes::WidgetData)
				{
					if (altargs != NULL) { reset = true; continue; }
					Messenger::print("Argument %i to function '%s' must be a widget.", count+1, funcname);
					result = false;
				}
				break;
			// Any
			case ('Z'):
				break;
			// Modifiers
			case ('*'):
			case ('^'):
			case ('['):
			case (']'):
			case ('|'):
			case ('&'):
			case ('2'):
			case ('3'):
			case ('4'):
			case ('5'):
			case ('6'):
			case ('7'):
			case ('8'):
			case ('9'):
				break;
			// Default (error)
			default:
				printf("Unrecognised argument specifier '%c'.\n", upc);
				return false;
		}
		// Was this argument requested to be a modifiable variable value?
		if (requirevar && argNode(count)->readOnly())
		{
			Messenger::print("Argument %i to function '%s' must be a variable and not a constant.", count+1, funcname);
			result = false;
		}
		// Was this argument requested to be an array (*not* an array element)?
		if (array)
		{
			if (argNode(count)->nodeType() != TreeNode::VarWrapperNode)
			{
				Messenger::print("Argument %i to function '%s' must be an array.", count+1, funcname);
				result = false;
			}
			Variable* v = ((VariableNode*) argNode(count))->variable();
			if (v->nodeType() != TreeNode::ArrayVarNode)
			{
				Messenger::print("Argument %i to function '%s' must be an array.", count+1, funcname);
				result = false;
			}
			else if (((VariableNode*) argNode(count))->arrayIndex() != NULL)
			{
				Messenger::print("Argument %i to function '%s' must be an array and not an array element.", count+1, funcname);
				result = false;
			}
		}
		// Check for failure
		if (!result) break;
		if ((upc != '*') && (repeat == 1)) c++;
		if (cluster) ngroup++;
		repeat--;
		count++;
	} while (*c != '\0');
	// End of the argument specification - do we still have arguments left over in the command?
	if (result && (args_.nItems() > count))
	{
		Messenger::print("Error: %i extra arguments given to function '%s'.", args_.nItems()-count, funcname);
		Messenger::exit("TreeNode::checkArguments");
		return false;
	}
	Messenger::exit("TreeNode::checkArguments");
	return result;
}

// Return (execute) argument specified
bool TreeNode::arg(int i, ReturnValue& rv)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::arg : Argument index %i is out of range (node = %p).\n", i, this);
		return false;
	}
	return args_[i]->item->execute(rv);
}

// Return (execute) argument specified as a bool
bool TreeNode::argb(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argb : Argument index %i is out of range (node = %p).\n", i, this);
		return false;
	}
	static ReturnValue rv;
	bool success;
	bool result;
	if (!args_[i]->item->execute(rv)) Messenger::print("Couldn't retrieve argument %i.", i+1);
	result = (rv.asInteger(success) > 0);
	if (!success) Messenger::print("Couldn't cast argument %i into an integer.", i+1);
	return result;
}

// Return (execute) argument specified as an integer
int TreeNode::argi(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argi : Argument index %i is out of range (node = %p).\n", i, this);
		return false;
	}
	static ReturnValue rv;
	bool success;
	int result = 0;
	if (!args_[i]->item->execute(rv)) Messenger::print("Couldn't retrieve argument %i.", i+1);
	result = rv.asInteger(success);
	if (!success) Messenger::print("Couldn't cast argument %i into an integer.", i+1);
	return result;
}

// Return (execute) argument specified as an atomic number
short int TreeNode::argz(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argz : Argument index %i is out of range (node = %p).\n", i, this);
		return false;
	}
	static ReturnValue rv;
	Atom* atm;
	short int result = 0;
	Element* elem;
	if (!args_[i]->item->execute(rv)) Messenger::print("Couldn't retrieve argument %i.", i+1);
	switch (rv.type())
	{
		case (VTypes::IntegerData):
			result = rv.asInteger();
			break;
		case (VTypes::DoubleData):
			result = (short int) floor(rv.asDouble() + 0.15);
			break;
		case (VTypes::StringData):
			result = Elements().find(rv.asString(), prefs.zMapType());
			break;
		case (VTypes::AtomData):
			atm = (Atom*) rv.asPointer(VTypes::AtomData);
			result = atm == NULL ? 0 : atm->element();
			break;
		case (VTypes::ElementData):
			// Check pointer data first....
			elem = (Element*) rv.asPointer(VTypes::ElementData);
			result = elem == 0 ? 0 : elem->z;
			break;
		default:
			Messenger::print("Couldn't cast argument %i (%s) into an atomic number.", i+1, VTypes::aDataType(rv.type()));
			break;
	}
	return result;
}

// Return (execute) argument specified as a double
double TreeNode::argd(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argd : Argument index %i is out of range (node = %p).\n", i, this);
		return false;
	}
	static ReturnValue rv;
	bool success;
	double result = 0.0;
	if (!args_[i]->item->execute(rv)) Messenger::print("Couldn't retrieve argument %i.", i+1);
	result = rv.asDouble(success);
	if (!success) Messenger::print("Couldn't cast argument %i into a real.", i+1);
	return result;
}

// Return (execute) argument specified as a GLFloat
GLfloat TreeNode::argGLf(int i)
{
	return (GLfloat) argd(i);
}

// Return (execute) argument specified as a character
QString TreeNode::argc(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argc : Argument index %i is out of range (node = %p).\n", i, this);
		return QString();
	}
	static ReturnValue rv[MAXNODEARGS];
	bool success;
	QString result;
	if (!args_[i]->item->execute(rv[i])) Messenger::print("Couldn't retrieve argument %i.", i+1);
	result = rv[i].asString(success);
	if (!success) Messenger::print("Couldn't cast argument %i into a character.", i+1);
	return result;
}

// Return (execute) argument specified as a vector
Vec3<double> TreeNode::argv(int i)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argv : Argument index %i is out of range (node = %p).\n", i, this);
		return false;
	}
	// For safety, check that the argument requested is actually a vector
	if (argType(i) != VTypes::VectorData)
	{
		printf("Internal Error: Tried to retrieve a vector from an incompatible argument.\n");
		return Vec3<double>();
	}
	static ReturnValue rv;
	bool success;
	if (!args_[i]->item->execute(rv)) Messenger::print("Couldn't retrieve argument %i.", i+1);
	return rv.asVector(success);
}

// Return (execute) argument specified as a pointer
void *TreeNode::argp(int i, VTypes::DataType type)
{
	if ((i < 0) || (i >= args_.nItems()))
	{
		printf("TreeNode::argp : Argument index %i is out of range (node = %p).", i, this);
		return NULL;
	}
	static ReturnValue rv;
	bool success;
	void *result = NULL;
	if (!args_[i]->item->execute(rv)) Messenger::print("Couldn't retrieve argument %i.", i+1);
	result = rv.asPointer(type, success);
	if (!success) Messenger::print("Couldn't cast argument %i into a pointer of type '%s'.", i+1, VTypes::dataType(type));
	return result;
}

// Return (execute) argument triplet specified
Vec3<double> TreeNode::arg3d(int i)
{
	if ((i < 0) || (i > (args_.nItems()-3)))
	{
		printf("TreeNode::arg3d : Argument index %i is out of range for a triplet (node = %p).\n", i, this);
		return Vec3<double>();
	}
	Vec3<double> result;
	result.set(argd(i), argd(i+1), argd(i+2));
	return result;
}

// Return (execute) argument triplet specified
Vec3<int> TreeNode::arg3i(int i)
{
	if ((i < 0) || (i > (args_.nItems()-3)))
	{
		printf("TreeNode::arg3i : Argument index %i is out of range for a triplet (node = %p).\n", i, this);
		return Vec3<int>();
	}
	Vec3<int> result;
	result.set(argi(i), argi(i+1), argi(i+2));
	return result;
}

// Return (execute) argument triplet specified
Vec3<GLfloat> TreeNode::arg3GLf(int i)
{
	if ((i < 0) || (i > (args_.nItems()-3)))
	{
		printf("TreeNode::arg3GLf : Argument index %i is out of range for a triplet (node = %p).\n", i, this);
		return Vec3<GLfloat>();
	}
	Vec3<GLfloat> result;
	result.set(argGLf(i), argGLf(i+1), argGLf(i+2));
	return result;
}

// Return the TreeNode corresponding to the argument, rather than executing it
TreeNode* TreeNode::argNode(int i)
{
	if ((i < 0) || (i > args_.nItems()))
	{
		printf("TreeNode::argNode : Argument index %i is out of range for returning the argument node (node = %p).\n", i, this);
		return NULL;
	}
	return args_[i]->item;
}

/*
 * Virtuals
 */

// Search accessors (if any) available for node
StepNode* TreeNode::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	// Default is to return NULL since there are no accessors available
	printf("Internal Error: This node type does not have any accessors.\n");
	return NULL;
}
