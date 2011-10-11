/*
	*** Dialog Variable
	*** src/parser/dialog.cpp
	Copyright T. Youngs 2007-2011

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

#include "parser/dialog.h"
#include "parser/stepnode.h"
#include "parser/treegui.h"
#include "gui/treegui.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <base/progress.h>

// Constructors
DialogVariable::DialogVariable(TreeGui *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::DialogData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
DialogVariable::~DialogVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor DialogVariable::accessorData[DialogVariable::nAccessors] = {
	{ "created",		VTypes::IntegerData,	0, FALSE },
	{ "title",		VTypes::StringData,	0, FALSE },
	{ "verticalFill",	VTypes::IntegerData,	0, FALSE }
};

// Function data
FunctionAccessor DialogVariable::functionData[DialogVariable::nFunctions] = {
	{ "addButton",		VTypes::WidgetData,"CCiiii",	"string name, string label, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addCheck",		VTypes::WidgetData,"CCIiiii",	"string name, string label, int state, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addCombo",		VTypes::WidgetData,"CCCIiiii",	"string name, string label, string items, int index, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addDoubleSpin",	VTypes::WidgetData,"CCDDDDiiii","string name, string label, double min, double max, double step, double value, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addEdit",		VTypes::WidgetData,"CCCiiii",	"string name, string label, string text, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addFrame",		VTypes::WidgetData,"Ciiii",	"string name, int l = <auto>, int t = <auto>, int xw = 1, int xh = 0" },
	{ "addGroup",		VTypes::WidgetData,"CCiiii",	"string name, string label, int l = <auto>, int t = <auto>, int xw = 1, int xh = 0" },
	{ "addIntegerSpin",	VTypes::WidgetData,"CCIIIIiiii","string name, string label, int min, int max, int step, int value, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addLabel",		VTypes::WidgetData,"CCiiii",	"string name, string text, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addPage",		VTypes::WidgetData,"CC",	"string name, string label" },
	{ "addRadioButton",	VTypes::WidgetData,"CCCIiiii",	"string name, string label, string group, int state, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0"},
	{ "addRadioGroup",	VTypes::WidgetData,"C",		"string name" },
	{ "addStack",		VTypes::WidgetData,"Ciiii",	"string name, int l = <auto>, int t = <auto>, int xw = 1, int xh = 0" },
	{ "addTabs",		VTypes::WidgetData,"Ciiii",	"string name, int l = <auto>, int t = <auto>, int xw = 1, int xh = 0" },
	{ "asDouble",		VTypes::DoubleData,"C",		"string name" },
	{ "asInteger",		VTypes::IntegerData,"C",	"string name" },
	{ "asString",		VTypes::StringData,"C",		"string name" },
	{ "asVector",		VTypes::VectorData,"CCC",	"string name1, string name2, string name3" },
	{ "isInteger",		VTypes::IntegerData,"CI",	"string name, int value" },
	{ "isRange",		VTypes::IntegerData,"CII",	"string name, int minvalue, int maxvalue" },
	{ "isString",		VTypes::IntegerData,"CC",	"string name, string value" },
	{ "show",		VTypes::IntegerData,"",		"" },
	{ "widget",		VTypes::WidgetData,"C",		"string name" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *DialogVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return DialogVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *DialogVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("DialogVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	i = Variable::searchAccessor(s, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(s, nFunctions, functionData);
		if (i == -1)
		{
			msg.print("Error: Type 'dialog&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("DialogVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'dialog&' function '%s'.\n", s);
			msg.exit("DialogVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::DialogData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'dialog&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
		{
			msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
			result = NULL;
		}
		else result = new StepNode(i, VTypes::DialogData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("DialogVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool DialogVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("DialogVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		msg.exit("DialogVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("DialogVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("DialogVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Variables used in retrieval
	bool result = TRUE;
	TreeGui *ptr = (TreeGui*) rv.asPointer(VTypes::DialogData, result);
	if ((!result) || (ptr == NULL))
	{
	        msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::DialogData));
	        result = FALSE;
	}
	if (result) switch (acc)
	{
		case (DialogVariable::Created):
			rv.set(ptr->created());
			break;
		case (DialogVariable::Title):
			rv.set(ptr->text());
			break;
		case (DialogVariable::VerticalFill):
			rv.set(ptr->qtWidgetObject() == NULL ? FALSE : ptr->qtWidgetObject()->autoFillVertical());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in DialogVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("DialogVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool DialogVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("DialogVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		msg.exit("DialogVariable::setAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = TRUE;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				msg.print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if (newvalue.arraySize() > 0)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if (newvalue.arraySize() > accessorData[i].arraySize)
			{
				msg.print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).\n", accessorData[i].name, newvalue.arraySize(), accessorData[i].arraySize);
				result = FALSE;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newvalue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
			else if ((newvalue.type() != VTypes::VectorData) && (newvalue.arraySize() != 3))
			{
				msg.print("Error: Only an array of size 3 can be assigned to a vector (member '%s').\n", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		msg.exit("DialogVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	TreeGui *ptr = (TreeGui*) sourcerv.asPointer(VTypes::DialogData, result);
	if ((!result) || (ptr == NULL))
	{
	        msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::DialogData));
	        result = FALSE;
	}
	switch (acc)
	{
		case (DialogVariable::Created):
			ptr->setCreated(newvalue.asBool());
			break;
		case (DialogVariable::Title):
			ptr->setProperty(TreeGuiWidgetEvent::TextProperty, newvalue.asString());
			break;
		case (DialogVariable::VerticalFill):
			if (ptr->qtWidgetObject()) ptr->qtWidgetObject()->setAutoFillVertical(newvalue.asBool());
			break;
		default:
			printf("DialogVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("DialogVariable::setAccessor");
	return result;
}

// Perform desired function
bool DialogVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("DialogVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Aten type.\n", i);
		msg.exit("DialogVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	int xw, xh, l, t;
	Vec3<double> v;
	TreeGuiWidget *w;
	bool result = TRUE;
	TreeGui *ptr = (TreeGui*) rv.asPointer(VTypes::DialogData, result);
	if ((!result) || (ptr == NULL))
	{
	        msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::DialogData));
	        result = FALSE;
	}
	if (result) switch (i)
	{
		case (DialogVariable::AddButton):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addCheck(node->argc(0), node->argc(1), l, t, xw, xh));
			break;
		case (DialogVariable::AddCheck):
			l = node->hasArg(3) ? node->argi(3) : -1;
			t = node->hasArg(4) ? node->argi(4) : -1;
			xw = node->hasArg(5) ? node->argi(5) : 0;
			xh = node->hasArg(6) ? node->argi(6) : 0;
			rv.set(VTypes::WidgetData, ptr->addCheck(node->argc(0), node->argc(1), node->argi(2), l, t, xw, xh));
			break;
		case (DialogVariable::AddCombo):
			l = node->hasArg(4) ? node->argi(4) : -1;
			t = node->hasArg(5) ? node->argi(5) : -1;
			xw = node->hasArg(6) ? node->argi(6) : 0;
			xh = node->hasArg(7) ? node->argi(7) : 0;
			rv.set(VTypes::WidgetData, ptr->addCombo(node->argc(0), node->argc(1), node->argc(2), node->argi(3), l, t, xw, xh));
			break;
		case (DialogVariable::AddDoubleSpin):
			l = node->hasArg(6) ? node->argi(6) : -1;
			t = node->hasArg(7) ? node->argi(7) : -1;
			xw = node->hasArg(8) ? node->argi(8) : 0;
			xh = node->hasArg(9) ? node->argi(9) : 0;
			rv.set(VTypes::WidgetData, ptr->addDoubleSpin(node->argc(0), node->argc(1), node->argd(2), node->argd(3), node->argd(4), node->argd(5), l, t, xw, xh));
			break;
		case (DialogVariable::AddEdit):
			l = node->hasArg(3) ? node->argi(3) : -1;
			t = node->hasArg(4) ? node->argi(4) : -1;
			xw = node->hasArg(5) ? node->argi(5) : 0;
			xh = node->hasArg(6) ? node->argi(6) : 0;
			rv.set(VTypes::WidgetData, ptr->addEdit(node->argc(0), node->argc(1), node->argc(2), l, t, xw, xh));
			break;
		case (DialogVariable::AddFrame):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 1;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addFrame(node->argc(0), l, t, xw, xh));
			break;
		case (DialogVariable::AddGroup):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 1;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addGroup(node->argc(0), node->argc(1), l, t, xw, xh));
			break;
		case (DialogVariable::AddIntegerSpin):
			l = node->hasArg(6) ? node->argi(6) : -1;
			t = node->hasArg(7) ? node->argi(7) : -1;
			xw = node->hasArg(8) ? node->argi(8) : 0;
			xh = node->hasArg(9) ? node->argi(9) : 0;
			rv.set(VTypes::WidgetData, ptr->addIntegerSpin(node->argc(0), node->argc(1), node->argi(2), node->argi(3), node->argi(4), node->argi(5), l, t, xw, xh));
			break;
		case (DialogVariable::AddLabel):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addLabel(node->argc(0), node->argc(1), l, t, xw, xh));
			break;
		case (DialogVariable::AddPage):
			rv.set(VTypes::WidgetData, ptr->addPage(node->argc(0), node->argc(1)));
			break;
		case (DialogVariable::AddRadioButton):
			l = node->hasArg(4) ? node->argi(4) : -1;
			t = node->hasArg(5) ? node->argi(5) : -1;
			xw = node->hasArg(6) ? node->argi(6) : 0;
			xh = node->hasArg(7) ? node->argi(7) : 0;
			rv.set(VTypes::WidgetData, ptr->addRadioButton(node->argc(0), node->argc(1), node->argc(2), node->argi(3), l, t, xw, xh));
			break;
		case (DialogVariable::AddRadioGroup):
			rv.set(VTypes::WidgetData, ptr->addRadioGroup(node->argc(0)));
			break;
		case (DialogVariable::AddStack):
			l = node->hasArg(1) ? node->argi(1) : -1;
			t = node->hasArg(2) ? node->argi(2) : -1;
			xw = node->hasArg(3) ? node->argi(3) : 1;
			xh = node->hasArg(4) ? node->argi(4) : 0;
			rv.set(VTypes::WidgetData, ptr->addStack(node->argc(0), l, t, xw, xh));
			break;
		case (DialogVariable::AddTabs):
			l = node->hasArg(1) ? node->argi(1) : -1;
			t = node->hasArg(2) ? node->argi(2) : -1;
			xw = node->hasArg(3) ? node->argi(3) : 1;
			xh = node->hasArg(4) ? node->argi(4) : 0;
			rv.set(VTypes::WidgetData, ptr->addTabs(node->argc(0), l, t, xw, xh));
			break;
		case (DialogVariable::AsDouble):
			w = ptr->findWidget(node->argc(0));
			if (w != NULL) rv.set(w->asDouble());
			else
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(0), ptr->name());
				result = FALSE;
			}
			break;
		case (DialogVariable::AsInteger):
			w = ptr->findWidget(node->argc(0));
			if (w != NULL) rv.set(w->asInteger());
			else
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(0), ptr->name());
				result = FALSE;
			}
			break;
		case (DialogVariable::AsString):
			w = ptr->findWidget(node->argc(0));
			if (w != NULL) rv.set(w->asCharacter());
			else
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(0), ptr->name());
				result = FALSE;
			}
			break;
		case (DialogVariable::AsVector):
			w = ptr->findWidget(node->argc(0));
			if (w != NULL) v.x = w->asDouble();
			else
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(0), ptr->name());
				result = FALSE;
			}
			w = ptr->findWidget(node->argc(1));
			if (w != NULL) v.y = w->asDouble();
			else
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(1), ptr->name());
				result = FALSE;
			}
			w = ptr->findWidget(node->argc(2));
			if (w != NULL) v.z = w->asDouble();
			else
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(2), ptr->name());
				result = FALSE;
			}
			rv.set(v);
			break;
		case (DialogVariable::IsInteger):
			w = ptr->findWidget(node->argc(0));
			if (w == NULL)
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(0), ptr->name());
				result = FALSE;
			}
			rv.set(w->asInteger() == node->argi(1));
			break;
		case (DialogVariable::IsRange):
			w = ptr->findWidget(node->argc(0));
			if (w == NULL)
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(0), ptr->name());
				result = FALSE;
			}
			rv.set((w->asInteger() >= node->argi(1)) && (w->asInteger() <= node->argi(2)));
			break;
		case (DialogVariable::IsString):
			w = ptr->findWidget(node->argc(0));
			if (w == NULL)
			{
				msg.print("Error: No Widget named '%s' exists in the dialog '%s'.\n", node->argc(0), ptr->name());
				result = FALSE;
			}
			rv.set(strcmp(w->asCharacter(), node->argc(1)) == 0);
			break;
		case (DialogVariable::Show):
			rv.set(ptr->execute());
			break;
		case (DialogVariable::Widget):
			rv.set(VTypes::WidgetData, ptr->findWidget(node->argc(0)));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in DialogVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("DialogVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void DialogVariable::printAccessors()
{
	if (DialogVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<DialogVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((DialogVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<DialogVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
DialogArrayVariable::DialogArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::DialogData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *DialogArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return DialogVariable::accessorSearch(s, arrayindex, arglist);
}
