/*
	*** Dialog Variable
	*** src/parser/dialog.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/prefs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

// Constructors
DialogVariable::DialogVariable(TreeGui* ptr, bool constant)
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
 * Accessors
 */

// Accessor data
Accessor DialogVariable::accessorData[DialogVariable::nAccessors] = {
	{ "title",		VTypes::StringData,	0, false },
	{ "verticalFill",	VTypes::IntegerData,	0, false }
};

// Function data
FunctionAccessor DialogVariable::functionData[DialogVariable::nFunctions] = {
	{ "addButton",		VTypes::WidgetData,"CC[ii]ii",	"string name, string label, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addCheck",		VTypes::WidgetData,"CCI[ii]ii",	"string name, string label, int state, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addCombo",		VTypes::WidgetData,"CCCI[ii]ii","string name, string label, string items, int index, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addDoubleSpin",	VTypes::WidgetData,"CCDDDD[ii]ii","string name, string label, double min, double max, double step, double value, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addEdit",		VTypes::WidgetData,"CCC[ii]ii",	"string name, string label, string text, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addFrame",		VTypes::WidgetData,"C[ii]ii",	"string name, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addGroup",		VTypes::WidgetData,"CC[ii]ii",	"string name, string label, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addIntegerSpin",	VTypes::WidgetData,"CCIIII[ii]ii","string name, string label, int min, int max, int step, int value, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addLabel",		VTypes::WidgetData,"CC[ii]ii",	"string name, string text, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addPage",		VTypes::WidgetData,"CC",	"string name, string label" },
	{ "addRadioButton",	VTypes::WidgetData,"CCCI[ii]ii","string name, string label, string group, int state, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0"},
	{ "addRadioGroup",	VTypes::WidgetData,"C",		"string name" },
	{ "addSpacer",		VTypes::NoData,    "II[ii]ii",	"int expandH, int expandV, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addStack",		VTypes::WidgetData,"C[ii]ii",	"string name, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addTabs",		VTypes::WidgetData,"C[ii]ii",	"string name, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
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
StepNode* DialogVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return DialogVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* DialogVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("DialogVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(name, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'Dialog&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("DialogVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Dialog&' function named '%s'.", qPrintable(name));
			Messenger::exit("DialogVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::DialogData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Dialog&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.", accessorData[i].name);
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (argList != NULL)
		{
			Messenger::print("Error: Argument list given to 'Dialog&' array member '%s'.", qPrintable(name));
			Messenger::exit("DialogVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::DialogData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("DialogVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool DialogVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("DialogVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("DialogVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("DialogVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("DialogVariable::retrieveAccessor");
			return false;
		}
	}
	// Variables used in retrieval
	bool result = true;
	TreeGui* ptr = (TreeGui*) rv.asPointer(VTypes::DialogData, result);
	if ((!result) || (ptr == NULL))
	{
	        Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::DialogData));
	        result = false;
	}
	if (result) switch (acc)
	{
		case (DialogVariable::Title):
			rv.set(ptr->text());
			break;
		case (DialogVariable::VerticalFill):
			rv.set(ptr->qtWidgetObject() == NULL ? false : ptr->qtWidgetObject()->autoFillVertical());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in DialogVariable.\n", accessorData[i].name);
			result = false;
			break;
	}
	Messenger::exit("DialogVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool DialogVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("DialogVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("DialogVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = true;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = false;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = false;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newValue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = false;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("DialogVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	TreeGui* ptr = (TreeGui*) sourcerv.asPointer(VTypes::DialogData, result);
	if ((!result) || (ptr == NULL))
	{
	        Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::DialogData));
	        result = false;
	}
	switch (acc)
	{
		case (DialogVariable::Title):
			ptr->setProperty(TreeGuiWidgetEvent::TextProperty, newValue.asString());
			break;
		case (DialogVariable::VerticalFill):
			if (ptr->qtWidgetObject()) ptr->qtWidgetObject()->setAutoFillVertical(newValue.asBool());
			break;
		default:
			printf("DialogVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("DialogVariable::setAccessor");
	return result;
}

// Perform desired function
bool DialogVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("DialogVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("DialogVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	int xw, xh, l, t;
	Vec3<double> v;
	TreeGuiWidget* w;
	bool result = true;
	TreeGui* ptr = (TreeGui*) rv.asPointer(VTypes::DialogData, result);
	if ((!result) || (ptr == NULL))
	{
	        Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::DialogData));
	        result = false;
	}
	if (result) switch (i)
	{
		case (DialogVariable::AddButton):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addCheck(node->argc(0), node->argc(1), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddCheck):
			l = node->hasArg(3) ? node->argi(3) : -1;
			t = node->hasArg(4) ? node->argi(4) : -1;
			xw = node->hasArg(5) ? node->argi(5) : 0;
			xh = node->hasArg(6) ? node->argi(6) : 0;
			rv.set(VTypes::WidgetData, ptr->addCheck(node->argc(0), node->argc(1), node->argi(2), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddCombo):
			l = node->hasArg(4) ? node->argi(4) : -1;
			t = node->hasArg(5) ? node->argi(5) : -1;
			xw = node->hasArg(6) ? node->argi(6) : 0;
			xh = node->hasArg(7) ? node->argi(7) : 0;
			rv.set(VTypes::WidgetData, ptr->addCombo(node->argc(0), node->argc(1), node->argc(2), node->argi(3), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddDoubleSpin):
			l = node->hasArg(6) ? node->argi(6) : -1;
			t = node->hasArg(7) ? node->argi(7) : -1;
			xw = node->hasArg(8) ? node->argi(8) : 0;
			xh = node->hasArg(9) ? node->argi(9) : 0;
			rv.set(VTypes::WidgetData, ptr->addDoubleSpin(node->argc(0), node->argc(1), node->argd(2), node->argd(3), node->argd(4), node->argd(5), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddEdit):
			l = node->hasArg(3) ? node->argi(3) : -1;
			t = node->hasArg(4) ? node->argi(4) : -1;
			xw = node->hasArg(5) ? node->argi(5) : 0;
			xh = node->hasArg(6) ? node->argi(6) : 0;
			rv.set(VTypes::WidgetData, ptr->addEdit(node->argc(0), node->argc(1), node->argc(2), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddFrame):
			l = node->hasArg(1) ? node->argi(1) : -1;
			t = node->hasArg(2) ? node->argi(2) : -1;
			xw = node->hasArg(3) ? node->argi(3) : 0;
			xh = node->hasArg(4) ? node->argi(4) : 0;
			rv.set(VTypes::WidgetData, ptr->addFrame(node->argc(0), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddGroup):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addGroup(node->argc(0), node->argc(1), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddIntegerSpin):
			l = node->hasArg(6) ? node->argi(6) : -1;
			t = node->hasArg(7) ? node->argi(7) : -1;
			xw = node->hasArg(8) ? node->argi(8) : 0;
			xh = node->hasArg(9) ? node->argi(9) : 0;
			rv.set(VTypes::WidgetData, ptr->addIntegerSpin(node->argc(0), node->argc(1), node->argi(2), node->argi(3), node->argi(4), node->argi(5), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddLabel):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addLabel(node->argc(0), node->argc(1), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddPage):
			rv.set(VTypes::WidgetData, ptr->addPage(node->argc(0), node->argc(1)));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddRadioButton):
			l = node->hasArg(4) ? node->argi(4) : -1;
			t = node->hasArg(5) ? node->argi(5) : -1;
			xw = node->hasArg(6) ? node->argi(6) : 0;
			xh = node->hasArg(7) ? node->argi(7) : 0;
			rv.set(VTypes::WidgetData, ptr->addRadioButton(node->argc(0), node->argc(1), node->argc(2), node->argi(3), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddRadioGroup):
			rv.set(VTypes::WidgetData, ptr->addRadioGroup(node->argc(0)));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddStack):
			l = node->hasArg(1) ? node->argi(1) : -1;
			t = node->hasArg(2) ? node->argi(2) : -1;
			xw = node->hasArg(3) ? node->argi(3) : 0;
			xh = node->hasArg(4) ? node->argi(4) : 0;
			rv.set(VTypes::WidgetData, ptr->addStack(node->argc(0), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AddTabs):
			l = node->hasArg(1) ? node->argi(1) : -1;
			t = node->hasArg(2) ? node->argi(2) : -1;
			xw = node->hasArg(3) ? node->argi(3) : 0;
			xh = node->hasArg(4) ? node->argi(4) : 0;
			rv.set(VTypes::WidgetData, ptr->addTabs(node->argc(0), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		case (DialogVariable::AsDouble):
			w = ptr->findWidget(node->argc(0));
			if (w != NULL) rv.set(w->asDouble());
			else
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(0)), qPrintable(ptr->name()));
				result = false;
			}
			break;
		case (DialogVariable::AsInteger):
			w = ptr->findWidget(node->argc(0));
			if (w != NULL) rv.set(w->asInteger());
			else
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(0)), qPrintable(ptr->name()));
				result = false;
			}
			break;
		case (DialogVariable::AsString):
			w = ptr->findWidget(node->argc(0));
			if (w != NULL) rv.set(w->asCharacter());
			else
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(0)), qPrintable(ptr->name()));
				result = false;
			}
			break;
		case (DialogVariable::AsVector):
			w = ptr->findWidget(node->argc(0));
			if (w != NULL) v.x = w->asDouble();
			else
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(0)), qPrintable(ptr->name()));
				result = false;
			}
			w = ptr->findWidget(node->argc(1));
			if (w != NULL) v.y = w->asDouble();
			else
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(1)), qPrintable(ptr->name()));
				result = false;
			}
			w = ptr->findWidget(node->argc(2));
			if (w != NULL) v.z = w->asDouble();
			else
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(2)), qPrintable(ptr->name()));
				result = false;
			}
			rv.set(v);
			break;
		case (DialogVariable::IsInteger):
			w = ptr->findWidget(node->argc(0));
			if (w == NULL)
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(0)), qPrintable(ptr->name()));
				result = false;
			}
			rv.set(w->asInteger() == node->argi(1));
			break;
		case (DialogVariable::IsRange):
			w = ptr->findWidget(node->argc(0));
			if (w == NULL)
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(0)), qPrintable(ptr->name()));
				result = false;
			}
			rv.set((w->asInteger() >= node->argi(1)) && (w->asInteger() <= node->argi(2)));
			break;
		case (DialogVariable::IsString):
			w = ptr->findWidget(node->argc(0));
			if (w == NULL)
			{
				Messenger::print("Error: No Widget named '%s' exists in the dialog '%s'.", qPrintable(node->argc(0)), qPrintable(ptr->name()));
				result = false;
			}
			rv.set(node->argc(1) == w->asCharacter());
			break;
		case (DialogVariable::Show):
			// If the GUI exists, or it doesn't but we allow dialogs to be raised, show it
			if (prefs.allowDialogs()) rv.set(ptr->execute());
			else rv.set(true);
			break;
		case (DialogVariable::Widget):
			rv.set(VTypes::WidgetData, ptr->findWidget(node->argc(0)));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = false;
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in DialogVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("DialogVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void DialogVariable::printAccessors()
{
	if (DialogVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<DialogVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((DialogVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<DialogVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
 * Variable Array
 */

// Constructor
DialogArrayVariable::DialogArrayVariable(TreeNode* sizeexpr, bool constant)
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
StepNode* DialogArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return DialogVariable::accessorSearch(name, arrayIndex, argList);
}
