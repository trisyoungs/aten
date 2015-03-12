/*
	*** Widget Variable
	*** src/parser/widget.cpp
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

#include "parser/widget.h"
#include "parser/stepnode.h"
#include "parser/treegui.h"
#include "gui/treegui.h"

ATEN_USING_NAMESPACE

// Constructors
WidgetVariable::WidgetVariable(TreeGuiWidget* widget, bool constant)
{
	// Private variables
	returnType_ = VTypes::WidgetData;
	readOnly_ = constant;
	pointerData_ = widget;
}

// Destructor
WidgetVariable::~WidgetVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor WidgetVariable::accessorData[WidgetVariable::nAccessors] = {
	{ "enabled",		VTypes::IntegerData,	0, FALSE },
	{ "verticalFill",	VTypes::IntegerData,	0, FALSE },
	{ "visible",		VTypes::IntegerData,	0, FALSE }
};

// Function data
FunctionAccessor WidgetVariable::functionData[WidgetVariable::nFunctions] = {
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
	{ "addSpacer",		VTypes::NoData,	   "II[ii]ii",	"int expandH, int expandV, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addStack",		VTypes::WidgetData,"C[ii]ii",	"string name, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "addTabs",		VTypes::WidgetData,"C[ii]ii",	"string name, int l = <auto>, int t = <auto>, int xw = 0, int xh = 0" },
	{ "onDouble",		VTypes::NoData,	   "DDCCCs*",	"double minval, double maxval, string event, string widget, string property, double|int|string value = <auto> ..." },
	{ "onInteger",		VTypes::NoData,	   "IICCCs*",	"int minval, int maxval, string event, string widget, string property, double|int|string value = <auto> ..." },
	{ "onString",		VTypes::NoData,	   "CCCCs",	"string text, string event, string widget, string property, double|int|string value = <auto>" },
	{ "setProperty",	VTypes::NoData,	   "Cs",	"string property, double|int|string value" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* WidgetVariable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	return WidgetVariable::accessorSearch(s, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* WidgetVariable::accessorSearch(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("WidgetVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(s, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		i = Variable::searchAccessor(s, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'dialog&' has no member or function named '%s'.", s);
			printAccessors();
			Messenger::exit("WidgetVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'dialog&' function '%s'.", s);
			Messenger::exit("WidgetVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::WidgetData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'dialog&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'Widget&' array member '%s'.", s);
			Messenger::exit("WidgetVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::WidgetData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("WidgetVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool WidgetVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("WidgetVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("WidgetVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("WidgetVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("WidgetVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Variables used in retrieval
	bool result = TRUE;
	TreeGuiWidget* ptr = (TreeGuiWidget*) rv.asPointer(VTypes::WidgetData, result);
	if ((!result) || (ptr == NULL))
	{
	        Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::WidgetData));
	        result = FALSE;
	}
	if (result) switch (acc)
	{
		case (WidgetVariable::Enabled):
			rv.set(ptr->enabled());
			break;
		case (WidgetVariable::VerticalFill):
			rv.set(ptr->qtWidgetObject() == NULL ? FALSE : ptr->qtWidgetObject()->autoFillVertical());
			break;
		case (WidgetVariable::Visible):
			rv.set(ptr->visible());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in WidgetVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("WidgetVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool WidgetVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("WidgetVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("WidgetVariable::setAccessor");
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
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = FALSE;
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
				result = FALSE;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("WidgetVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	TreeGuiWidget* ptr = (TreeGuiWidget*) sourcerv.asPointer(VTypes::WidgetData, result);
	if ((!result) || (ptr == NULL))
	{
	        Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::WidgetData));
	        result = FALSE;
	}
	switch (acc)
	{
		case (WidgetVariable::Enabled):
			ptr->setEnabled(newValue.asBool());
			if (ptr->qtWidgetObject()) ptr->qtWidgetObject()->updateQt();;
			break;
		case (WidgetVariable::VerticalFill):
			if (ptr->qtWidgetObject()) ptr->qtWidgetObject()->setAutoFillVertical(newValue.asBool());
			break;
		case (WidgetVariable::Visible):
			ptr->setVisible(newValue.asBool());
			if (ptr->qtWidgetObject()) ptr->qtWidgetObject()->updateQt();;
			break;
		default:
			printf("WidgetVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	Messenger::exit("WidgetVariable::setAccessor");
	return result;
}

// Perform desired function
bool WidgetVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("WidgetVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("WidgetVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	int xw, xh, l, t;
	TreeGuiWidgetEvent *event;
	TreeGuiWidgetEvent::EventProperty eventProperty;
	TreeGuiWidgetEvent::EventType eventType;
	TreeGuiWidget *targetWidget;
	ReturnValue value;
	bool result = TRUE;
	TreeGuiWidget* ptr = (TreeGuiWidget*) rv.asPointer(VTypes::WidgetData, result);
	if ((!result) || (ptr == NULL))
	{
	        Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::WidgetData));
	        result = FALSE;
	}
	if (result) switch (i)
	{
		case (WidgetVariable::AddButton):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addCheck(node->argc(0), node->argc(1), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddCheck):
			l = node->hasArg(3) ? node->argi(3) : -1;
			t = node->hasArg(4) ? node->argi(4) : -1;
			xw = node->hasArg(5) ? node->argi(5) : 0;
			xh = node->hasArg(6) ? node->argi(6) : 0;
			rv.set(VTypes::WidgetData, ptr->addCheck(node->argc(0), node->argc(1), node->argi(2), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddCombo):
			l = node->hasArg(4) ? node->argi(4) : -1;
			t = node->hasArg(5) ? node->argi(5) : -1;
			xw = node->hasArg(6) ? node->argi(6) : 0;
			xh = node->hasArg(7) ? node->argi(7) : 0;
			rv.set(VTypes::WidgetData, ptr->addCombo(node->argc(0), node->argc(1), node->argc(2), node->argi(3), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddDoubleSpin):
			l = node->hasArg(6) ? node->argi(6) : -1;
			t = node->hasArg(7) ? node->argi(7) : -1;
			xw = node->hasArg(8) ? node->argi(8) : 0;
			xh = node->hasArg(9) ? node->argi(9) : 0;
			rv.set(VTypes::WidgetData, ptr->addDoubleSpin(node->argc(0), node->argc(1), node->argd(2), node->argd(3), node->argd(4), node->argd(5), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddEdit):
			l = node->hasArg(3) ? node->argi(3) : -1;
			t = node->hasArg(4) ? node->argi(4) : -1;
			xw = node->hasArg(5) ? node->argi(5) : 0;
			xh = node->hasArg(6) ? node->argi(6) : 0;
			rv.set(VTypes::WidgetData, ptr->addEdit(node->argc(0), node->argc(1), node->argc(2), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddFrame):
			l = node->hasArg(1) ? node->argi(1) : -1;
			t = node->hasArg(2) ? node->argi(2) : -1;
			xw = node->hasArg(3) ? node->argi(3) : 0;
			xh = node->hasArg(4) ? node->argi(4) : 0;
			rv.set(VTypes::WidgetData, ptr->addFrame(node->argc(0), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddGroup):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addGroup(node->argc(0), node->argc(1), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddIntegerSpin):
			l = node->hasArg(6) ? node->argi(6) : -1;
			t = node->hasArg(7) ? node->argi(7) : -1;
			xw = node->hasArg(8) ? node->argi(8) : 0;
			xh = node->hasArg(9) ? node->argi(9) : 0;
			rv.set(VTypes::WidgetData, ptr->addIntegerSpin(node->argc(0), node->argc(1), node->argi(2), node->argi(3), node->argi(4), node->argi(5), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddLabel):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			rv.set(VTypes::WidgetData, ptr->addLabel(node->argc(0), node->argc(1), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddPage):
			rv.set(VTypes::WidgetData, ptr->addPage(node->argc(0), node->argc(1)));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddRadioButton):
			l = node->hasArg(4) ? node->argi(4) : -1;
			t = node->hasArg(5) ? node->argi(5) : -1;
			xw = node->hasArg(6) ? node->argi(6) : 0;
			xh = node->hasArg(7) ? node->argi(7) : 0;
			rv.set(VTypes::WidgetData, ptr->addRadioButton(node->argc(0), node->argc(1), node->argc(2), node->argi(3), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddRadioGroup):
			rv.set(VTypes::WidgetData, ptr->addRadioGroup(node->argc(0)));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddSpacer):
			l = node->hasArg(2) ? node->argi(2) : -1;
			t = node->hasArg(3) ? node->argi(3) : -1;
			xw = node->hasArg(4) ? node->argi(4) : 0;
			xh = node->hasArg(5) ? node->argi(5) : 0;
			if (!ptr->addSpacer(node->argb(0), node->argb(1), l, t, xw, xh))  result = FALSE;
			rv.reset();
			break;
		case (WidgetVariable::AddStack):
			l = node->hasArg(1) ? node->argi(1) : -1;
			t = node->hasArg(2) ? node->argi(2) : -1;
			xw = node->hasArg(3) ? node->argi(3) : 0;
			xh = node->hasArg(4) ? node->argi(4) : 0;
			rv.set(VTypes::WidgetData, ptr->addStack(node->argc(0), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::AddTabs):
			l = node->hasArg(1) ? node->argi(1) : -1;
			t = node->hasArg(2) ? node->argi(2) : -1;
			xw = node->hasArg(3) ? node->argi(3) : 0;
			xh = node->hasArg(4) ? node->argi(4) : 0;
			rv.set(VTypes::WidgetData, ptr->addTabs(node->argc(0), l, t, xw, xh));
			if (rv.asPointer(VTypes::WidgetData) == NULL) result = FALSE;
			break;
		case (WidgetVariable::OnDouble):
		case (WidgetVariable::OnInteger):
			rv.reset();
			// Check supplied parameters
			result = FALSE;
			eventType = TreeGuiWidgetEvent::eventType(node->argc(2), TRUE);
			if (eventType == TreeGuiWidgetEvent::nEventTypes) break;
			targetWidget = ptr->parent()->findWidget(node->argc(3));
			if (targetWidget == NULL)
			{
				Messenger::print("Error: No widget named '%s' is defined in the current dialog.", node->argc(3));
				break;
			}
			eventProperty = TreeGuiWidgetEvent::eventProperty(node->argc(4), TRUE);
			if (eventProperty == TreeGuiWidgetEvent::nEventProperties) break;
			event = ptr->addEvent(eventType, targetWidget, eventProperty);
			// Set qualifying value or range
			if (i == WidgetVariable::OnDouble) event->setQualifiers(node->argd(0), node->argd(1));
			else event->setQualifiers(node->argi(0), node->argi(1));
			// If a specific value(s) were supplied, store it. Otherwise the widget's current value will be sent
			if (node->hasArg(5))
			{
				// Can't set more than one value for an onDouble event
				if (i == WidgetVariable::OnDouble)
				{
					if (node->hasArg(6))
					{
						Messenger::print("Error: Can't set more than one send value for an 'onDouble' event.");
						result = FALSE;
					}
					else
					{
						ReturnValue *sendValue = event->addSendValue();
						node->arg(5, *sendValue);
					}
				}
				else
				{
					// Values corresponding to each valid integer in the range may have been supplied
					int range = node->argi(1) - node->argi(0) + 1;
					for (int n = 0; n<range; ++n)
					{
						if (!node->hasArg(5+n)) break;
						ReturnValue *sendValue = event->addSendValue();
						node->arg(5+n, *sendValue);
					}
					// Check number of sendvalues supplied
					if ((event->nSendValues() != 1) && (event->nSendValues() != range))
					{
						Messenger::print("Error: %s values (%i) supplied to 'onRange' function, based on integer range provided (expected (%i).", (event->nSendValues() < range ? "Not enough" : "Too many"), event->nSendValues(), range);
						result = FALSE;
						break;
					}
				}
			
			}
			result = TRUE;
			break;
		case (WidgetVariable::OnString):
			rv.reset();
			// Check supplied parameters
			result = FALSE;
			eventType = TreeGuiWidgetEvent::eventType(node->argc(1), TRUE);
			if (eventType == TreeGuiWidgetEvent::nEventTypes) break;
			targetWidget = ptr->parent()->findWidget(node->argc(2));
			if (targetWidget == NULL)
			{
				Messenger::print("Error: No widget named '%s' is defined in the current dialog.", node->argc(2));
				break;
			}
			eventProperty = TreeGuiWidgetEvent::eventProperty(node->argc(3), TRUE);
			if (eventProperty == TreeGuiWidgetEvent::nEventProperties) break;
			event = ptr->addEvent(eventType, targetWidget, eventProperty);
			// Set qualifying value
			event->setQualifiers(node->argc(0));
			// If a specific value was supplied, store it. Otherwise the widget's current value will be sent
			if (node->hasArg(4))
			{
				ReturnValue *sendValue = event->addSendValue();
				node->arg(4, *sendValue);
			}
			result = TRUE;
			break;
		case (WidgetVariable::SetProperty):
			eventProperty = TreeGuiWidgetEvent::eventProperty(node->argc(0), TRUE);
			if (eventProperty == TreeGuiWidgetEvent::nEventProperties) break;
			if (node->argType(1) == VTypes::IntegerData) value.set(node->argi(1));
			else if (node->argType(1) == VTypes::DoubleData) value.set(node->argd(1));
			else value.set(node->argc(1));
			result = ptr->setProperty(eventProperty, value);
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in WidgetVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("WidgetVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void WidgetVariable::printAccessors()
{
	if (WidgetVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		for (int n=0; n<WidgetVariable::nAccessors; ++n) Messenger::print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print("");
	}
	if ((WidgetVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		for (int n=0; n<WidgetVariable::nFunctions; ++n) Messenger::print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print("");
	}
}

/*
// Variable Array
*/

// Constructor
WidgetArrayVariable::WidgetArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::WidgetData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* WidgetArrayVariable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	return WidgetVariable::accessorSearch(s, arrayIndex, argList);
}
