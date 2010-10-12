/*
	*** Widget Node FUnctions
	*** src/parser/widgetnode.cpp
	Copyright T. Youngs 2007-2010

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

#include "parser/widgetnode.h"
#include "parser/treenode.h"
#include "base/lineparser.h"
#include "base/messenger.h"
#include "base/sysfunc.h"

// GUI Control Typesname
const char *GuiControlKeywords[WidgetNode::nGuiControls] = { "check", "combo", "doublespin", "edit", "intcombo", "intspin", "label", "radiogroup" };
const char *WidgetNode::guiControl(WidgetNode::GuiControl gct)
{
	return GuiControlKeywords[gct];
}
WidgetNode::GuiControl WidgetNode::guiControl(const char *s, bool reporterror)
{
        WidgetNode::GuiControl gct = (WidgetNode::GuiControl) enumSearch("gui control type", WidgetNode::nGuiControls, GuiControlKeywords, s);
	if ((gct == WidgetNode::nGuiControls) && reporterror) enumPrintValid(WidgetNode::nGuiControls,GuiControlKeywords);
	return gct;
}

// Options for Qt layout
const char *GuiQtOptionKeywords[WidgetNode::nGuiQtOptions] = { "centre", "disabled", "group", "labelspan", "left", "newline", "parentspan", "span", "state", "tab" };
const char *WidgetNode::guiQtOption(GuiQtOption gqo)
{
	return GuiQtOptionKeywords[gqo];
}
WidgetNode::GuiQtOption WidgetNode::guiQtOption(const char *s, bool reporterror)
{
        WidgetNode::GuiQtOption gqo = (WidgetNode::GuiQtOption) enumSearch("QUI Qt option", WidgetNode::nGuiQtOptions, GuiQtOptionKeywords, s);
	if ((gqo == WidgetNode::nGuiQtOptions) && reporterror) enumPrintValid(WidgetNode::nGuiQtOptions,GuiQtOptionKeywords);
	return gqo;
}

// State change actions
enum StateAction { DisableAction, EnableAction, ItemsAction, nStateActions };
const char *StateActionKeywords[StateChange::nStateActions] = { "checked", "disable", "enable", "items", "originalitems" };
StateChange::StateAction StateChange::stateAction(const char *s, bool reporterror)
{
        StateChange::StateAction sa = (StateChange::StateAction) enumSearch("state action", StateChange::nStateActions, StateActionKeywords, s);
	if ((sa == StateChange::nStateActions) && reporterror) enumPrintValid(StateChange::nStateActions,StateActionKeywords);
	return sa;
}
const char *StateChange::stateAction(StateChange::StateAction sa)
{
	return StateActionKeywords[sa];
}

/*
// StateChange
*/

// Constructor
StateChange::StateChange()
{
	// Public variables
	prev = NULL;
	next = NULL;
}


// Set control value for which state change applies
void StateChange::setStateValue(const char *value)
{
	stateValue_ = value;
}

// Return control value for which state change applies
const char *StateChange::stateValue() const
{
	return stateValue_.get();
}

// Return control value for which state change applies as an integer
int StateChange::stateValueAsInteger() const
{
	return stateValue_.asInteger();
}

// Return control value for which state change applies as a double
double StateChange::stateValueAsDouble() const
{
	return stateValue_.asDouble();
}

// Set name target widget to which state change applies
void StateChange::setTargetWidget(const char *value)
{
	targetWidget_ = value;
}

// Return name of target widget to which state change applies
const char *StateChange::targetWidget() const
{
	return targetWidget_.get();
}

// Set action to perform on target widget
void StateChange::setChange(StateAction sa, const char *data)
{
	changeData_.setKey(sa);
	changeData_.setValue(data);
}

// Return action type
StateChange::StateAction StateChange::changeAction() const
{
	return changeData_.key();
}

// Return action data
const char *StateChange::changeData() const
{
	return changeData_.value().get();
}

// Return action data as integer
int StateChange::changeDataAsInteger() const
{
	return changeData_.value().asInteger();
}

// Return action data as double
double StateChange::changeDataAsDouble() const
{
	return changeData_.value().asDouble();
}

// Return action data as bool
bool StateChange::changeDataAsBool() const
{
	return changeData_.value().asBool();
}

/*
// WidgetNode
*/

// Constructor
WidgetNode::WidgetNode()
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Private variables
	controlType_ = WidgetNode::nGuiControls;
	returnType_ = VTypes::NoData;
	widget_ = NULL;
	widgetParentType_ = WidgetNode::NoParent;
	widgetParentSpan_ = 2;
	widgetSpan_ = 1;
	widgetLabelAlignment_ = 2;
	widgetLabelSpan_ = 1;
	widgetNewLine_ = FALSE;
	widgetEnabled_ = TRUE;
}

// Destructor
WidgetNode::~WidgetNode()
{
}

// Set return value
void WidgetNode::setReturnValue(const ReturnValue &rv)
{
	returnValue_ = rv;
}

// Set argument list from parser-joined treenodes
bool WidgetNode::addJoinedArguments(TreeNode *arglist)
{
	msg.enter("WidgetNode::addJoinedArguments");
	// From supplied argument list (which contains items in reverse order as passed from the parser) get rest of data...
	TreeNode *arg;
	for (arg = arglist; arg != NULL; arg = arg->prevArgument) if (arg->prevArgument == NULL) break;
	ReturnValue rv;
	Dnchar d;

	// First argument is the control name
	if (arg == NULL)
	{
		msg.print("Error: No control name specified for GUI filter option.\n");
		msg.exit("WidgetNode::addJoinedArguments");
		return FALSE;
	}
	arg->execute(rv);
	name_ = rv.asString();
	msg.print(Messenger::Parse, "GUI filter option name = '%s'\n", name_.get());

	// Next argument is control type
	arg = arg->nextArgument;
	if (arg == NULL)
	{
		msg.print("Error: No control type specified for GUI filter option.\n");
		msg.exit("WidgetNode::addJoinedArguments");
		return FALSE;
	}
	arg->execute(rv);
	controlType_ = WidgetNode::guiControl(rv.asString(), TRUE);
	if (controlType_ == WidgetNode::nGuiControls)
	{
		msg.exit("WidgetNode::addJoinedArguments");
		return FALSE;
	}
	msg.print(Messenger::Parse, "GUI filter option control type = '%s'\n", WidgetNode::guiControl(controlType_));

	// Set basic return type based on control type
	switch (controlType_)
	{
		case (WidgetNode::EditControl):
		case (WidgetNode::ComboControl):
			returnType_ = VTypes::StringData;
			break;
		case (WidgetNode::DoubleSpinControl):
			returnType_ = VTypes::DoubleData;
			break;
		case (WidgetNode::CheckControl):
		case (WidgetNode::RadioGroupControl):
		case (WidgetNode::IntegerSpinControl):
		case (WidgetNode::IntegerComboControl):
			returnType_ = VTypes::IntegerData;
			break;
		case (WidgetNode::LabelControl):
			returnType_ = VTypes::NoData;
			break;
	}

	// Next arguments are specific to the control type
	bool result = FALSE;
	arg = arg->nextArgument;
	switch (controlType_)
	{
		// Check Box - option("Title", "check", int state)
		case (WidgetNode::CheckControl):
			if (!setData("state", arg, "Warning: No initial state supplied for 'check' GUI filter option - 'off' assumed.\n", TRUE, "0")) break;
			arg = arg->nextArgument;
			result = TRUE;
			// Set default value ready to return
			if (!data("state", d)) printf("Internal Error - Failed to set default value for %s control.\n", WidgetNode::guiControl(controlType_));
			returnValue_.set( d.asInteger() );
			break;
		// RadioGroup Box - option("Title", "radiogroup", "<csv itemlist>", int default=1)
		case (WidgetNode::RadioGroupControl):
			if (!setData("items", arg, "Error: No items list supplied for 'radiogroup' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			setData("default", arg, "No default value supplied for 'radiogroup' GUI filter option - '1' assumed.\n", TRUE, "1");
			arg = arg->nextArgument;
			result = TRUE;
			// Set default value ready to return
			if (!data("default", d)) printf("Internal Error - Failed to set default value for %s control.\n", WidgetNode::guiControl(controlType_));
			returnValue_.set( d.asInteger() );
			break;
		// Combo Box - option("Title", "combo", "<csv itemlist>", int default=1)
		case (WidgetNode::IntegerComboControl):
		case (WidgetNode::ComboControl):
			if (!setData("items", arg, "Error: No items list supplied for 'combo' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			setData("default", arg, "No default value supplied for 'combo' GUI filter option - '1' assumed.\n", TRUE, "1");
			arg = arg->nextArgument;
			result = TRUE;
			break;
		// Double Spin Edit - option("Title", "spin", double min, double max, double start, double step)
		case (WidgetNode::DoubleSpinControl):
			if (!setData("min", arg, "Error: No minimum value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("max", arg, "Error: No maximum value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("start", arg, "Error: No starting value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("step", arg, "Error: No step value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			result = TRUE;
			break;
		// Spin Edit - option("Title", "edit", string text = "")
		case (WidgetNode::EditControl):
			setData("text", arg, "No default string supplied for 'edit' GUI filter option - empty string assumed.\n", TRUE, "");
			arg = arg->nextArgument;
			result = TRUE;
			break;
		// Integer Spin Edit - option("Title", "spin", int min, int max, int start, int step)
		case (WidgetNode::IntegerSpinControl):
			if (!setData("min", arg, "Error: No minimum value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("max", arg, "Error: No maximum value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("start", arg, "Error: No starting value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("step", arg, "Error: No step value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			result = TRUE;
			break;
		// Label - no data
		case (WidgetNode::LabelControl):
			result = TRUE;
			break;
		default:
			printf("Internal Error: Setting arguments for control type '%s' has not been implemented.\n", WidgetNode::guiControl(controlType_));
			break;
	}
	// Any remaining arguments are options of the format opt=arg
	for (arg = arg; arg != NULL; arg = arg->nextArgument) setOption(arg);
	msg.exit("WidgetNode::addJoinedArguments");
	return result;
}

// Return type of GUI control
WidgetNode::GuiControl WidgetNode::controlType()
{
	return controlType_;
}

// Return name of option
const char *WidgetNode::name()
{
	return name_.get();
}

// Set associated data
bool WidgetNode::setData(const char *name, TreeNode *arg, const char *errormsg, bool critical, const char *def)
{
	ReturnValue rv;
	if ((arg == NULL) || (!arg->execute(rv)))
	{
		msg.print(errormsg);
		// No argument here - was it a critical value?
		if (critical )return FALSE;
		// Not critical, so set default
		data_.add(name, def);
		return TRUE;
	}
	data_.add(name, rv.asString());
	return TRUE;
}

// Retrieve associated data
bool WidgetNode::data(const char *name, Dnchar &value)
{
	bool success;
	value = data_.value(name, success);
	return success;
}

// Return first state change
StateChange *WidgetNode::stateChanges()
{
	return stateChanges_.first();
}

// Set option from argument
void WidgetNode::setOption(TreeNode *arg)
{
	msg.enter("WidgetNode::setOption");
	ReturnValue rv;
	StateChange *state;
	StateChange::StateAction sa;
	arg->execute(rv);
	Dnchar keywd = beforeChar(rv.asString(), '='), otherdata;
	Dnchar argdata = afterChar(rv.asString(), '=');
	// Determine option enum
	WidgetNode::GuiQtOption gqo = WidgetNode::guiQtOption(keywd.get(), TRUE);
	if (gqo == WidgetNode::nGuiQtOptions) return;
	// Set relevant data
	switch (gqo)
	{
		case (WidgetNode::CentreOption):
			widgetLabelAlignment_ = 4;
			break;
		case (WidgetNode::DisabledOption):
			widgetEnabled_ = FALSE;
			break;
		case (WidgetNode::GroupNameOption):
			widgetParentType_ = WidgetNode::GroupBoxParent;
			widgetParentName_ = argdata;
			break;
		case (WidgetNode::LabelSpanOption):
			widgetLabelSpan_ = argdata.asInteger();
			break;
		case (WidgetNode::LeftOption):
			widgetLabelAlignment_ = 1;
			break;
		case (WidgetNode::NewLineOption):
			widgetNewLine_ = TRUE;
			break;
		case (WidgetNode::ParentSpanOption):
			widgetParentSpan_ = argdata.asInteger();
			break;
		case (WidgetNode::SpanOption):
			widgetSpan_ = argdata.asInteger();
			break;
		case (WidgetNode::StateOption):
			state = stateChanges_.add();
			// Split argument again: part before '@' is the state value
			keywd = beforeChar(argdata.get(), '@');
			state->setStateValue(keywd.get());
			// ...after '@' and before '?' is the target control...
			otherdata = afterChar(argdata.get(), '@');
			keywd = beforeChar(otherdata.get(), '?');
			state->setTargetWidget(keywd.get());
			// ...and after '?' is the state change definition
			keywd = afterChar(otherdata.get(), '?');
			otherdata = beforeChar(keywd.get(), '=');
			if (otherdata.isEmpty()) otherdata = keywd;
			sa = StateChange::stateAction(otherdata.get(), TRUE);
			otherdata = afterChar(keywd.get(), '=');
			state->setChange(sa, otherdata.get());
			break;
		case (WidgetNode::TabsOption):
			widgetParentType_ = WidgetNode::TabWidgetParent;
			widgetParentName_ = argdata;	// In format 'page@tabwidget'
			break;
		default:
			printf("Internal Error: Don't know how to set GuiQtOption '%s'\n", WidgetNode::guiQtOption(gqo));
			break;
	}
	msg.exit("WidgetNode::setOption");
}

// Return whether a parent exists
WidgetNode::GuiWidgetParent WidgetNode::widgetParentType()
{
	return widgetParentType_;
}

// Return parent name
const char *WidgetNode::widgetParentName()
{
	return widgetParentName_.get();
}

// Return parent span
int WidgetNode::widgetParentSpan()
{
	return widgetParentSpan_;
}

// Return label span
int WidgetNode::widgetLabelSpan()
{
	return widgetLabelSpan_;
}

// Return label span
int WidgetNode::widgetLabelAlignment()
{
	return widgetLabelAlignment_;
}

// Return widget span
int WidgetNode::widgetSpan()
{
	return widgetSpan_;
}

// Return newline flag
bool WidgetNode::widgetNewLine()
{
	return widgetNewLine_;
}

// Set widget pointer
void WidgetNode::setWidget(QWidget *w)
{
	widget_ = w;
}

// Return widget pointer
QWidget *WidgetNode::widget()
{
	return widget_;
}

// Return whether the widget is enabled
bool WidgetNode::widgetEnabled()
{
	return widgetEnabled_;
}

/*
// Inherited Virtuals
*/

// Execute command
bool WidgetNode::execute(ReturnValue &rv)
{
	msg.enter("WidgetNode::execute");
	// Just copy local returnvalue contents
	rv = returnValue_;
	msg.exit("WidgetNode::execute");
	return TRUE;
}

// Print node contents
void WidgetNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	Dnchar tab(offset+32);
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab.strcat("   |--> ");
	tab.strcat(prefix);

	// Output node data
	printf("[CN]%s%s (WDGT)\n", tab.get(), name_.get());
}

// Set from returnvalue node
bool WidgetNode::set(ReturnValue &setrv)
{
	msg.enter("WidgetNode::set");
	returnValue_ = setrv;
	msg.exit("WidgetNode::set");
	return TRUE;
}

// Initialise node
bool WidgetNode::initialise()
{
	returnValue_.reset();
	return TRUE;
}
