/*
	*** Widget Node FUnctions
	*** src/parser/widgetnode.cpp
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

#include "parser/widgetnode.h"
#include "parser/treenode.h"
#include "base/lineparser.h"
#include "base/messenger.h"
#include "base/sysfunc.h"

// GUI Control Typesname
const char *GuiControlKeywords[WidgetNode::nGuiControls] = { "check", "combo", "doublespin", "edit", "intcombo", "intspin", "label", "radio", "radiogroup", "stack", "stringradiogroup" };
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
const char *StateActionKeywords[StateChange::nStateActions] = { "checked", "disable", "enable", "items", "maximum", "minimum", "originalitems", "step", "switchstack", "value" };
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
	// Private variables
	dynamicValue_ = TRUE;
	
	// Public variables
	prev = NULL;
	next = NULL;
}


// Set control value for which state change applies
void StateChange::setStateValue(const char *value)
{
	stateValue_ = value;
}

// Set control value for which state change applies
void StateChange::setStateValue(int i)
{
	stateValue_ = itoa(i);
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

// Return whether or not the state change value is linked to current control state
bool StateChange::dynamicValue()
{
	return dynamicValue_;
}

// Set whether or not the state change value is linked to current control state
void StateChange::setDynamicValue(bool b)
{
	dynamicValue_ = b;
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
	nodeType_ = TreeNode::GuiWidgetNode;
	controlType_ = WidgetNode::nGuiControls;
	returnType_ = VTypes::NoData;
	widget_ = NULL;
	object_ = NULL;
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

// Set integer return value accessed from value get calls
void WidgetNode::setReturnValue(int i)
{
	returnValue_ = i;
}

// Set double return value accessed from value get calls
void WidgetNode::setReturnValue(double d)
{
	returnValue_ = d;
}

// Set string return value accessed from value get calls
void WidgetNode::setReturnValue(const char *s)
{
	returnValue_ = s;
}

// Set return value
void WidgetNode::setReturnValue(ReturnValue &rv)
{
	returnValue_ = rv;
}

// Set widget value from supplied ReturnValue
bool WidgetNode::setWidgetValue(ReturnValue &rv)
{
	msg.enter("WidgetNode::setWidgetValue");
	QLineEdit *lineedit;
	QCheckBox *checkbox;
	QLabel *label;
	QSpinBox *spinbox;
	QDoubleSpinBox *dspinbox;
	QComboBox *combo;
	QRadioButton *radio;
	QButtonGroup *buttongroup;
	QAbstractButton *button;
	bool result = TRUE;
	switch (controlType_)
	{
		case (WidgetNode::EditControl):
			lineedit = qobject_cast<QLineEdit*> (widget_);
			if (lineedit) lineedit->setText(rv.asString());
			else
			{
				msg.print("WidgetNode::setWidgetValue() - Couldn't set text of edit control.\n");
				result = FALSE;
			}
			break;
		case (WidgetNode::IntegerComboControl):
		case (WidgetNode::ComboControl):
			combo = qobject_cast<QComboBox*> (widget_);
			if (combo)
			{
				// If an integer was supplied, just set the index. Otherwise, search for string
				if (rv.type() == VTypes::IntegerData)
				{
					// Check range of argument
					int id = rv.asInteger()-1;
					if ((id < 0) || (id >= combo->count()))
					{
						msg.print("Error: Index %i is out of range for Combo.\n", id+1);
						result = FALSE;
					}
					else combo->setCurrentIndex(rv.asInteger());
				}
				else if (rv.type() == VTypes::StringData)
				{
					// Search for combo item corresponding to supplied text
					int id = combo->findText(rv.asString());
					if (id == -1)
					{
						msg.print("Error: Combo has no option '%s'.\n", rv.asString());
						result = FALSE;
					}
					else combo->setCurrentIndex(id);
				}
				else printf("WidgetNode::setWidgetValue() - Couldn't set text of (int)combo control.\n");
			}
			else printf("WidgetNode::setWidgetValue() - Couldn't set text of (int)combo control.\n");
			break;
		case (WidgetNode::DoubleSpinControl):
			dspinbox = qobject_cast<QDoubleSpinBox*> (widget_);
			if (dspinbox) dspinbox->setValue(rv.asDouble());
			else
			{
				msg.print("WidgetNode::setWidgetValue() - Couldn't set text of doublespin control.\n");
				result = FALSE;
			}
			break;
		case (WidgetNode::CheckControl):
			checkbox = qobject_cast<QCheckBox*> (widget_);
			if (checkbox) checkbox->setChecked(rv.asBool());
			else
			{
				msg.print("WidgetNode::setWidgetValue() - Couldn't set state of check control.\n");
				result = FALSE;
			}
			break;
		case (WidgetNode::RadioButtonControl):
			radio = qobject_cast<QRadioButton*> (widget_);
			if (radio) radio->setChecked(rv.asBool());
			else printf("WidgetNode::setWidgetValue() - Couldn't set state of radio control.\n");
			break;
		case (WidgetNode::StringRadioGroupControl):
		case (WidgetNode::RadioGroupControl):
			buttongroup = qobject_cast<QButtonGroup*> (object_);
			if (buttongroup)
			{
				if (rv.type() == VTypes::IntegerData)
				{
					// Search for button with supplied id
					button = buttongroup->button(rv.asInteger());
					if (button == NULL) printf("WidgetNode::setWidgetValue() - Couldn't find button %i in buttongroup.\n", rv.asInteger());
					else button->setChecked(TRUE);
				}
				else printf("WidgetNode::setWidgetValue() - Can only set active buttongroup member from an integer id.\n");
			}
			else printf("WidgetNode::setWidgetValue() - Couldn't set radiogroup control.\n");
			break;
		case (WidgetNode::IntegerSpinControl):
			spinbox = qobject_cast<QSpinBox*> (widget_);
			if (spinbox) spinbox->setValue(rv.asInteger());
			else printf("WidgetNode::setWidgetValue() - Couldn't set text of intspin control.\n");
			break;
		case (WidgetNode::LabelControl):
			lineedit = qobject_cast<QLineEdit*> (widget_);
			if (lineedit) lineedit->setText(rv.asString());
			else printf("WidgetNode::setWidgetValue() - Couldn't set text of edit control.\n");
			break;
		case (WidgetNode::StackControl):
			printf("WidgetNode::setWidgetValue() - Couldn't set stack control.\n");
			break;
	}
	msg.exit("WidgetNode::setWidgetValue");
	return result;
}

// Set argument list from parser-joined treenodes
bool WidgetNode::addJoinedArguments(TreeNode *arglist)
{
	msg.enter("WidgetNode::addJoinedArguments");
	// From supplied argument list (which contains items in reverse order as passed from the parser) get rest of data...
	TreeNode *arg;
	for (arg = arglist; arg != NULL; arg = arg->prevArgument) if (arg->prevArgument == NULL) break;
	ReturnValue rv;
	Dnchar value;

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
		case (WidgetNode::StringRadioGroupControl):
			returnType_ = VTypes::StringData;
			break;
		case (WidgetNode::DoubleSpinControl):
			returnType_ = VTypes::DoubleData;
			break;
		case (WidgetNode::CheckControl):
		case (WidgetNode::RadioButtonControl):
		case (WidgetNode::RadioGroupControl):
		case (WidgetNode::IntegerSpinControl):
		case (WidgetNode::IntegerComboControl):
			returnType_ = VTypes::IntegerData;
			break;
		case (WidgetNode::LabelControl):
		case (WidgetNode::StackControl):
			returnType_ = VTypes::NoData;
			break;
	}

	// Next arguments are specific to the control type
	bool result = FALSE;
	if (arg != NULL) arg = arg->nextArgument;
	switch (controlType_)
	{
		// Check Box - option("Title", "check", int state)
		case (WidgetNode::CheckControl):
			if (!setData("state", arg, "Error: No initial state supplied for 'check' GUI filter option - 'off' assumed.\n", TRUE, "0")) break;
			if (arg != NULL) arg = arg->nextArgument;
			// Set default return value
			data("state", value);
			returnValue_ = value.asInteger();
			result = TRUE;
			break;
		// Check Box - option("Title", "check", "buttongroup", int state)
		case (WidgetNode::RadioButtonControl):
			if (!setData("buttongroup", arg, "Error: No button group supplied for 'radiobutton' control.\n", TRUE, "")) break;
			if (arg != NULL) arg = arg->nextArgument;
			if (!setData("state", arg, "Error: No initial state supplied for 'radiobutton' GUI filter option.\n", TRUE, "0")) break;
			if (arg != NULL) arg = arg->nextArgument;
			// Set default return value
			data("state", value);
			returnValue_ = value.asInteger();
			result = TRUE;
			break;
		// RadioGroup 'containers'
		case (WidgetNode::RadioGroupControl):
		case (WidgetNode::StringRadioGroupControl):
			result = TRUE;
			break;
		// Combo Box - option("Title", "combo", "<csv itemlist>", int default=1)
		case (WidgetNode::IntegerComboControl):
		case (WidgetNode::ComboControl):
			if (!setData("items", arg, "Error: No items list supplied for 'combo' GUI filter option.\n", TRUE, "")) break;
			if (arg != NULL) arg = arg->nextArgument;
			setData("default", arg, "No default value supplied for 'combo' GUI filter option - '1' assumed.\n", TRUE, "1");
			if (arg != NULL) arg = arg->nextArgument;
			// Set default return value
			if (controlType_ == WidgetNode::ComboControl)
			{
				data("items", value);
				LineParser parser;
				parser.getArgsDelim(LineParser::UseQuotes, value);
				data("default", value);
				returnValue_ = parser.argc(value.asInteger()-1);
			}
			else
			{
				data("default", value);
				returnValue_ = value.asInteger();
			}
			result = TRUE;
			break;
		// Double Spin Edit - option("Title", "spin", double min, double max, double start, double step)
		case (WidgetNode::DoubleSpinControl):
			if (!setData("min", arg, "Error: No minimum value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			if (arg != NULL) arg = arg->nextArgument;
			if (!setData("max", arg, "Error: No maximum value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			if (arg != NULL) arg = arg->nextArgument;
			if (!setData("step", arg, "Error: No step value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			if (arg != NULL) arg = arg->nextArgument;
			if (!setData("start", arg, "Error: No starting value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			if (arg != NULL) arg = arg->nextArgument;
			// Set default return value
			data("start", value);
			returnValue_ = value.asDouble();
			result = TRUE;
			break;
		// Spin Edit - option("Title", "edit", string text = "")
		case (WidgetNode::EditControl):
			setData("text", arg, "No default string supplied for 'edit' GUI filter option - empty string assumed.\n", TRUE, "");
			if (arg != NULL) arg = arg->nextArgument;
			// Set default return value
			data("text", value);
			returnValue_ = value;
			result = TRUE;
			break;
		// Integer Spin Edit - option("Title", "spin", int min, int max, int start, int step)
		case (WidgetNode::IntegerSpinControl):
			if (!setData("min", arg, "Error: No minimum value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("max", arg, "Error: No maximum value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("step", arg, "Error: No step value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("start", arg, "Error: No starting value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			// Set default return value
			data("start", value);
			returnValue_ = value.asInteger();
			result = TRUE;
			break;
		// Stack - number of pages, initial page number
		case (WidgetNode::StackControl):
			setData("pages", arg, "Error: Number of pages not supplied for stack.\n", TRUE, "");
			if (arg != NULL) arg = arg->nextArgument;
			setData("index", arg, "Error: Initial page index not supplied for stack.\n", TRUE, "");
			if (arg != NULL) arg = arg->nextArgument;
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
		if (critical) return FALSE;
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
	Dnchar keywd, argdata, otherdata;
	// Keyword is part of string before first '@' before first '=' (if both/either exists)
	keywd = beforeChar(rv.asString(), '@');
	if (keywd.find('=') != -1)
	{
		otherdata = keywd;
		argdata = afterChar(rv.asString(), '=');
		keywd = beforeChar(otherdata, '=');
		otherdata = "";
	}
	else argdata = afterChar(rv.asString(), '@');
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
			// Split argument again: part before '@' is the state value (if any)
			if (argdata.find('@') != -1)
			{
				keywd = beforeChar(argdata.get(), '@');
				state->setStateValue(keywd.get());
				state->setDynamicValue(FALSE);
				otherdata = afterChar(argdata.get(), '@');
			}
			else otherdata = argdata;
			// ...after '@' and before '?' is the target control...
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

// Set object pointer
void WidgetNode::setObject(QObject *obj)
{
	object_ = obj;
}

// Return object pointer
QObject *WidgetNode::object()
{
	return object_;
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
