/*
	*** Gui Filter Option Definition
	*** src/parser/guifilteroption.cpp
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

#include "parser/guifilteroptionnode.h"
#include "parser/treenode.h"
#include "base/lineparser.h"
#include "base/messenger.h"
#include "base/sysfunc.h"

// GUI Control Typesname
const char *GuiControlTypeKeywords[GuiFilterOptionNode::nGuiControlTypes] = { "check", "combo", "doublespin", "edit", "intcombo", "spin" };
const char *GuiFilterOptionNode::guiControlType(GuiFilterOptionNode::GuiControlType gct)
{
	return GuiControlTypeKeywords[gct];
}
GuiFilterOptionNode::GuiControlType GuiFilterOptionNode::guiControlType(const char *s, bool reporterror)
{
        GuiFilterOptionNode::GuiControlType gct = (GuiFilterOptionNode::GuiControlType) enumSearch("gui control type", GuiFilterOptionNode::nGuiControlTypes, GuiControlTypeKeywords, s);
	if ((gct == GuiFilterOptionNode::nGuiControlTypes) && reporterror) enumPrintValid(GuiFilterOptionNode::nGuiControlTypes,GuiControlTypeKeywords);
	return gct;
}

// Options for Qt layout
const char *GuiQtOptionKeywords[GuiFilterOptionNode::nGuiQtOptions] = { "disabled", "group", "labelspan", "newline", "parentspan", "span", "tab" };
const char *GuiFilterOptionNode::guiQtOption(GuiQtOption gqo)
{
	return GuiQtOptionKeywords[gqo];
}
GuiFilterOptionNode::GuiQtOption GuiFilterOptionNode::guiQtOption(const char *s, bool reporterror)
{
        GuiFilterOptionNode::GuiQtOption gqo = (GuiFilterOptionNode::GuiQtOption) enumSearch("QUI Qt option", GuiFilterOptionNode::nGuiQtOptions, GuiQtOptionKeywords, s);
	if ((gqo == GuiFilterOptionNode::nGuiQtOptions) && reporterror) enumPrintValid(GuiFilterOptionNode::nGuiQtOptions,GuiQtOptionKeywords);
	return gqo;
}

// Constructor
GuiFilterOptionNode::GuiFilterOptionNode()
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Private variables
	controlType_ = GuiFilterOptionNode::nGuiControlTypes;
	returnType_ = VTypes::NoData;
	widget_ = NULL;
	widgetParentType_ = GuiFilterOptionNode::NoParent;
	widgetParentSpan_ = 2;
	widgetSpan_ = 1;
	widgetLabelSpan_ = 1;
	widgetNewLine_ = FALSE;
	widgetEnabled_ = TRUE;
}

// Destructor
GuiFilterOptionNode::~GuiFilterOptionNode()
{
}

// Set return value
void GuiFilterOptionNode::setReturnValue(const ReturnValue &rv)
{
	returnValue_ = rv;
}

// Set argument list from parser-joined treenodes
bool GuiFilterOptionNode::addJoinedArguments(TreeNode *arglist)
{
	msg.enter("GuiFilterOptionNode::addJoinedArguments");
	// From supplied argument list (which contains items in reverse order as passed from the parser) get rest of data...
	TreeNode *arg;
	for (arg = arglist; arg != NULL; arg = arg->prevArgument) if (arg->prevArgument == NULL) break;
	ReturnValue rv;

	// First argument is the control name
	if (arg == NULL)
	{
		msg.print("Error: No control name specified for GUI filter option.\n");
		msg.exit("GuiFilterOptionNode::addJoinedArguments");
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
		msg.exit("GuiFilterOptionNode::addJoinedArguments");
		return FALSE;
	}
	arg->execute(rv);
	controlType_ = GuiFilterOptionNode::guiControlType(rv.asString(), TRUE);
	if (controlType_ == GuiFilterOptionNode::nGuiControlTypes)
	{
		msg.exit("GuiFilterOptionNode::addJoinedArguments");
		return FALSE;
	}
	msg.print(Messenger::Parse, "GUI filter option control type = '%s'\n", GuiFilterOptionNode::guiControlType(controlType_));

	// Set basic return type based on control type
	switch (controlType_)
	{
		case (GuiFilterOptionNode::EditType):
		case (GuiFilterOptionNode::ComboType):
			returnType_ = VTypes::StringData;
			break;
		case (GuiFilterOptionNode::DoubleSpinType):
			returnType_ = VTypes::DoubleData;
			break;
		case (GuiFilterOptionNode::CheckType):
		case (GuiFilterOptionNode::SpinType):
		case (GuiFilterOptionNode::IntegerComboType):
			returnType_ = VTypes::IntegerData;
			break;
	}

	// Next arguments are specific to the control type
	bool result = FALSE;
	arg = arg->nextArgument;
	switch (controlType_)
	{
		// Check Box - option("Title", "check", int state)
		case (GuiFilterOptionNode::CheckType):
			if (!setData("state", arg, "Error: No initial state supplied for 'check' GUI filter option.\n", TRUE, "")) break;
			result = TRUE;
			break;
		// Combo Box - option("Title", "combo", "<csv itemlist>", int default=1)
		case (GuiFilterOptionNode::IntegerComboType):
		case (GuiFilterOptionNode::ComboType):
			if (!setData("items", arg, "Error: No items list supplied for 'combo' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			setData("default", arg, "No default value supplied for 'combo' GUI filter option - '1' assumed.\n", TRUE, "1");
			result = TRUE;
			break;
		// Double Spin Edit - option("Title", "spin", double min, double max, double start, double step)
		case (GuiFilterOptionNode::DoubleSpinType):
			if (!setData("min", arg, "Error: No minimum value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("max", arg, "Error: No maximum value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("start", arg, "Error: No starting value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("step", arg, "Error: No step value supplied for 'doublespin' GUI filter option.\n", TRUE, "")) break;
			result = TRUE;
			break;
		// Spin Edit - option("Title", "edit", string text = "")
		case (GuiFilterOptionNode::EditType):
			setData("text", arg, "No default string supplied for 'edit' GUI filter option - empty string assumed.\n", TRUE, "");
			result = TRUE;
			break;
		// Spin Edit - option("Title", "spin", int min, int max, int start, int step)
		case (GuiFilterOptionNode::SpinType):
			if (!setData("min", arg, "Error: No minimum value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("max", arg, "Error: No maximum value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("start", arg, "Error: No starting value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			arg = arg->nextArgument;
			if (!setData("step", arg, "Error: No step value supplied for 'spin' GUI filter option.\n", TRUE, "")) break;
			result = TRUE;
			break;
		default:
			printf("Internal Error: Setting arguments for control type '%s' has not been implemented.\n", GuiFilterOptionNode::guiControlType(controlType_));
			break;
	}
	// Any remaining arguments are options of the format opt=arg
	for (arg = arg->nextArgument; arg != NULL; arg = arg->nextArgument) setOption(arg);
	msg.exit("GuiFilterOptionNode::addJoinedArguments");
	return result;
}

// Return type of GUI control
GuiFilterOptionNode::GuiControlType GuiFilterOptionNode::controlType()
{
	return controlType_;
}

// Return name of option
const char *GuiFilterOptionNode::name()
{
	return name_.get();
}

// Set associated data
bool GuiFilterOptionNode::setData(const char *name, TreeNode *arg, const char *errormsg, bool critical, const char *def)
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
bool GuiFilterOptionNode::data(const char *name, Dnchar &value)
{
	bool success;
	value = data_.value(name, success);
	return success;
}

// Set option from argument
void GuiFilterOptionNode::setOption(TreeNode *arg)
{
	ReturnValue rv;
	arg->execute(rv);
	Dnchar keywd = beforeChar(rv.asString(), '=');
	Dnchar argdata = afterChar(rv.asString(), '=');
	// Determine option enum
	GuiFilterOptionNode::GuiQtOption gqo = GuiFilterOptionNode::guiQtOption(keywd.get(), TRUE);
	if (gqo == GuiFilterOptionNode::nGuiQtOptions) return;
	// Set relevant data
	switch (gqo)
	{
		case (GuiFilterOptionNode::DisabledOption):
			widgetEnabled_ = FALSE;
			break;
		case (GuiFilterOptionNode::GroupNameOption):
			widgetParentType_ = GuiFilterOptionNode::GroupBoxParent;
			widgetParentName_ = argdata;
			break;
		case (GuiFilterOptionNode::LabelSpanOption):
			widgetLabelSpan_ = argdata.asInteger();
			break;
		case (GuiFilterOptionNode::NewLineOption):
			widgetNewLine_ = TRUE;
			break;
		case (GuiFilterOptionNode::ParentSpanOption):
			widgetParentSpan_ = argdata.asInteger();
			break;
		case (GuiFilterOptionNode::SpanOption):
			widgetSpan_ = argdata.asInteger();
			break;
		case (GuiFilterOptionNode::TabsOption):
			widgetParentType_ = GuiFilterOptionNode::TabWidgetParent;
			widgetParentName_ = argdata;	// In format 'page@tabwidget'
			break;
		default:
			printf("Internal Error: Don't know how to set GuiQtOption '%s'\n", GuiFilterOptionNode::guiQtOption(gqo));
			break;
	}
}

// Return whether a parent exists
GuiFilterOptionNode::GuiWidgetParent GuiFilterOptionNode::widgetParentType()
{
	return widgetParentType_;
}

// Return parent name
const char *GuiFilterOptionNode::widgetParentName()
{
	return widgetParentName_.get();
}

// Return parent span
int GuiFilterOptionNode::widgetParentSpan()
{
	return widgetParentSpan_;
}

// Return label span
int GuiFilterOptionNode::widgetLabelSpan()
{
	return widgetLabelSpan_;
}

// Return widget span
int GuiFilterOptionNode::widgetSpan()
{
	return widgetSpan_;
}

// Return newline flag
bool GuiFilterOptionNode::widgetNewLine()
{
	return widgetNewLine_;
}

// Set widget pointer
void GuiFilterOptionNode::setWidget(QWidget *w)
{
	widget_ = w;
}

// Return widget pointer
QWidget *GuiFilterOptionNode::widget()
{
	return widget_;
}

// Return whether the widget is enabled
bool GuiFilterOptionNode::widgetEnabled()
{
	return widgetEnabled_;
}

/*
// Inherited Virtuals
*/

// Execute command
bool GuiFilterOptionNode::execute(ReturnValue &rv)
{
	msg.enter("GuiFilterOptionNode::execute");
	// Just copy local returnvalue contents
	rv = returnValue_;
	msg.exit("GuiFilterOptionNode::execute");
	return TRUE;
}

// Print node contents
void GuiFilterOptionNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("[CN]%s%s (GUIFO)\n", tab, name_.get());
	delete[] tab;
}

// Set from returnvalue node
bool GuiFilterOptionNode::set(ReturnValue &setrv)
{
	msg.enter("GuiFilterOptionNode::set");
	returnValue_ = setrv;
	msg.exit("GuiFilterOptionNode::set");
	return TRUE;
}

// Initialise node
bool GuiFilterOptionNode::initialise()
{
	returnValue_.reset();
	return TRUE;
}
