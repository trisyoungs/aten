/*
	*** Tree GUI for CLI
	*** src/parser/treegui.cpp
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

#include "gui/treegui.h"
#include "parser/treegui.h"
#include "base/sysfunc.h"

/*
// Widget Event
*/

// Event types
const char *EventTypeKeywords[TreeGuiWidgetEvent::nEventTypes] = { "activate", "function", "sendbool", "senddouble", "sendinteger", "sendstring", "set" };
TreeGuiWidgetEvent::EventType TreeGuiWidgetEvent::eventType(const char *s, bool reportError)
{
	TreeGuiWidgetEvent::EventType et = (TreeGuiWidgetEvent::EventType) enumSearch("event type", TreeGuiWidgetEvent::nEventTypes, EventTypeKeywords, s, reportError);
	if ((et == TreeGuiWidgetEvent::nEventTypes) && reportError) enumPrintValid(TreeGuiWidgetEvent::nEventTypes,EventTypeKeywords);
	return et;
}
const char *TreeGuiWidgetEvent::eventType(TreeGuiWidgetEvent::EventType i)
{
	return EventTypeKeywords[i];
}

// Event properties
const char *EventPropertyKeywords[TreeGuiWidgetEvent::nEventProperties] = { "disabled", "enabled", "invisible", "items", "maximum", "minimum", "text", "value", "visible" };
TreeGuiWidgetEvent::EventProperty TreeGuiWidgetEvent::eventProperty(const char *s, bool reportError)
{
	TreeGuiWidgetEvent::EventProperty ep = (TreeGuiWidgetEvent::EventProperty) enumSearch("event property", TreeGuiWidgetEvent::nEventProperties, EventPropertyKeywords, s, reportError);
	if ((ep == TreeGuiWidgetEvent::nEventProperties) && reportError) enumPrintValid(TreeGuiWidgetEvent::nEventProperties,EventPropertyKeywords);
	return ep;
}
const char *TreeGuiWidgetEvent::eventProperty(TreeGuiWidgetEvent::EventProperty i)
{
	return EventPropertyKeywords[i];
}

// Constructor / Destructor
TreeGuiWidgetEvent::TreeGuiWidgetEvent()
{
	// Private variables
	qualifier_ = TreeGuiWidgetEvent::nEventQualifiers;
	type_ = TreeGuiWidgetEvent::nEventTypes;
	targetWidget_ = NULL;
	targetProperty_ = TreeGuiWidgetEvent::nEventProperties;

	// Public variables
	prev = NULL;
	next = NULL;
}

TreeGuiWidgetEvent::~TreeGuiWidgetEvent()
{
}

// Return qualifier type
TreeGuiWidgetEvent::EventQualifier TreeGuiWidgetEvent::qualifier()
{
	return qualifier_;
}

// Return event type
TreeGuiWidgetEvent::EventType TreeGuiWidgetEvent::type()
{
	return type_;
}

// Return event property
TreeGuiWidgetEvent::EventProperty TreeGuiWidgetEvent::targetProperty()
{
	return targetProperty_;
}

// Return target widget
TreeGuiWidget *TreeGuiWidgetEvent::targetWidget()
{
	return targetWidget_;
}

// Set integer qualifying event
void TreeGuiWidgetEvent::setQualifiers(int min, int max)
{
	qualifier_ = TreeGuiWidgetEvent::IntegerQualifier;
	minimumI_ = min;
	maximumI_ = max;
}

// Set double qualifying event
void TreeGuiWidgetEvent::setQualifiers(double min, double max)
{
	qualifier_ = TreeGuiWidgetEvent::DoubleQualifier;
	minimumD_ = min;
	maximumD_ = max;
}

// Set string qualifying event (comma-separated list)
void TreeGuiWidgetEvent::setQualifiers(const char *s)
{
	qualifier_ = TreeGuiWidgetEvent::StringQualifier;
	LineParser parser;
	parser.getArgsDelim(LineParser::UseQuotes, s);
	for (int n=0; n<parser.nArgs(); ++n)
	{
		Dnchar *d = matchS_.add();
		d->set(parser.argc(n));
	}
}

// Set remaining event data
void TreeGuiWidgetEvent::setEventData(TreeGuiWidgetEvent::EventType type, TreeGuiWidget *targetwidget, TreeGuiWidgetEvent::EventProperty property)
{
	type_ = type;
	targetWidget_ = targetwidget;
	targetProperty_ = property;
}

// Add send data to event
ReturnValue *TreeGuiWidgetEvent::addSendValue()
{
	return sendValues_.add();
}

// Return number of send values defined
int TreeGuiWidgetEvent::nSendValues()
{
	return sendValues_.nItems();
}

// Return first send value in list
ReturnValue *TreeGuiWidgetEvent::firstSendValue()
{
	return sendValues_.first();
}

// Return relevant send data based on supplied widget value
ReturnValue *TreeGuiWidgetEvent::sendValue(int widgetValue)
{
	if ((qualifier_ != TreeGuiWidgetEvent::IntegerQualifier) || sendValues_.nItems() == 1) return sendValues_[0];
	else
	{
		// Dumb check - is the supplied value in range?
		if (qualifies(widgetValue)) return sendValues_[widgetValue-minimumI_];
		else printf("Internal Error: Tried to get a sendValue for a widgetValue which does not qualify.\n");
		return NULL;
	}
}

// Return whether supplied integer value qualifies
bool TreeGuiWidgetEvent::qualifies(int i)
{
	if (qualifier_ != TreeGuiWidgetEvent::IntegerQualifier)
	{
		printf("Internal Error: Tried to qualify a value that isn't an integer.\n");
		return FALSE;
	}
	if ((i < minimumI_) || (i > maximumI_)) return FALSE;
	return TRUE;
}

// Return whether supplied double value qualifies
bool TreeGuiWidgetEvent::qualifies(double d)
{
	if (qualifier_ != TreeGuiWidgetEvent::DoubleQualifier)
	{
		printf("Internal Error: Tried to qualify a value that isn't a double.\n");
		return FALSE;
	}
	if ((d < minimumD_) || (d > maximumD_)) return FALSE;
	return TRUE;
}

// Return whether supplied character value qualifies
bool TreeGuiWidgetEvent::qualifies(const char *s)
{
	if (qualifier_ != TreeGuiWidgetEvent::StringQualifier)
	{
		printf("Internal Error: Tried to qualify a value that isn't a string.\n");
		return FALSE;
	}
	// Go through list of matches
	for (Dnchar *d = matchS_.first(); d != NULL; d = d->next) if (*d == s) return TRUE;
	return FALSE;
}

/*
// TreeGuiWidget
*/

// Event properties
const char *WidgetTypeKeywords[TreeGuiWidget::nWidgetTypes] = { "button", "check", "combo", "dialog", "doublespin", "edit", "frame", "group", "integerspin", "label", "page", "radiobutton", "radiogroup", "stack", "tab" };
TreeGuiWidget::WidgetType TreeGuiWidget::widgetType(const char *s, bool reportError)
{
	TreeGuiWidget::WidgetType wt = (TreeGuiWidget::WidgetType) enumSearch("widget type", TreeGuiWidget::nWidgetTypes, WidgetTypeKeywords, s, reportError);
	if ((wt == TreeGuiWidget::nWidgetTypes) && reportError) enumPrintValid(TreeGuiWidget::nWidgetTypes,WidgetTypeKeywords);
	return wt;
}
const char *TreeGuiWidget::widgetType(TreeGuiWidget::WidgetType i)
{
	return WidgetTypeKeywords[i];
}

// Constructor
TreeGuiWidget::TreeGuiWidget()
{
	// Private variables
	type_ = TreeGuiWidget::nWidgetTypes;
	qtWidgetObject_ = NULL;
	parent_ = NULL;
	minimumI_ = 0;
	maximumI_ = 0;
	valueI_ = 0;
	minimumD_ = 0.0;
	maximumD_ = 0.0;
	valueD_ = 0.0;
	for (int n = 0; n < TreeGuiWidgetEvent::nEventProperties; ++n) propertyChanged_[n] = FALSE;
	enabled_ = TRUE;
	visible_ = TRUE;
	
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
TreeGuiWidget::~TreeGuiWidget()
{
}

// Set widget type, name, and parent
void TreeGuiWidget::set(TreeGuiWidget::WidgetType type, const char *name, TreeGui *parent)
{
	type_ = type;
	name_ = name;
	parent_ = parent;
}

// Return widget type
TreeGuiWidget::WidgetType TreeGuiWidget::type()
{
	return type_;
}

// Return widget name
const char *TreeGuiWidget::name()
{
	return name_.get();
}

// Return widget parent
TreeGui *TreeGuiWidget::parent()
{
	return parent_;
}

// Set corresponding Qt QWidget
void TreeGuiWidget::setQtWidgetObject(QtWidgetObject *wo)
{
	qtWidgetObject_ = wo;
}

// Return associated qtWidgetObject
QtWidgetObject *TreeGuiWidget::qtWidgetObject()
{
	return qtWidgetObject_;
}

// Set integer properties
bool TreeGuiWidget::setInitialProperties(int minval, int maxval, int value)
{
	// Check / set limits
	if (maxval < minval)
	{
		msg.print("Error setting widget integer properties: max > min.\n");
		return FALSE;
	}
	minimumI_ = minval;
	maximumI_ = maxval;
	// Check / set value
	if ((value < minimumI_) || (value  > maximumI_))
	{
		msg.print("Warning: Value given when setting widget integer properties is out of defined limits (%i to %i) and has been reset to %i.\n", value, minimumI_, maximumI_, minimumI_);
		valueI_ = minimumI_;
	}
	else valueI_ = value;
	return TRUE;
}

// Set double properties
bool TreeGuiWidget::setInitialProperties(double minval, double maxval, double value)
{
	// Check / set limits
	if (maxval < minval)
	{
		msg.print("Error setting widget double properties: max > min.\n");
		return FALSE;
	}
	minimumD_ = minval;
	maximumD_ = maxval;
	// Check / set value
	if ((value < minimumD_) || (value  > maximumD_))
	{
		msg.print("Warning: Value given when setting widget double properties is out of defined limits (%f to %f) and has been reset to %f.\n", value, minimumD_, maximumD_, minimumD_);
		valueD_ = minimumD_;
	}
	else valueD_ = value;
	return TRUE;
}

// Set string properties
bool TreeGuiWidget::setInitialProperties(const char *s)
{
	text_ = s;
	return TRUE;
}

// Add text item
void TreeGuiWidget::addItem(const char *s)
{
	Dnchar *d = items_.add();
	d->set(s);
	propertyChanged_[TreeGuiWidgetEvent::ItemsProperty] = TRUE;
}

// Return number of defined items
int TreeGuiWidget::nItems()
{
	return items_.nItems();
}

// Return whether integer value is within range
bool TreeGuiWidget::isGoodValue(int i, bool printError)
{
	bool result = ((i >= minimumI_) && (i <= maximumI_));
	if (!result && printError) msg.print("Error: Value %i is out of range for widget '%s'.\n\tValid range is %i to %i.\n", i, name_.get(), minimumI_, maximumI_);
	return result;
}

// Return whether double value is within range
bool TreeGuiWidget::isGoodValue(double d, bool printError)
{
	bool result = ((d >= minimumD_) && (d <= maximumD_));
	if (!result && printError) msg.print("Error: Value %f is out of range for widget '%s'.\n\tValid range is %f to %f.\n", d, name_.get(), minimumD_, maximumD_);
	return result;
}

// Return whether string is in list (returning index or 0 for FALSE)
int TreeGuiWidget::isInList(const char* s, bool printError)
{
	// Search through items list
	int result = 0;
	for (Dnchar *d = items_.first(); d != NULL; d = d->next)
	{
		++result;
		if (*d == s) return result;
	}
	return 0;
}

// Return current integer minimum
int TreeGuiWidget::minimumI()
{
	return minimumI_;
}

// Return current integer maximum
int TreeGuiWidget::maximumI()
{
	return maximumI_;
}

// Return current integer value
int TreeGuiWidget::valueI()
{
	return valueI_;
}

// Return current double minimum
double TreeGuiWidget::minimumD()
{
	return minimumD_;
}

// Return current double maximum
double TreeGuiWidget::maximumD()
{
	return maximumD_;
}

// Return current double value
double TreeGuiWidget::valueD()
{
	return valueD_;
}

// Return current text
const char *TreeGuiWidget::text()
{
	return text_.get();
}

// Return head of items list
Dnchar *TreeGuiWidget::items()
{
	return items_.first();
}

// Set whether widget is enabled
void TreeGuiWidget::setEnabled(bool b)
{
	enabled_ = b;
}

// Return whether widget is enabled
bool TreeGuiWidget::enabled()
{
	return enabled_;
}

// Set whether widget is visible
void TreeGuiWidget::setVisible(bool b)
{
	visible_ = b;
}

// Return whether widget is visible
bool TreeGuiWidget::visible()
{
	return visible_;
}

// Set specified property
bool TreeGuiWidget::setProperty(TreeGuiWidgetEvent::EventProperty property, ReturnValue rv)
{
	msg.enter("TreeGuiWidget::setProperty");
	// Do generic properties which are applicable to any widget type first
	bool done = TRUE;
	if (property == TreeGuiWidgetEvent::DisabledProperty)
	{
		enabled_ = !rv.asBool();
		propertyChanged_[TreeGuiWidgetEvent::EnabledProperty] = TRUE;
	}
	else if (property == TreeGuiWidgetEvent::EnabledProperty)
	{
		enabled_ = rv.asBool();
		propertyChanged_[TreeGuiWidgetEvent::EnabledProperty] = TRUE;
	}
	else if (property == TreeGuiWidgetEvent::InvisibleProperty)
	{
		visible_ = !rv.asBool();
		propertyChanged_[TreeGuiWidgetEvent::VisibleProperty] = TRUE;
	}
	else if (property == TreeGuiWidgetEvent::VisibleProperty)
	{
		visible_ = rv.asBool();
		propertyChanged_[TreeGuiWidgetEvent::VisibleProperty] = TRUE;
	}
	else done = FALSE;
	if (done)
	{
		if (qtWidgetObject_ != NULL) qtWidgetObject_->updateQt();
		checkWidgetEvents();
		msg.exit("TreeGuiWidget::setProperty");
		return TRUE;
	}

	// All other properties must be checked against widget type
	done = TRUE;
	switch (type_)
	{
		// Widgets for which only 'text' value is valid
		case (TreeGuiWidget::DialogWidget):
			if (property == TreeGuiWidgetEvent::TextProperty)
			{
				text_ = rv.asString();
				msg.exit("TreeGuiWidget::setProperty");
				return TRUE;
			}
			else done = FALSE;
			break;
		case (TreeGuiWidget::LabelWidget):
		case (TreeGuiWidget::GroupWidget):
			if (property == TreeGuiWidgetEvent::TextProperty)
			{
				text_ = rv.asString();
				propertyChanged_[TreeGuiWidgetEvent::TextProperty] = TRUE;
			}
			else done = FALSE;
			break;
		// Edit widget - 'text' and 'value' do the same thing
		case (TreeGuiWidget::EditWidget):
			if ((property == TreeGuiWidgetEvent::TextProperty) || (property == TreeGuiWidgetEvent::ValueProperty))
			{
				text_ = rv.asString();
				propertyChanged_[TreeGuiWidgetEvent::TextProperty] = TRUE;
			}
			else done = FALSE;
			break;
		// Widgets for which only 'value' property is valid
		case (TreeGuiWidget::ButtonWidget):
		case (TreeGuiWidget::CheckWidget):
		case (TreeGuiWidget::RadioButtonWidget):
		case (TreeGuiWidget::RadioGroupWidget):
		case (TreeGuiWidget::TabWidget):
		case (TreeGuiWidget::StackWidget):
			if (property == TreeGuiWidgetEvent::ValueProperty)
			{
				// Check range
				int i = rv.asInteger();
				if (!isGoodValue(i))
				{
					msg.print("Dialog Warning: Value (%i) is out of range for this widget (%s) whose minimum/maximum values are %i and %i respectively. Value reset to %i.\n", i, name_.get(), minimumI_, maximumI_, i < minimumI_ ? minimumI_ : maximumI_);
					i = i < minimumI_ ? minimumI_ : maximumI_;
				}
				valueI_ = i;
				propertyChanged_[TreeGuiWidgetEvent::ValueProperty] = TRUE;
			}
			else done = FALSE;
			break;
		// Spin widgets
		case (TreeGuiWidget::IntegerSpinWidget):
		case (TreeGuiWidget::DoubleSpinWidget):
			if (property == TreeGuiWidgetEvent::ValueProperty)
			{
				// Check range
				if ((type_ == TreeGuiWidget::IntegerSpinWidget) && (!isGoodValue(rv.asInteger())))
				{
					msg.print("Dialog Warning: Value (%i) is out of range for this widget (%s) whose minimum/maximum values are %i and %i respectively. Value reset to %i.\n", rv.asInteger(), name_.get(), minimumI_, maximumI_, rv.asInteger() < minimumI_ ? minimumI_ : maximumI_);
					rv = rv.asInteger() < minimumI_ ? minimumI_ : maximumI_;
				}
				else if ((type_ == TreeGuiWidget::DoubleSpinWidget) && (!isGoodValue(rv.asDouble())))
				{
					msg.print("Dialog Warning: Value (%f) is out of range for this widget (%s) whose minimum/maximum values are %f and %f respectively. Value reset to %f.\n", rv.asDouble(), name_.get(), minimumD_, maximumD_, rv.asDouble() < minimumD_ ? minimumD_ : maximumD_);
					rv = rv.asDouble() < minimumD_ ? minimumD_ : maximumD_;
				}
				if (type_ == TreeGuiWidget::IntegerSpinWidget) valueI_ = rv.asInteger();
				else valueD_ = rv.asDouble();
				propertyChanged_[TreeGuiWidgetEvent::ValueProperty] = TRUE;
			}
			else if (property == TreeGuiWidgetEvent::MinimumProperty)
			{
				if ((type_ == TreeGuiWidget::IntegerSpinWidget) && (rv.asInteger() > maximumI_))
				{
					msg.print("Dialog Warning: Minimum (%i) is higher than the current maximum (%i) for this widget (%s). Value reset to %i.\n", rv.asInteger(), name_.get(), maximumI_, maximumI_);
					rv = maximumI_;
				}
				else if ((type_ == TreeGuiWidget::DoubleSpinWidget) && (rv.asDouble() > maximumD_))
				{
					msg.print("Dialog Warning: Minimum (%f) is higher than the current maximum (%f) for this widget (%s). Value reset to %f.\n", rv.asDouble(), name_.get(), maximumD_, maximumD_);
					rv = maximumD_;
				}
				if (type_ == TreeGuiWidget::IntegerSpinWidget) minimumI_ = rv.asInteger();
				else minimumD_ = rv.asDouble();
				propertyChanged_[TreeGuiWidgetEvent::MinimumProperty] = TRUE;
				// Check value is still in range
				if ((type_ == TreeGuiWidget::IntegerSpinWidget) && (valueI_ < minimumI_))
				{
					valueI_ = minimumI_;
					msg.print("Value of widget %s altered to conform to new min/max value.\n", name_.get());
					propertyChanged_[TreeGuiWidgetEvent::ValueProperty] = TRUE;
				}
				else if ((type_ == TreeGuiWidget::DoubleSpinWidget) && (valueD_ < minimumD_))
				{
					valueD_ = minimumD_;
					msg.print("Value of widget %s altered to conform to new min/max value.\n", name_.get());
					propertyChanged_[TreeGuiWidgetEvent::ValueProperty] = TRUE;
				}
			}
			else if (property == TreeGuiWidgetEvent::MaximumProperty)
			{
				if ((type_ == TreeGuiWidget::IntegerSpinWidget) && (rv.asInteger() < minimumI_))
				{
					msg.print("Dialog Warning: Maximum (%i) is lower than the current minimum (%i) for this widget (%s). Value reset to %i.\n", rv.asInteger(), name_.get(), minimumI_, minimumI_);
					rv = minimumI_;
				}
				else if ((type_ == TreeGuiWidget::DoubleSpinWidget) && (rv.asDouble() < minimumD_))
				{
					msg.print("Dialog Warning: Maximum (%f) is lower than the current minimum (%f) for this widget (%s). Value reset to %f.\n", rv.asDouble(), name_.get(), minimumD_, minimumD_);
					rv = minimumD_;
				}
				if (type_ == TreeGuiWidget::IntegerSpinWidget) maximumI_ = rv.asInteger();
				else maximumD_ = rv.asDouble();
				propertyChanged_[TreeGuiWidgetEvent::MaximumProperty] = TRUE;
				// Check value is still in range
				if ((type_ == TreeGuiWidget::IntegerSpinWidget) && (valueI_ > maximumI_))
				{
					valueI_ = maximumI_;
					msg.print("Value of widget %s altered to conform to new min/max value.\n", name_.get());
					propertyChanged_[TreeGuiWidgetEvent::ValueProperty] = TRUE;
				}
				else if ((type_ == TreeGuiWidget::DoubleSpinWidget) && (valueD_ > maximumD_))
				{
					valueD_ = maximumD_;
					msg.print("Value of widget %s altered to conform to new min/max value.\n", name_.get());
					propertyChanged_[TreeGuiWidgetEvent::ValueProperty] = TRUE;
				}
			}
			else done = FALSE;
			break;
		// Combo box widget
		case (TreeGuiWidget::ComboWidget):
			if (property == TreeGuiWidgetEvent::ValueProperty)
			{
				// Set depending on variable type stored in ReturnValue
				int i;
				if (rv.type() == VTypes::StringData)
				{
					// Search for stored string in items list
					for (i = 0; i<items_.nItems(); ++i) if (*(items_[i]) == rv.asString()) break;
					if (i == items_.nItems())
					{
						msg.print("Dialog Error: Item '%s' does not exist in the combo '%s'.\nValid values are:\n\t", rv.asString(), name_.get());
						for (Dnchar *d = items_.first(); d != NULL; d = d->next)
						{
							if (d->next == NULL) msg.print("\"%s\"\n", d->get());
							else msg.print("\"%s\", ", d->get());
						}
						msg.exit("TreeGuiWidget::setProperty");
						return FALSE;
					}
					// Increase 'i' to conform to 1-N range
					++i;
				}
				else i = rv.asInteger();
				
				// Check range
				if (!isGoodValue(i))
				{
					msg.print("Dialog Warning: Value (%i) is out of range for this widget (%s) whose minimum/maximum values are %i and %i respectively. Value reset to %i.\n", i, name_.get(), minimumI_, maximumI_, i < minimumI_ ? minimumI_ : maximumI_);
					i = i < minimumI_ ? minimumI_ : maximumI_;
				}
				valueI_ = i;
				propertyChanged_[TreeGuiWidgetEvent::ValueProperty] = TRUE;
			}
			else if (property == TreeGuiWidgetEvent::ItemsProperty)
			{
				// Clear old items list
				items_.clear();
				// Parse new items list
				LineParser parser;
				parser.getArgsDelim(LineParser::UseQuotes, rv.asString());
				if (parser.nArgs() == 0) msg.print("Dialog Warning: Recreated combo items list is empty.\n");
				else
				{
					for (int n=0; n<parser.nArgs(); ++n) addItem(parser.argc(n));
					propertyChanged_[TreeGuiWidgetEvent::ItemsProperty] = TRUE;
					maximumI_ = parser.nArgs();
					propertyChanged_[TreeGuiWidgetEvent::MaximumProperty] = TRUE;
					if (valueI_ > maximumI_)
					{
						valueI_ = 1;
						propertyChanged_[TreeGuiWidgetEvent::ValueProperty] = TRUE;
					}
				}
			}
			else done = FALSE;
			break;
		// Widgets which have no other settable properties
		case (TreeGuiWidget::FrameWidget):
		case (TreeGuiWidget::PageWidget):
			done = FALSE;
			break;
		default:
			printf("Internal Error: Widget type '%s' has not been accounted for in TreeGuiWidget::setProperty.\n", TreeGuiWidget::widgetType(type_));
			break;
	}

	// Did we succeed?
	if (!done)
	{
		msg.print("Cannot set '%s' property for a widget of type '%s' (%s)).\n", TreeGuiWidgetEvent::eventProperty(property), TreeGuiWidget::widgetType(type_), name_.get());
		msg.exit("TreeGuiWidget::setProperty");
		return FALSE;
	}
	else if (qtWidgetObject_ != NULL) qtWidgetObject_->updateQt();
	checkWidgetEvents();

	msg.exit("TreeGuiWidget::setProperty");
	return TRUE;
}

// 'Activate' property, setting flag and executing events, but leaving value unchanged
bool TreeGuiWidget::activateProperty(TreeGuiWidgetEvent::EventProperty property)
{
	msg.enter("TreeGuiWidget::activateProperty");
	propertyChanged_[property] = TRUE;
	if (qtWidgetObject_ != NULL) qtWidgetObject_->updateQt();
	checkWidgetEvents();
	
	// If this was a radiogroup, activate the current button as well
	if ((type_ == TreeGuiWidget::RadioGroupWidget) && (property == TreeGuiWidgetEvent::ValueProperty))
	{
		Refitem<TreeGuiWidget,int> *button = buttonList_[valueI_-1];
		if (button == NULL)
		{
			msg.exit("TreeGuiWidget::activateProperty");
			return FALSE;
		}
		button->item->activateProperty(TreeGuiWidgetEvent::ValueProperty);
	}
	msg.exit("TreeGuiWidget::activateProperty");
	return TRUE;
}

// Return whether specified property has changed
bool TreeGuiWidget::propertyChanged(TreeGuiWidgetEvent::EventProperty property)
{
	return propertyChanged_[property];
}

// Reset changed flag for specified property
void TreeGuiWidget::resetChanged(TreeGuiWidgetEvent::EventProperty property)
{
	propertyChanged_[property] = FALSE;
}

// Add widget to the layout in this widget (if it has one) at specified geometry, returning added widget for convenience
void TreeGuiWidget::addWidget(TreeGuiWidget *widget, int left, int top, int addToWidth, int addToHeight)
{
	msg.enter("TreeGuiWidget::addWidget");
	// Do we have an associated Qt widget? If not, then we have no GUI, so nothing to do.
	if (qtWidgetObject_ == NULL)
	{
		msg.exit("TreeGuiWidget::addWidget");
		return;
	}
	
	// If l and r are both zero, this widget should not be added to any layout
	if ((left == 0) && (top == 0))
	{
		msg.exit("TreeGuiWidget::addWidget");
		return;
	}

	// Check widget type - does it have a layout?
	switch (type_)
	{
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::FrameWidget):
		case (TreeGuiWidget::GroupWidget):
		case (TreeGuiWidget::PageWidget):
			qtWidgetObject_->addWidget(widget->qtWidgetObject(), left, top, addToWidth, addToHeight);
			break;
		default:
			msg.print("Error: Widget '%s' does not have a layout, and so addWidget() cannot be used.\n", name_.get());
			break;
	}
	msg.exit("TreeGuiWidget::addWidget");
}

// Create new checkbox widget
TreeGuiWidget *TreeGuiWidget::addCheck(const char *name, const char *label, int state, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addCheck");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::CheckWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addCheck");
		return NULL;
	}

	// Set control limits
	if (!widget->setInitialProperties(0, 1, state))
	{
		msg.print("Error when setting up check widget '%s'.\n", name);
		msg.exit("TreeGuiWidget::addCheck");
		return NULL;
	}
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addCheck(widget, label));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addCheck");
	return widget;
}

// Create new combo widget
TreeGuiWidget *TreeGuiWidget::addCombo(const char *name, const char *label, const char *items, int index, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addCombo");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::ComboWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addCombo");
		return NULL;
	}
	
	// Parse items list
	LineParser parser;
	parser.getArgsDelim(LineParser::UseQuotes, items);
	if (parser.nArgs() == 0) msg.print("Warning: Combo box created with no items.\n");
	else
	{
		for (int n=0; n<parser.nArgs(); ++n) widget->addItem(parser.argc(n));
		widget->setInitialProperties(1, parser.nArgs(), index);
	}
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addCombo(widget, label));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addCombo");
	return widget;
}

// Create new double spin widget
TreeGuiWidget *TreeGuiWidget::addDoubleSpin(const char *name, const char *label, double min, double max, double step, double value, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addDoubleSpin");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::DoubleSpinWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addDoubleSpin");
		return NULL;
	}
	
	// Set control limits
	if (!widget->setInitialProperties(min, max, value)) msg.print("Error when setting up double spin widget '%s'.\n", name);
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addDoubleSpin(widget, label, step));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addDoubleSpin");
	return widget;
}

// Create new edit widget
TreeGuiWidget *TreeGuiWidget::addEdit(const char *name, const char *label, const char *text, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addEdit");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::EditWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addEdit");
		return NULL;
	}
	
	// Set control properties
	if (!widget->setInitialProperties(text)) msg.print("Error when setting up edit widget '%s'.\n", name);
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addEdit(widget, label));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addEdit");
	return widget;
}

// Create new frame widget
TreeGuiWidget *TreeGuiWidget::addFrame(const char *name, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addFrame");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::FrameWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addFrame");
		return NULL;
	}
	
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addFrame(widget));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addFrame");
	return widget;
}

// Create new group box
TreeGuiWidget *TreeGuiWidget::addGroup(const char *name, const char *label, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addGroup");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::GroupWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addGroup");
		return NULL;
	}
	
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addGroup(widget, label));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addGroup");
	return widget;
}

// Create new integer spin widget
TreeGuiWidget *TreeGuiWidget::addIntegerSpin(const char *name, const char *label, int min, int max, int step, int value, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addIntegerSpin");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::IntegerSpinWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addIntegerSpin");
		return NULL;
	}
	
	// Set control limits
	if (!widget->setInitialProperties(min, max, value))
	{
		msg.print("Error when setting up integer spin widget '%s'.\n", name);
		return NULL;
	}
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addIntegerSpin(widget, label, step));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addIntegerSpin");
	return widget;
}

// Create new label widget
TreeGuiWidget *TreeGuiWidget::addLabel(const char *name, const char *text, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addLabel");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::LabelWidget);
	widget->setInitialProperties(text);
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL) 
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addLabel(widget, text));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addLabel");
	return widget;
}

// Create new page (only in TabWidget and StackWidget)
TreeGuiWidget *TreeGuiWidget::addPage(const char *name, const char *label)
{
	msg.enter("TreeGuiWidget::addPag");
	// Is this a tab widget?
	if ((type_ != TreeGuiWidget::TabWidget) && (type_ != TreeGuiWidget::StackWidget))
	{
		msg.print("Error: Attempted to add a page to a widget that isn't a tab or a stack (%s).\n", name_.get());
		msg.exit("TreeGuiWidget::addPage");
		return NULL;
	}
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::PageWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addPage");
		return NULL;
	}
	
	// Update minimum and maximum
	setInitialProperties(1, maximumI_+1, valueI_ == 0 ? 1 : valueI_);
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL) widget->setQtWidgetObject(parent_->qtTreeGui()->addPage(widget, this, label));
	msg.exit("TreeGuiWidget::addPage");
	return widget;
}

// Create new radio button
TreeGuiWidget *TreeGuiWidget::addRadioButton(const char *name, const char *label, const char *radioGroup, int state, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addRadioButton");
	// Find named radioGroup
	TreeGuiWidget *group = parent_->findWidget(radioGroup);
	if (group == NULL)
	{
		msg.print("Error: Couldn't find radiogroup named '%s'.\n", radioGroup);
		msg.exit("TreeGuiWidget::addRadioButton");
		return NULL;
	}
	if (group->type() != TreeGuiWidget::RadioGroupWidget)
	{
		msg.print("Error: Attempted to add a radiobutton to a non-radiogroup widget (%s).\n", radioGroup);
		msg.exit("TreeGuiWidget::addRadioButton");
		return NULL;
	}

	// Create the new button and add it to the local list
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::RadioButtonWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addRadioButton");
		return NULL;
	}
	widget->setInitialProperties(0, 1, state);
	int id = group->buttonList_.nItems()+1;
	group->buttonList_.add(widget, id);
	group->setInitialProperties(1, id, state == 1 ? id : (group->valueI_ == 0 ? 1 : group->valueI_));

	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addRadioButton(widget, group, name, label, id));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addRadioButton");
	return widget;
}

// Create new (invisible) radio group
TreeGuiWidget *TreeGuiWidget::addRadioGroup(const char *name)
{
	msg.enter("TreeGuiWidget::addRadioGroup");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::RadioGroupWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addRadioGroup");
		return NULL;
	}
	
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL) widget->setQtWidgetObject(parent_->qtTreeGui()->addRadioGroup(widget));
	msg.exit("TreeGuiWidget::addRadioGroup");
	return widget;
}

// Create new spacer
bool TreeGuiWidget::addSpacer(bool expandHorizontal, bool expandVertical, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addSpacer");
	// Do we have an associated Qt widget? If not, then we have no GUI, so nothing to do.
	if (qtWidgetObject_ == NULL)
	{
		msg.exit("TreeGuiWidget::addSpacer");
		return NULL;
	}
	
	// Check widget type - does it have a layout?
	bool result = FALSE;
	switch (type_)
	{
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::FrameWidget):
		case (TreeGuiWidget::GroupWidget):
		case (TreeGuiWidget::PageWidget):
			result = qtWidgetObject_->addSpacer(expandHorizontal, expandVertical, l, t, xw, xh);
			break;
		default:
			msg.print("Error: Widget '%s' does not have a layout, and so addSpacer() cannot be used.\n", name_.get());
			result = FALSE;
			break;
	}
	msg.exit("TreeGuiWidget::addSpacer");
	return result;
}

// Create new stack widget
TreeGuiWidget *TreeGuiWidget::addStack(const char *name, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addStack");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::StackWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addStack");
		return NULL;
	}
	
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addStack(widget));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addStack");
	return widget;
}

// Create new tab widget
TreeGuiWidget *TreeGuiWidget::addTabs(const char *name, int l, int t, int xw, int xh)
{
	msg.enter("TreeGuiWidget::addTabs");
	TreeGuiWidget *widget = parent_->createWidget(name, TreeGuiWidget::TabWidget);
	if (widget == NULL)
	{
		msg.exit("TreeGuiWidget::addTabs");
		return NULL;
	}
	
	// Create complementary Qt control?
	if (parent_->qtTreeGui() != NULL)
	{
		widget->setQtWidgetObject(parent_->qtTreeGui()->addTabs(widget));
		addWidget(widget, l, t, xw, xh);
	}
	msg.exit("TreeGuiWidget::addTabs");
	return widget;
}

// Return widget value as integer
int TreeGuiWidget::asInteger()
{
	int result;
	switch (type_)
	{
		case (TreeGuiWidget::DoubleSpinWidget):
			msg.print("Warning: Converting double value to integer when retrieving doublespin widget's value (%s).\n", name_.get());
			result = int(valueD_);
			break;
		case (TreeGuiWidget::RadioGroupWidget):
		case (TreeGuiWidget::CheckWidget):
		case (TreeGuiWidget::ComboWidget):
		case (TreeGuiWidget::IntegerSpinWidget):
		case (TreeGuiWidget::RadioButtonWidget):
		case (TreeGuiWidget::TabWidget):
		case (TreeGuiWidget::StackWidget):
			result = valueI_;
			break;
		case (TreeGuiWidget::EditWidget):
		case (TreeGuiWidget::LabelWidget):
			msg.print("Warning: Converting string value to integer when retrieving widget value (%s).\n", name_.get());
			result = atoi(text_.get());
			break;
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::PageWidget):
			msg.print("Warning: Don't know how to retrieve an integer value for this widget (%s).\n", name_.get());
			result = 0;
			break;
		default:
			printf("Internal Error: Control type not accounted for in TreeGuiWidget::asInteger.\n");
			result = 0;
			break;
	}
	return result;
}

// Return widget value as double
double TreeGuiWidget::asDouble()
{
	double result;
	switch (type_)
	{
		case (TreeGuiWidget::DoubleSpinWidget):
			result = valueD_;
			break;
		case (TreeGuiWidget::RadioGroupWidget):
		case (TreeGuiWidget::CheckWidget):
		case (TreeGuiWidget::ComboWidget):
		case (TreeGuiWidget::IntegerSpinWidget):
		case (TreeGuiWidget::RadioButtonWidget):
		case (TreeGuiWidget::TabWidget):
		case (TreeGuiWidget::StackWidget):
			msg.print("Warning: Converting integer value to double when retrieving widget value (%s).\n", name_.get());
			result = double(valueI_);
			break;
		case (TreeGuiWidget::EditWidget):
		case (TreeGuiWidget::LabelWidget):
			msg.print("Warning: Converting string value to double when retrieving widget value (%s).\n", name_.get());
			result = atof(text_.get());
			break;
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::PageWidget):
			msg.print("Warning: Don't know how to retrieve a double value for this widget (%s).\n", name_.get());
			result = 0.0;
			break;
		default:
			printf("Internal Error: Control type not accounted for in TreeGuiWidget::asDouble.\n");
			result = 0.0;
			break;
	}
	return result;
}

// Return widget value as character string
const char *TreeGuiWidget::asCharacter()
{
	static Dnchar result;
	switch (type_)
	{
		case (TreeGuiWidget::DoubleSpinWidget):
			msg.print("Warning: Converting double value to string when retrieving doublespin widget (%s).\n", name_.get());
			result = ftoa(valueD_);
			break;
		case (TreeGuiWidget::RadioGroupWidget):
		case (TreeGuiWidget::CheckWidget):
		case (TreeGuiWidget::IntegerSpinWidget):
		case (TreeGuiWidget::RadioButtonWidget):
		case (TreeGuiWidget::TabWidget):
		case (TreeGuiWidget::StackWidget):
			msg.print("Warning: Converting integer value to string when retrieving doublespin widget (%s).\n", name_.get());
			result = itoa(valueI_);
			break;
		case (TreeGuiWidget::ComboWidget):
			result = items_[valueI_-1]->get();
			break;
		case (TreeGuiWidget::EditWidget):
		case (TreeGuiWidget::LabelWidget):
			result = text_;
			break;
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::PageWidget):
			msg.print("Warning: Don't know how to retrieve a string value for this widget (%s).\n", name_.get());
			result = "NULL";
			break;
		default:
			printf("Internal Error: Control type not accounted for in TreeGuiWidget::asCharacter.\n");
			result = "NULL";
			break;
	}
	return result.get();
}

// Add widget event
TreeGuiWidgetEvent *TreeGuiWidget::addEvent(TreeGuiWidgetEvent::EventType type, TreeGuiWidget *targetWidget, TreeGuiWidgetEvent::EventProperty property)
{
	TreeGuiWidgetEvent *event = events_.add();
	event->setEventData(type, targetWidget, property);
	return event;
}

// Check widget's events and act on them if necessary
void TreeGuiWidget::checkWidgetEvents()
{
	for (TreeGuiWidgetEvent *event = events_.first(); event != NULL; event = event->next)
	{
		// Check the type of event, and then check the widget's current value
		bool qualifies;
		if (event->qualifier() == TreeGuiWidgetEvent::IntegerQualifier) qualifies = event->qualifies(asInteger());
		else if (event->qualifier() == TreeGuiWidgetEvent::DoubleQualifier) qualifies = event->qualifies(asDouble());
		else qualifies = event->qualifies(asCharacter());
		
		// Check for valid target widget
		TreeGuiWidget *targetWidget = event->targetWidget();
		if (targetWidget == NULL)
		{
			printf("Internal Error: Target widget in event is NULL.\n");
			return;
		}

		// Should this event be performed
		if (qualifies)
		{
			// Set widget property, the value depending on the type of event
			switch (event->type())
			{
				case (TreeGuiWidgetEvent::ActivateType):
					targetWidget->activateProperty(event->targetProperty());
					break;
				case (TreeGuiWidgetEvent::SendBoolType):
					targetWidget->setProperty(event->targetProperty(), TRUE);
					break;
				case (TreeGuiWidgetEvent::SendDoubleType):
					if (event->nSendValues() == 0) targetWidget->setProperty(event->targetProperty(), asDouble());
					else targetWidget->setProperty(event->targetProperty(), event->firstSendValue()->asDouble());
					break;
				case (TreeGuiWidgetEvent::SendIntegerType):
					if (event->nSendValues() == 0) targetWidget->setProperty(event->targetProperty(), asInteger());
					else if (event->nSendValues() == 1) targetWidget->setProperty(event->targetProperty(), event->firstSendValue()->asInteger());
					else targetWidget->setProperty(event->targetProperty(), *event->sendValue(asInteger()));
					break;
				case (TreeGuiWidgetEvent::SendStringType):
					if (event->nSendValues() == 0) targetWidget->setProperty(event->targetProperty(), asCharacter());
					else targetWidget->setProperty(event->targetProperty(), event->firstSendValue()->asString());
					break;
				case (TreeGuiWidgetEvent::SetPropertyType):
					// Check 'range' of event integer
					targetWidget->setProperty(event->targetProperty(), *event->sendValue(asInteger()));
					break;
				default:
					printf("Internal Error: Event type not recognised in TreeGuiWidget::checkWidgetEvents().\n");
					break;
			}
		}
		else if (event->type() == TreeGuiWidgetEvent::SendBoolType)
		{
			targetWidget->setProperty(event->targetProperty(), FALSE);
		}
	}
}

/*
// TreeGui
*/

// Constructor
TreeGui::TreeGui() : TreeGuiWidget()
{
	// Private variables
	if (gui.applicationType() != QApplication::Tty)
	{
		qtTreeGui_ = new AtenTreeGuiDialog(this);
		setQtWidgetObject(qtTreeGui_->addDialogLayout(this));
	}
	else qtTreeGui_ = NULL;
	set(TreeGuiWidget::DialogWidget, "DialogWidget", this);
	
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
TreeGui::~TreeGui()
{
}

// Return qtTreeGui pointer
AtenTreeGuiDialog *TreeGui::qtTreeGui()
{
	return qtTreeGui_;
}

// Create new widget of specified type
TreeGuiWidget *TreeGui::createWidget(const char *name, TreeGuiWidget::WidgetType type)
{
	// Does this name already exist?
	if (findWidget(name))
	{
		msg.print("Error: A widget named '%s' already exists in the dialog, and cannot be duplicated.\n", name);
		return NULL;
	}
	TreeGuiWidget *widget = widgets_.add();
	// If a blank name was given, construct one
	if (strcmp(name,"") == 0)
	{
		Dnchar newName(-1,"widget%03i", widgets_.nItems());
		widget->set(type, newName, this);
	}
	else widget->set(type, name, this);
	return widget;
}

// Return number of defined widgets in GUI
int TreeGui::nWidgets()
{
	return widgets_.nItems();
}

// Search for named widget
TreeGuiWidget *TreeGui::findWidget(const char *name)
{
	for (TreeGuiWidget *result = widgets_.first(); result != NULL; result = result->next) if (strcmp(name,result->name()) == 0) return result;
	return NULL;
}

// List available widget names, types, and values
void TreeGui::listWidgets()
{
	// For neatness, determine longest widget name in list...
	TreeGuiWidget *widget;
	int maxLen = 0, n;
	for (widget = widgets_.first(); widget != NULL; widget = widget->next)
	{
		n = strlen(widget->name());
		maxLen = max(maxLen, n);
	}

	// Construct formatting string for output
	Dnchar fmt(-1, "%%-%is      %%-14s    ", maxLen);
// 	printf("Formatting string is [%s]\n", fmt.get());
	Dnchar s(1000);
	for (widget = widgets_.first(); widget != NULL; widget = widget->next)
	{
		s.sprintf(fmt.get(), widget->name(), TreeGuiWidget::widgetType(widget->type()));
		switch (widget->type())
		{
			case (TreeGuiWidget::DoubleSpinWidget):
				s.strcatf("min=%f, max=%f, value=%f", widget->minimumD(), widget->maximumD(), widget->valueD()); 
				break;
			case (TreeGuiWidget::IntegerSpinWidget):
				s.strcatf("min=%i, max=%i, value=%i", widget->minimumI(), widget->maximumI(), widget->valueI()); 
				break;
			case (TreeGuiWidget::CheckWidget):
			case (TreeGuiWidget::RadioButtonWidget):
				s.strcatf("value(index)=%i (%s)", widget->valueI(), widget->valueI() ? "checked" : "unchecked");
				break;
			case (TreeGuiWidget::RadioGroupWidget):
			case (TreeGuiWidget::TabWidget):
			case (TreeGuiWidget::StackWidget):
				s.strcatf("value(index)=%i", widget->valueI());
				break;
			case (TreeGuiWidget::ComboWidget):
				s.strcatf("value(index)=%i, value(string)='%s', values=(", widget->valueI(), widget->asCharacter());
				for (Dnchar *item = widget->items(); item != NULL; item = item->next) s.strcatf("'%s'%s", item->get(), item->next != NULL ? ", " : ")");
				break;
			case (TreeGuiWidget::EditWidget):
				s.strcatf("value(string)='%s'", widget->asCharacter());
				break;
			default:
				// Nothing to print...
				continue;
		}
		msg.print("%s\n", s.get());
	}
}

// Set named widget's value from integer
bool TreeGui::setWidgetValue(const char *name, int i)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling setWidgetValue).\n", name);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	return widget->setProperty(TreeGuiWidgetEvent::ValueProperty, i);
}

// Set named widget's value from double
bool TreeGui::setWidgetValue(const char* name, double d)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling setWidgetValue).\n", name);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	return widget->setProperty(TreeGuiWidgetEvent::ValueProperty, d);
}

// Set named widget's value from string
bool TreeGui::setWidgetValue(const char* name, const char* s)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling setWidgetValue).\n", name);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	return widget->setProperty(TreeGuiWidgetEvent::ValueProperty, s);
}

// Return value in named widget as integer
int TreeGui::asInteger(const char *name)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling asInteger).\n", name);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	return widget->asInteger();
}

// Return value in named widget as double
double TreeGui::asDouble(const char *name)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling asDouble).\n", name);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	return widget->asDouble();
}

// Return value in named widget as character string
const char *TreeGui::asCharacter(const char *name)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling asCharacter).\n", name);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	return widget->asCharacter();
}

// Return values in named widgets as Vec3<double>
Vec3<double> TreeGui::asVec3(const char *name1, const char *name2, const char *name3)
{
	Vec3<double> v;
	// Find first named widget
	TreeGuiWidget *widget = findWidget(name1);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling asVector).\n", name1);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	v.x = widget->asDouble();
	// Find first named widget
	widget = findWidget(name2);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling asVector).\n", name2);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	v.y = widget->asDouble();
	// Find first named widget
	widget = findWidget(name3);
	if (widget == NULL)
	{
		msg.print("Error: No widget named '%s' exists in this dialog (calling asVector).\n", name3);
		msg.print("Valid widgets are:\n");
		listWidgets();
		return FALSE;
	}
	v.z = widget->asDouble();
	return v;
}

// Show Qt dialog (if it exists)
bool TreeGui::execute()
{
// 	printf("EXECUTING TREE DIALOG %p nWidgets = %i, qtTreeGui_ = %p\n", this, widgets_.nItems(), qtTreeGui_);
	if ((widgets_.nItems() > 0) && (qtTreeGui_ != NULL)) return qtTreeGui_->execute(text());
	else return TRUE;
}
