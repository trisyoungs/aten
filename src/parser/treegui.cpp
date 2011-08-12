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
// Widget Event Action
*/

// Constructor
TreeGuiWidgetEventAction::TreeGuiWidgetEventAction()
{
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
TreeGuiWidgetEventAction::~TreeGuiWidgetEventAction()
{
}

/*
// Widget Event
*/

// Constructor / Destructor
TreeGuiWidgetEvent::TreeGuiWidgetEvent()
{
	// Private variables
	type_ = TreeGuiWidgetEvent::nEventQualifiers;

	// Public variables
	prev = NULL;
	next = NULL;
}

TreeGuiWidgetEvent::~TreeGuiWidgetEvent()
{
}

// Set integer qualifying event
void TreeGuiWidgetEvent::setQualifier(int min, int max)
{
	type_ = TreeGuiWidgetEvent::IntegerQualifier;
	minimumI_ = min;
	maximumI_ = max;
}

// Set double qualifying event
void TreeGuiWidgetEvent::setQualifier(double min, double max)
{
	type_ = TreeGuiWidgetEvent::DoubleQualifier;
	minimumD_ = min;
	maximumD_ = max;
}

// Set string qualifying event
void TreeGuiWidgetEvent::setQualifier(const char *s)
{
	type_ = TreeGuiWidgetEvent::StringQualifier;
	matchS_ = s;
}

/*
// TreeGuiWidget
*/

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
	itemsChanged_ = FALSE;
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
bool TreeGuiWidget::setProperties(int min, int max, int value)
{
	// Check / set limits
	if (max < min)
	{
		msg.print("Error setting widget integer properties: max > min.\n");
		return FALSE;
	}
	minimumI_ = min;
	maximumI_ = max;
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
bool TreeGuiWidget::setProperties(double min, double max, double value)
{
	// Check / set limits
	if (max < min)
	{
		msg.print("Error setting widget double properties: max > min.\n");
		return FALSE;
	}
	minimumD_ = min;
	maximumD_ = max;
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
bool TreeGuiWidget::setProperties(const char *s)
{
	text_ = s;
}

// Add text item
void TreeGuiWidget::addItem(const char *s)
{
	Dnchar *d = items_.add();
	d->set(s);
	itemsChanged_= TRUE;
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
int TreeGuiWidget::minimumD()
{
	return minimumD_;
}

// Return current double maximum
int TreeGuiWidget::maximumD()
{
	return maximumD_;
}

// Return current double value
int TreeGuiWidget::valueD()
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

// Return whether items list has recently changed (since last Qt update)
bool TreeGuiWidget::itemsChanged()
{
	return itemsChanged_;
}

// Reset items changed flag
void TreeGuiWidget::resetItemsChanged()
{
	itemsChanged_ = FALSE;
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

// Add widget to the layout in this widget (if it has one) at specified geometry, returning added widget for convenience
TreeGuiWidget *TreeGuiWidget::addWidget(TreeGuiWidget *widget, int l, int r, int addToWidth, int addToHeight)
{
	// Do we have an associated Qt widget? If not, then we have no GUI, so nothing to do.
	if (qtWidgetObject_ == NULL) return widget;

	// Check widget type - does it have a layout?
	switch (type_)
	{
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::PageWidget):
		case (TreeGuiWidget::GroupWidget):
			qtWidgetObject_->addWidget(widget, l, r, addToWidth, addToHeight);
			break;
		default:
			msg.print("Error: Widget '%s' does not have a layout, and so addWidget() cannot be used.\n", name_.get());
			return NULL;
			break;
	}
	return widget;
}

// Create new radio button (only for RadioGroupWidget)
TreeGuiWidget *TreeGuiWidget::addRadioButton(const char *name, const char *label, int state)
{
	// Is this a RadioGroup widget?
	if (type_ != TreeGuiWidget::RadioGroupWidget)
	{
		msg.print("Error: Attempted to add a radiobutton to a non-radiogroup widget (%s).\n", name_.get());
		return NULL;
	}
	// Create the new button and add it to the local list
	TreeGuiWidget *radio = parent_->addButtonToGroup(name, label, this, buttonList_.nItems()+1);
	if (radio) buttonList_.add(radio, buttonList_.nItems()+1);
	return radio;
}

// Create new page (only in tab widget)
TreeGuiWidget *TreeGuiWidget::addPage(const char* name, const char* label)
{
	// Is this a tab widget?
	if (type_ != TreeGuiWidget::TabWidget)
	{
		msg.print("Error: Attempted to add a page to a non-tab widget (%s).\n", name_.get());
		return NULL;
	}
	return parent_->addPageToTab(name, label, this);
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
			result =valueI_;
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
			msg.print("Warning: Converting integer value to double when retrieving doublespin widget (%s).\n", name_.get());
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
			result = items_[valueI_]->get();
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

// Set widget value from integer (and perform events)
bool TreeGuiWidget::setValue(int i)
{
	// Success or the action we perform depends on the widget type
	switch (type_)
	{
		case (TreeGuiWidget::DoubleSpinWidget):
			msg.print("Warning: Converting integer value to double when setting doublespin widget (%s).\n", name_.get());
		case (TreeGuiWidget::RadioGroupWidget):
		case (TreeGuiWidget::CheckWidget):
		case (TreeGuiWidget::ComboWidget):
		case (TreeGuiWidget::IntegerSpinWidget):
		case (TreeGuiWidget::RadioButtonWidget):
		case (TreeGuiWidget::TabWidget):
		case (TreeGuiWidget::StackWidget):
			// Value must be within integer ranges
			if (!isGoodValue(i)) return FALSE;
			valueI_ = i;
			break;
		case (TreeGuiWidget::EditWidget):
		case (TreeGuiWidget::LabelWidget):
			msg.print("Warning: Converting integer value to string when setting widget value (%s).\n", name_.get());
			text_ = itoa(i);
			break;
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::PageWidget):
			msg.print("Warning: Don't know what to do with an integer value for this widget (%s).\n", name_.get());
			break;
		default:
			printf("Internal Error: Control type not accounted for in TreeGuiWidget::setValue(int).\n");
			return FALSE;
	}
	
	// Update associated Qt control
	if (qtWidgetObject_ != NULL) qtWidgetObject_->update();
	
	// Check widget events
	checkWidgetEvents();
}

// Set widget value from double (and perform events)
bool TreeGuiWidget::setValue(double d)
{
	// Success or the action we perform depends on the widget type
	switch (type_)
	{
		case (TreeGuiWidget::RadioGroupWidget):
		case (TreeGuiWidget::CheckWidget):
		case (TreeGuiWidget::ComboWidget):
		case (TreeGuiWidget::IntegerSpinWidget):
		case (TreeGuiWidget::RadioButtonWidget):
		case (TreeGuiWidget::TabWidget):
		case (TreeGuiWidget::StackWidget):
			msg.print("Warning: Converting double value to integer when setting widget value (%s).\n", name_.get());
		case (TreeGuiWidget::DoubleSpinWidget):
			// Value must be within double ranges
			if (!isGoodValue(d)) return FALSE;
			valueD_ = d;
			break;
		case (TreeGuiWidget::EditWidget):
		case (TreeGuiWidget::LabelWidget):
			msg.print("Warning: Converting double value to string when setting widget value (%s).\n", name_.get());
			text_ = ftoa(d);
			break;
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::PageWidget):
			msg.print("Warning: Don't know what to do with a double value for this widget (%s).\n", name_.get());
			break;
		default:
			printf("Internal Error: Control type not accounted for in TreeGuiWidget::setValue(double).\n");
			return FALSE;
	}
	
	// Update associated Qt control
	if (qtWidgetObject_ != NULL) qtWidgetObject_->update();
	
	// Check widget events
	checkWidgetEvents();
}

// Set widget value from character string (and perform events)
bool TreeGuiWidget::setValue(const char *s)
{
	// Success or the action we perform depends on the widget type
	int i;
	switch (type_)
	{
		case (TreeGuiWidget::RadioGroupWidget):
		case (TreeGuiWidget::CheckWidget):
			
		case (TreeGuiWidget::IntegerSpinWidget):
		case (TreeGuiWidget::RadioButtonWidget):
		case (TreeGuiWidget::TabWidget):
		case (TreeGuiWidget::StackWidget):
		case (TreeGuiWidget::DoubleSpinWidget):
			msg.print("Warning: Converting string value to integer when setting widget value (%s).\n", name_.get());
			// Value must be within double ranges
			i = atoi(s);
			if (!isGoodValue(i)) return FALSE;
			valueI_ = i;
			break;
		case (TreeGuiWidget::ComboWidget):
			// For combobox widget, search the items list...
			for (i = 0; i < items_.nItems(); ++i) if (*(items_[i]) == s) break;
			if (i < items_.nItems()) valueI_ = i;
			else
			{
				msg.print("Error: '%s' is not a valid value for this combo (%s).\n", s, name_.get());
				return FALSE;
			}
			break;
		case (TreeGuiWidget::EditWidget):
		case (TreeGuiWidget::LabelWidget):
			text_ = s;
			break;
		case (TreeGuiWidget::DialogWidget):
		case (TreeGuiWidget::PageWidget):
			msg.print("Warning: Don't know what to do with a double value for this widget (%s).\n", name_.get());
			break;
		default:
			printf("Internal Error: Control type not accounted for in TreeGuiWidget::setValue(double).\n");
			return FALSE;
	}
	
	// Update associated Qt control
	if (qtWidgetObject_ != NULL) qtWidgetObject_->update();
	
	// Check widget events
	checkWidgetEvents();
}

// Check widget's events and act on them if necessary
void TreeGuiWidget::checkWidgetEvents()
{
	XXX
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
		qtTreeGui_ = new AtenTreeGuiDialog;
		setQtWidgetObject(qtTreeGui_->addDialogLayout(this));
	}
	else qtTreeGui_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
TreeGui::~TreeGui()
{
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
	widget->set(type, name, this);
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
	
// Create new combo widget
TreeGuiWidget *TreeGui::addCombo(const char *name, const char *label, const char *items, int index)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::ComboWidget);
	if (widget == NULL) return NULL;
	// Parse items list
	LineParser parser;
	parser.getArgsDelim(LineParser::UseQuotes, items);
	if (parser.nArgs() == 0) msg.print("Warning: Combo box created with no items.\n");
	else
	{
		for (int n=0; n<parser.nArgs(); ++n) widget->addItem(parser.argc(n));
		widget->setProperties(1, parser.nArgs(), index);
	}
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addCombo(widget, label));
	return widget;
}

// Create new integer spin widget
TreeGuiWidget *TreeGui::addIntegerSpin(const char *name, const char *label, int min, int max, int step, int value)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::IntegerSpinWidget);
	if (widget == NULL) return NULL;
	// Set control limits
	if (!widget->setProperties(min, max, value))
	{
		msg.print("Error when setting up integer spin widget '%s'.\n", name);
		return NULL;
	}
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addIntegerSpin(widget, label, step));
	return widget;
}

// Create new double spin widget
TreeGuiWidget *TreeGui::addDoubleSpin(const char *name, const char *label, double min, double max, double step, double value)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::DoubleSpinWidget);
	if (widget == NULL) return NULL;
	// Set control limits
	if (!widget->setProperties(min, max, value)) msg.print("Error when setting up double spin widget '%s'.\n", name);
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addIntegerSpin(widget, label, step));
	return widget;
}

// Create new label widget
TreeGuiWidget *TreeGui::addLabel(const char *text)
{
	TreeGuiWidget *widget = widgets_.add();
	widget->set(TreeGuiWidget::LabelWidget, "_LABEL_", this);
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addLabel(widget, text));
	return widget;
}

// Create new edit widget
TreeGuiWidget *TreeGui::addEdit(const char *name, const char *label, const char *text)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::EditWidget);
	if (widget == NULL) return NULL;
	// Set control properties
	if (!widget->setProperties(text)) msg.print("Error when setting up edit widget '%s'.\n", name);
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addEdit(widget, label));
	return widget;
}

// Create new checkbox widget
TreeGuiWidget *TreeGui::addCheck(const char *name, const char *label, int state)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::CheckWidget);
	if (widget == NULL) return NULL;
	// Set control limits
	if (!widget->setProperties(0, 1, state))
	{
		msg.print("Error when setting up check widget '%s'.\n", name);
		return NULL;
	}
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addCheck(widget, label));
	return widget;
}

// Create new tab widget
TreeGuiWidget *TreeGui::addTabs(const char *name)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::TabWidget);
	if (widget == NULL) return NULL;
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addTabs(widget));
	return widget;
}

// Create new group box
TreeGuiWidget *TreeGui::addGroup(const char *name, const char *label)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::GroupWidget);
	if (widget == NULL) return NULL;
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addGroup(widget, label));
	return widget;
}

// Create new (invisible) radio group
TreeGuiWidget *TreeGui::addRadioGroup(const char *name)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::RadioGroupWidget);
	if (widget == NULL) return NULL;
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addRadioGroup(widget));
	return widget;
}

// Create new page in specified tab (called by TreeGuiWidget)
TreeGuiWidget *TreeGui::addPageToTab(const char *name, const char *label, TreeGuiWidget *tabWidget)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::PageWidget);
	if (widget == NULL) return NULL;
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addPage(widget, tabWidget, label));
	return widget;
}

// Create new radio button in specified radio group (called by TreeGuiWidget)
TreeGuiWidget *TreeGui::addButtonToGroup(const char *name, const char *label, TreeGuiWidget *groupWidget, int buttonId)
{
	TreeGuiWidget *widget = createWidget(name, TreeGuiWidget::RadioButtonWidget);
	if (widget == NULL) return NULL;
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQtWidgetObject(qtTreeGui_->addRadioButton(widget, groupWidget, name, label, buttonId));
	return widget;
}

// Set named widget's value from integer
bool TreeGui::setWidgetValue(const char *name, int i)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: Can't set the value of widget named '%s' since it doesn't exist.\n", name);
		return FALSE;
	}
	return widget->setValue(i);
}

// Set named widget's value from double
bool TreeGui::setWidgetValue(const char* name, double d)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: Can't set the value of widget named '%s' since it doesn't exist.\n", name);
		return FALSE;
	}
	return widget->setValue(d);
}

// Set named widget's value from string
bool TreeGui::setWidgetValue(const char* name, const char* s)
{
	// Find named widget
	TreeGuiWidget *widget = findWidget(name);
	if (widget == NULL)
	{
		msg.print("Error: Can't set the value of widget named '%s' since it doesn't exist.\n", name);
		return FALSE;
	}
	return widget->setValue(s);
}

// Return value in named widget as integer
int TreeGui::asInteger(const char *name)
{
	XXX
}

// Return value in named widget as double
double TreeGui::asDouble(const char *name)
{
	XXX
}

// Return value in named widget as character string
const char *TreeGui::asCharacter(const char *name)
{
	XXX
}

// Return values in named widgets as Vec3<double>
Vec3<double> TreeGui::asVec3(const char *name1, const char *name2, const char *name3)
{
	XXX
}

// Show Qt dialog (if it exists)
bool TreeGui::execute()
{
	if ((widgets_.nItems() > 0) && (qtTreeGui_ != NULL)) return qtTreeGui_->execute();
	else return TRUE;
}

// // Add new (GUI-based) widget linked to a variable
// TreeNode *TreeGui::addWidget(TreeNode *arglist)
// {
// 	msg.enter("TreeGui::addWidget");
// 	// Wrap the variable and add it to the arguments_ list
// 	WidgetNode *node = new WidgetNode();
// 	node->setParent(this);
// 	// Store in reflist also...
// 	widgets_.add(node);
// 	// Add arguments to node (also sets return type)
// 	if (node->addJoinedArguments(arglist)) msg.print(Messenger::Parse, "Added GUI widget '%s'...\n", node->name());
// 	else
// 	{
// 		msg.print("Failed to add GUI widget.\n");
// 		msg.exit("TreeGui::addWidget");
// 		return NULL;
// 	}
// 	msg.exit("TreeGui::addWidget");
// 	return node;
// }
// 
// // Return first item in list of widgets
// Refitem<WidgetNode,int> *TreeGui::widgets()
// {
// 	return widgets_.first();
// }
// 
// // Create custom dialog from defined widgets
// void TreeGui::createCustomDialog(const char *title)
// {
// 	if (gui.applicationType() != QApplication::Tty)
// 	{
// 		customDialog_ = new AtenCustomDialog(NULL);
// 		customDialog_->createWidgets(title, this);
// 	}
// }
// 
// // Return custom dialog (if any)
// AtenCustomDialog *TreeGui::customDialog()
// {
// 	return customDialog_;
// }
// 
// // Execute contained custom dialog
//  bool TreeGui::executeCustomDialog(bool getvaluesonly, const char *newtitle)
// {
// 	if (customDialog_ == NULL) return TRUE;
// 	// Retitle dialog?
// 	if (newtitle) customDialog_->setWindowTitle(newtitle);
// 	if (getvaluesonly)
// 	{
// 		customDialog_->storeValues();
// 		return TRUE;
// 	}
// 	return customDialog_->showDialog();
// }
/*
// Locate named widget
WidgetNode *TreeGui::findWidget(const char *name)
{
	for (Refitem<WidgetNode,int> *ri = widgets_.first(); ri != NULL; ri = ri->next)
	{
		if (strcmp(name, ri->item->name()) == 0) return ri->item;
	}
	printf("Internal Error: Couldn't find widget named '%s' in tree '%s'.\n", name, name_.get());
	return NULL;
}

// Locate named widget
WidgetNode *TreeGui::findWidget(QWidget *widget)
{
	for (Refitem<WidgetNode,int> *ri = widgets_.first(); ri != NULL; ri = ri->next) if (ri->item->widget() == widget) return ri->item;
	printf("Internal Error: Couldn't find widget %p in tree '%s'.\n", widget, name_.get());
	return NULL;
}

// Locate widget with specified object pointer
WidgetNode *TreeGui::findWidgetObject(QObject *obj)
{	
	for (Refitem<WidgetNode,int> *ri = widgets_.first(); ri != NULL; ri = ri->next) if (ri->item->object() == obj) return ri->item;
	printf("Internal Error: Couldn't find widget %p in tree '%s'.\n", obj, name_.get());
	return NULL;
}

// Retrieve current value of named widget as a double
double TreeGui::widgetValued(const char *name)
{
	WidgetNode *node = findWidget(name);
	if (node == NULL) return 0.0;
	ReturnValue rv;
	node->execute(rv);
	return rv.asDouble();
}

// Retrieve current value of named widget as an integer
int TreeGui::widgetValuei(const char *name)
{
	WidgetNode *node = findWidget(name);
	if (node == NULL) return 0;
	ReturnValue rv;
	node->execute(rv);
	return rv.asInteger();
}

// Retrieve current value of named widget as a string
const char *TreeGui::widgetValuec(const char *name)
{
	WidgetNode *node = findWidget(name);
	if (node == NULL) return "NULL";
	static ReturnValue rv;
	node->execute(rv);
	return rv.asString();
}

// Retrieve current value of named widget triplet as a vector
Vec3<double> TreeGui::widgetValue3d(const char *name1, const char *name2, const char *name3)
{
	ReturnValue rv;
	Vec3<double> result;
	// First value
	WidgetNode *node = findWidget(name1);
	if (node == NULL) result.x = 0.0;
	node->execute(rv);
	result.x = rv.asDouble();
	// Second value
	node = findWidget(name2);
	if (node == NULL) result.y = 0.0;
	node->execute(rv);
	result.y = rv.asDouble();
	// Third value
	node = findWidget(name3);
	if (node == NULL) result.z = 0.0;
	node->execute(rv);
	result.z = rv.asDouble();
	return result;
}

// Set current value of named widget
void TreeGui::setWidgetValue(const char *name, ReturnValue value)
{
	WidgetNode *node = findWidget(name);
	if (node == NULL) return;
	node->setWidgetValue(value);
}

// Set property of named widget (via a state change)
bool TreeGui::setWidgetProperty(const char *name, const char *property, ReturnValue value)
{
	// First, find named widget
	WidgetNode *node = findWidget(name);
	if (node == NULL) return FALSE;
	// Next, find state change property
	StateChange::StateAction action = StateChange::stateAction(property, TRUE);
	if (action == StateChange::nStateActions) return FALSE;
	StateChange sc;
	sc.setTargetWidget(name);
	sc.setChange(action, value.asString());
	customDialog_->performStateChange(&sc);
	return TRUE;
}*/
