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
TreeGuiWidgetEventAction~TreeGuiWidgetEventAction()
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
	XXX
}

/*
// TreeGuiWidget
*/

// Constructor
TreeGuiWidget::TreeGuiWidget()
{
	// Private variables
	type_ = TreeGuiWidget::nWidgetTypes;
	qWidget_ = NULL;
	qObject_ = NULL;
	parent_ = NULL;
	minimumI_ = 0;
	maximumI_ = 0;
	valueI_ = 0;
	minimumD_ = 0.0;
	maximumD_ = 0.0;
	valueD_ = 0.0;
	itemsChanged_ = FALSE;
	enabled_ = TRUE;
	
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
void TreeGuiWidget::setQWidget(QWidget *widget)
{
	qWidget_ = widget;
}

// Set corresponding Qt QObject
void TreeGuiWidget::setQObject(QObject *object)
{
	qObject_ = object;
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
	if ((value < minimumI_) || (value  > maximumI_))
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
	bool result = ((i >= minimumD_) && (i <= maximumD_));
	if (!result && printError) msg.print("Error: Value %f is out of range for widget '%s'.\n\tValid range is %f to %f.\n", i, name_.get(), minimumD_, maximumD_);
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

// Add widget to the layout in this widget (if it has one) at specified geometry, returning added widget for convenience
TreeGuiWidget *TreeGuiWidget::addWidget(TreeGuiWidget *widget, int l, int r, int addToWidth = 0, int addToHeight = 0)
{
}

// Add button to button group (only valid for ButtonGroupWidget);
TreeGuiWidget *TreeGuiWidget::addButton(TreeGuiWidget *widget)
{
}
	
// Return widget value as integer
int TreeGuiWidget::asInteger()
{
}

// Return widget value as double
double TreeGuiWidget::asDouble()
{
}

// Return widget value as character string
const char *TreeGuiWidget::asCharacter()
{
}

// Set widget value from integer (and perform events)
bool TreeGuiWidget::setValue(int i)
{
	// Success or the action we perform depends on the widget type
	switch (type_)
	{
		case (TreeGuiWidget::ButtonGroupWidget):
			// Value must be within integer ranges
			if (!isGoodValue(i)) return FALSE;
			valueI_ = i;
			break;
		case (TreeGuiWidget::CheckWidget):
			break;
		case (TreeGuiWidget::ComboWidget):
			break;
		case (TreeGuiWidget::DialogWidget):
			break;
		case (TreeGuiWidget::DoubleSpinWidget):
			break;
		case (TreeGuiWidget::EditWidget):
			break;
		case (TreeGuiWidget::IntegerSpinWidget):
			break;
		case (TreeGuiWidget::LabelWidget):
			break;
		case (TreeGuiWidget::PageWidget):
			break;
 		case (TreeGuiWidget::RadioButtonWidget):
			break;
		case (TreeGuiWidget::StackWidget):
			break;
		case (TreeGuiWidget::TabWidget):
			break;
		default:
			printf("Internal Error: Control type not accounted for in TreeGuiWidget::setValue(int).\n");
			return FALSE;
	}
	
	// Update associated Qt control
	if (parent_ != NULL) w
}

// Set widget value from double (and perform events)
bool TreeGuiWidget::setValue(double d)
{
}

// Set widget value from character string (and perform events)
bool TreeGuiWidget::setValue(const char *s)
{
}

/*
// TreeGui
*/

// Constructor
TreeGui::TreeGui()
{
	// Private variables
	if (gui.applicationType() != QApplication::Tty) qtTreeGui_ = new AtenTreeGuiDialog;
	else qtTreeGui_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
TreeGui::~TreeGui()
{
}

// Set title of dialog
void TreeGui::setTitle(const char *title)
{
	title_ = title;
}

// Return title of dialog
const char *TreeGui::title()
{
	return title_.get();
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
	// Does this name already exist?
	if (findWidget(name))
	{
		msg.print("Error: A widget named '%s' already exists in the dialog, and cannot be duplicated.\n", name);
		return NULL;
	}
	TreeGuiWidget *widget = widgets_.add();
	widget->set(TreeGuiWidget::ComboWidget, name, this);
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
	if (qtTreeGui_ != NULL) widget->setQWidget(qtTreeGui_->addCombo(widget, label, items, index));
	return widget;
}

// Create new integer spin widget
TreeGuiWidget *TreeGui::addIntegerSpin(const char *name, const char *label, int min, int max, int step, int value)
{
	// Does this name already exist?
	if (findWidget(name))
	{
		msg.print("Error: A widget named '%s' already exists in the dialog, and cannot be duplicated.\n", name);
		return NULL;
	}
	TreeGuiWidget *widget = widgets_.add();
	widget->set(TreeGuiWidget::IntegerSpinWidget, name, this);
	// Set control limits
	if (!widget->setProperties(min, max, value))
	{
		msg.print("Error when setting up integer spin widget '%s'.\n", name);
		return NULL;
	}
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQWidget(qtTreeGui_->addIntegerSpin(widget, label, min, max, step, value));
	return widget;
}

// Create new double spin widget
TreeGuiWidget *TreeGui::addDoubleSpin(const char *name, const char *label, double min, double max, double step, double value)
{
	// Does this name already exist?
	if (findWidget(name))
	{
		msg.print("Error: A widget named '%s' already exists in the dialog, and cannot be duplicated.\n", name);
		return NULL;
	}
	TreeGuiWidget *widget = widgets_.add();
	widget->set(TreeGuiWidget::DoubleSpinWidget, name, this);
	// Set control limits
	if (!widget->setProperties(min, max, value)) msg.print("Error when setting up double spin widget '%s'.\n", name);
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQWidget(qtTreeGui_->addIntegerSpin(widget, label, min, max, step, value));
	return widget;
}

// Create new label widget
TreeGuiWidget *TreeGui::addLabel(const char *text)
{
	TreeGuiWidget *widget = widgets_.add();
	widget->set(TreeGuiWidget::LabelWidget, "_LABEL_", this);
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQWidget(qtTreeGui_->addLabel(widget, text));
	return widget;
}

// Create new edit widget
TreeGuiWidget *TreeGui::addEdit(const char *name, const char *label, const char *text)
{
	// Does this name already exist?
	if (findWidget(name))
	{
		msg.print("Error: A widget named '%s' already exists in the dialog, and cannot be duplicated.\n", name);
		return NULL;
	}
	TreeGuiWidget *widget = widgets_.add();
	widget->set(TreeGuiWidget::EditWidget, name, this);
	// Set control properties
	if (!widget->setProperties(text)) msg.print("Error when setting up edit widget '%s'.\n", name);
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQWidget(qtTreeGui_->addEdit(widget, label, text));
	return widget;
}

// Create new checkbox widget
TreeGuiWidget *TreeGui::addCheck(const char *name, const char *label, int state)
{
	// Does this name already exist?
	if (findWidget(name))
	{
		msg.print("Error: A widget named '%s' already exists in the dialog, and cannot be duplicated.\n", name);
		return NULL;
	}
	TreeGuiWidget *widget = widgets_.add();
	widget->set(TreeGuiWidget::CheckWidget, name, this);
	// Set control limits
	if (!widget->setProperties(0, 1, state))
	{
		msg.print("Error when setting up check widget '%s'.\n", name);
		return NULL;
	}
	// Create complementary Qt control?
	if (qtTreeGui_ != NULL) widget->setQWidget(qtTreeGui_->addCheck(widget, label, state));
	return widget;
}

// Create new tab widget
TreeGuiWidget *TreeGui::addTabs(const char *name)
{
}

// Create new page (only in tab widget)
TreeGuiWidget *TreeGui::addPage(const char *label)
{
}

// Create new group box
TreeGuiWidget *TreeGui::addGroup(const char *name)
{
}

// Create new (invisible) radio group
TreeGuiWidget *TreeGui::addRadioGroup(const char *name)
{
}

// Create new radio button
TreeGuiWidget *TreeGui::addRadioButton(const char *name, const char *label, int state)
{
}

// Set named widget's value from integer
bool TreeGui::setValue(const char *name, int i)
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
bool TreeGui::setValue(const char* name, double d)
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
bool TreeGui::setValue(const char* name, const char* s)
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
}

// Return value in named widget as double
double TreeGui::asDouble(const char *name)
{
}

// Return value in named widget as character string
const char *TreeGui::asCharacter(const char *name)
{
}

// Return values in named widgets as Vec3<double>
Vec3<double> TreeGui::asVec3(const char *name1, const char *name2, const char *name3)
{
}


// Show Qt dialog (if it exists)
void TreeGui::show()
{
	if ((widgets_.nItems() > 0) && (qtTreeGui_ != NULL)) qtTreeGui_->showDialog();
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
