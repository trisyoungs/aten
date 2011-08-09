/*
	*** Tree GUI for Qt/CLI
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

#include "gui/customdialog.h"
#include "parser/treegui.h"

// Constructors
TreeGui::Tree()
{
	// Private variables
	customDialog_ = NULL;
	
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
TreeGui::~Tree()
{
	clear();
}


// Add new (GUI-based) widget linked to a variable
TreeNode *TreeGui::addWidget(TreeNode *arglist)
{
	msg.enter("TreeGui::addWidget");
	// Wrap the variable and add it to the arguments_ list
	WidgetNode *node = new WidgetNode();
	node->setParent(this);
	// Store in reflist also...
	widgets_.add(node);
	// Add arguments to node (also sets return type)
	if (node->addJoinedArguments(arglist)) msg.print(Messenger::Parse, "Added GUI widget '%s'...\n", node->name());
	else
	{
		msg.print("Failed to add GUI widget.\n");
		msg.exit("TreeGui::addWidget");
		return NULL;
	}
	msg.exit("TreeGui::addWidget");
	return node;
}

// Return first item in list of widgets
Refitem<WidgetNode,int> *TreeGui::widgets()
{
	return widgets_.first();
}

// Create custom dialog from defined widgets
void TreeGui::createCustomDialog(const char *title)
{
	if (gui.applicationType() != QApplication::Tty)
	{
		customDialog_ = new AtenCustomDialog(NULL);
		customDialog_->createWidgets(title, this);
	}
}

// Return custom dialog (if any)
AtenCustomDialog *TreeGui::customDialog()
{
	return customDialog_;
}

// Execute contained custom dialog
 bool TreeGui::executeCustomDialog(bool getvaluesonly, const char *newtitle)
{
	if (customDialog_ == NULL) return TRUE;
	// Retitle dialog?
	if (newtitle) customDialog_->setWindowTitle(newtitle);
	if (getvaluesonly)
	{
		customDialog_->storeValues();
		return TRUE;
	}
	return customDialog_->showDialog();
}

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
}
