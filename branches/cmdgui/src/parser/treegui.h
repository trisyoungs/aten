/*
	*** Tree GUI for CLI/Qt
	*** src/parser/treegui.h
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

#ifndef ATEN_TREEGUI_H
#define ATEN_TREEGUI_H

//#include <iostream>
//#include "parser/filterdata.h"
//#include "parser/returnvalue.h"
//#include "parser/variable.h"
//#include "parser/variablelist.h"
//#include "command/commands.h"
//#include "templates/namemap.h"
#include "templates/list.h"
#include "templates/reflist.h"
//#include "base/dnchar.h"
//#include "base/elements.h"
//#include "base/lineparser.h"
#include "templates/vector3.h"

// Forward declarations
class AtenCustomDialog;
class QWidget;
class QObject;
class TreeGui;

// Widget in TreeGui
class TreeGuiWidget
{
	public:
	// Constructor / Destructor
	TreeGuiWidget();
	~TreeGuiWidget();
	// List pointers
	TreeGuiWidget *prev, *next;
	// Widget Types
	enum WidgetType { ButtonGroupWidget, CheckWidget, ComboWidget, DialogWidget, DoubleSpinWidget, EditWidget, IntegerSpinWidget, LabelWidget, PageWidget,  RadioButtonWidget, StackWidget, TabWidget, nWidgetTypes };
	
	
	/*
	// Data
	*/
	private:
	// Type of widget
	WidgetType type_;
	// Pointer to partnered Qt widget (if GUI exists)
	QWidget *widget_;
	// Pointer to partnered QObject (if any)
	QObject *object_;
	// Local name used for reference and retrieval
	Dnchar name_;
	// Parent TreeGui
	TreeGui *parent_;
	
	public:
	// Set widget type
	void setType(TreeGuiWidget::WidgetType type);
	// Return widget type
	TreeGuiWidget::WidgetType type();
	
	
	/*
	// Properties / Limits
	*/
	private:
	// Integer minimum, maximum, and value (ComboWidget, IntegerSpinWidget, CheckWidget)
	int minimimI_, maximumI_, valueI_;
	// Double minimum and maximum (DoubleSpinWidget)
	double minimumD_, maximumD_, valueD_;
	// Text (EditWidget, LabelWidget)
	Dnchar text_;
	// Items list (ComboWidget)
	List<Dnchar> items_;
	
	
	/*
	// Layout / Grouping
	*/
	private:
	// List of buttons contained within this widget (if ButtonGroupWidget)
	Reflist<TreeGuiWidget,int> buttonList_;

	public:
	// Add widget to the layout in this widget (if it has one) at specified geometry, returning added widget for convenience
	TreeGuiWidget *addWidget(TreeGuiWidget *widget, int l, int r, int addToWidth = 0, int addToHeight = 0);
	// Add button to button group (only valid for ButtonGroupWidget);
	TreeGuiWidget *addButton(TreeGuiWidget *widget);
};

// TreeGui
class TreeGui : public TreeGuiWidget
{
	public:
	// Constructor / Destructor
	TreeGui();
	~TreeGui();
	// List pointers
	Tree *prev, *next;


	/*
	// Basic Data
	*/
	private:
	// Title of dialog
	Dnchar title_;
	
	public:
	// Set title of window
	void setTitle(const char *title);
	// Return title of window
	const char *title();


	/*
	// Widgets
	*/
	private:
	// List of user-defined widgets for custom dialog / filter options
	List<TreeGuiWidget> widgets_;
	// Qt dialog containing ready-created set of controls
	AtenCustomDialog *customDialog_;

	public:
	// Create new combo widget
	TreeGuiWidget *addCombo(const char *name, const char *label, const char *items, int index);
	// Create new integer spin widget
	TreeGuiWidget *addIntegerSpin(const char *name, const char *label, int min, int max, int step, int value);
	// Create new double spin widget
	TreeGuiWidget *addDoubleSpin(const char *name, const char *label, double min, double max, double step, double value);
	// Create new label widget
	TreeGuiWidget *addLabel(const char *name, const char *label);
	// Create new edit widget
	TreeGuiWidget *addEdit(const char *name, const char *label, const char *text);
	// Create new checkbox widget
	TreeGuiWidget *addCheck(const char *name, const char *label, int state);
	// Create new tab widget
	TreeGuiWidget *addTabs(const char *name);
	// Create new page (only in tab widget)
	TreeGuiWidget *addPage(const char *name);
	// Create new group box
	TreeGuiWidget *addGroup(const char *name);
	// Create new (invisible) button group
	TreeGuiWidget *addButtonGroup(const char *name);
	// Create new radio button
	TreeGuiWidget *addRadioButton(const char *name, const char *label, int state);


	/*
	// Widget Value Set / Get
	*/
	public:
	// Set named widget's value from integer
	void setValue(const char *name, int i);
	// Set named widget's value from double
	void setValue(const char *name, double d);
	// Set named widget's value from string
	void setValue(const char *name, const char *s);
	// Return value in named widget as integer
	int asInteger(const char *name);
	// Return value in named widget as double
	double asDouble(const char *name);
	// Return value in named widget as character string
	const char *asCharacter(const char *name);
	// Return values in named widgets as Vec3<double>
	Vec3<double> asVec3(const char *name1, const char *name2, const char *name3);
// 	// Add new (GUI-based) filter option linked to a variable
// 	TreeNode *addWidget(TreeNode *arglist);
// 	// Return first item in list of filter options
// 	Refitem<TreeGuiWidget,int> *widgets();
// 	// Locate named widget
// 	TreeGuiWidget *findWidget(const char *name);
// 	// Locate widget with specified widget pointer
// 	TreeGuiWidget *findWidget(QWidget *widget);
// 	// Locate widget with specified object pointer
// 	TreeGuiWidget *findWidgetObject(QObject *obj);
// 	// Create custom dialog from defined widgets (if there are any)
// 	void createCustomDialog(const char *title = NULL);
// 	// Return custom dialog (if any)
// 	AtenCustomDialog *customDialog();
// 	// Execute defined custom dialog (if one exists, just return TRUE if not)
// 	bool executeCustomDialog(bool getvaluesonly = FALSE, const char *newtitle = NULL);
// 	// Retrieve current value of named widget as a double
// 	double widgetValued(const char *name);
// 	// Retrieve current value of named widget as an integer
// 	int widgetValuei(const char *name);
// 	// Retrieve current value of named widget as a string
// 	const char *widgetValuec(const char *name);
// 	// Retrieve current value of named widget triplet as a vector
// 	Vec3<double> widgetValue3d(const char *name1, const char *name2, const char *name3);
// 	// Set current value of named widget
// 	void setWidgetValue(const char *name, ReturnValue value);
// 	// Set property of named widget (via a state change)
// 	bool setWidgetProperty(const char *name, const char *property, ReturnValue value);
};

#endif
