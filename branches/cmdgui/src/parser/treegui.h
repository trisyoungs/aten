/*
	*** Tree GUI for CLI
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

#include "templates/list.h"
#include "templates/reflist.h"
#include "templates/vector3.h"

// Forward declarations
class AtenTreeGuiDialog;
class QtWidgetObject;
class TreeGui;

// Widget Event Action
class TreeGuiWidgetEventAction
{
	public:
	// Constructor / Destructor
	TreeGuiWidgetEventAction();
	~TreeGuiWidgetEventAction();
	// List pointers
	TreeGuiWidgetEventAction *prev, *next;
};

// Widget Event
class TreeGuiWidgetEvent
{
	public:
	// Constructor / Destructor
	TreeGuiWidgetEvent();
	~TreeGuiWidgetEvent();
	// List pointers
	TreeGuiWidgetEvent *prev, *next;
	// Event Qualifier
	enum EventQualifier { IntegerQualifier, DoubleQualifier, StringQualifier, nEventQualifiers };
	
	
	private:
	// Type of qualifying data
	EventQualifier type_;
	// Qualifying (min/max) integer values 
	int minimumI_, maximumI_;
	// Qualifying (min/max) double values 
	double minimumD_, maximumD_;
	// Qualifying string value
	Dnchar matchS_;
	
	public:
	// Set integer qualifying event
	void setQualifier(int min, int max);
	// Set double qualifying event
	void setQualifier(double min, double max);
	// Set string qualifying event
	void setQualifier(const char *s);
};

// Widget in TreeGui
class TreeGuiWidget
{
	public:
	// Constructor / Destructor
	TreeGuiWidget();
	virtual ~TreeGuiWidget();
	// List pointers
	TreeGuiWidget *prev, *next;
	// Widget Types
	enum WidgetType { CheckWidget, ComboWidget, DialogWidget, DoubleSpinWidget, EditWidget, GroupWidget, IntegerSpinWidget, LabelWidget, PageWidget, RadioButtonWidget, RadioGroupWidget, StackWidget, TabWidget, nWidgetTypes };
	
	
	/*
	// Data
	*/
	private:
	// Type of widget
	WidgetType type_;
	// Pointer to partnered Qt widget/object (if there is one)
	QtWidgetObject *qtWidgetObject_;
	// Local name used for reference and retrieval
	Dnchar name_;
	// Parent TreeGui
	TreeGui *parent_;
	
	public:
	// Set widget type, name, and parent
	void set(TreeGuiWidget::WidgetType type, const char *name, TreeGui *parent);
	// Return widget type
	TreeGuiWidget::WidgetType type();
	// Return widget name
	const char *name();
	// Return widget parent
	TreeGui *parent();
	// Set corresponding Qt QWidget/QObject
	void setQtWidgetObject(QtWidgetObject *wo);
	// Return associated qtWidgetObject
	QtWidgetObject *qtWidgetObject();
	
	
	/*
	// Properties / Limits
	*/
	private:
	// Integer minimum, maximum, and value (ComboWidget, IntegerSpinWidget, CheckWidget)
	int minimumI_, maximumI_, valueI_;
	// Double minimum and maximum (DoubleSpinWidget)
	double minimumD_, maximumD_, valueD_;
	// Text (EditWidget, LabelWidget, DialogWidget)
	Dnchar text_;
	// Items list (ComboWidget)
	List<Dnchar> items_;
	// Flag indicating whether items list has recently changed (since last Qt update)
	bool itemsChanged_;
	// Whether widget is enabled
	bool enabled_;
	// Whether widget is visible
	bool visible_;
	
	public:
	// Set integer properties
	bool setProperties(int min, int max, int value);
	// Set double properties
	bool setProperties(double min, double max, double value);
	// Set string properties
	bool setProperties(const char *s);
	// Add text item
	void addItem(const char *s);
	// Return number of defined items
	int nItems();
	// Return whether integer value is within range
	bool isGoodValue(int i, bool printError = FALSE);
	// Return whether double value is within range
	bool isGoodValue(double d, bool printError = FALSE);
	// Return whether string is in list (returning index or 0 for FALSE)
	int isInList(const char *s, bool printError = FALSE);
	// Return current integer minimum
	int minimumI();
	// Return current integer maximum
	int maximumI();
	// Return current integer value
	int valueI();
	// Return current double minimum
	int minimumD();
	// Return current double maximum
	int maximumD();
	// Return current double value
	int valueD();
	// Return current text
	const char *text();
	// Return head of items list
	Dnchar *items();
	// Return whether items list has recently changed (since last Qt update)
	bool itemsChanged();
	// Reset items changed flag
	void resetItemsChanged();
	// Set whether widget is enabled
	void setEnabled(bool b);
	// Return whether widget is enabled
	bool enabled();
	// Set whether widget is visible
	void setVisible(bool b);
	// Return whether widget is visible
	bool visible();


	/*
	// Layout / Grouping
	*/
	private:
	// List of buttons contained within this widget (if ButtonGroupWidget)
	Reflist<TreeGuiWidget,int> buttonList_;

	public:
	// Add widget to the layout in this widget (if it has one) at specified geometry, returning added widget for convenience
	TreeGuiWidget *addWidget(TreeGuiWidget *widget, int l, int r, int addToWidth = 0, int addToHeight = 0);
	// Create new radio button (only for RadioGroupWidget)
	TreeGuiWidget *addRadioButton(const char *name, const char *label, int state);
	// Create new page (only valid for TabWidget)
	TreeGuiWidget *addPage(const char *name, const char *label);
	
	/*
	// Value Access and Events
	*/
	private:
	// List of events associated to this widget
	List<TreeGuiWidgetEvent> events_;
	
	public:
	// Return widget value as integer
	int asInteger();
	// Return widget value as double
	double asDouble();
	// Return widget value as character string
	const char *asCharacter();
	// Set widget value from integer (and perform events)
	bool setValue(int i);
	// Set widget value from double (and perform events)
	bool setValue(double d);
	// Set widget value from character string (and perform events)
	bool setValue(const char *s);
	// Check widget's events and act on them if necessary
	void checkWidgetEvents();
};

// TreeGui
class TreeGui : public TreeGuiWidget
{
	public:
	// Constructor / Destructor
	TreeGui();
	~TreeGui();
	// List pointers
	TreeGui *prev, *next;


	/*
	// Widgets
	*/
	private:
	// List of user-defined widgets for custom dialog / filter options
	List<TreeGuiWidget> widgets_;
	// Qt dialog containing ready-created set of controls
	AtenTreeGuiDialog *qtTreeGui_;
	// Create basic widget of specified type
	TreeGuiWidget *createWidget(const char *name, TreeGuiWidget::WidgetType type);

	public:
	// Return number of defined widgets in GUI
	int nWidgets();
	// Search for named widget
	TreeGuiWidget *findWidget(const char *name);
	// Create new combo widget
	TreeGuiWidget *addCombo(const char *name, const char *label, const char *items, int index);
	// Create new integer spin widget
	TreeGuiWidget *addIntegerSpin(const char *name, const char *label, int min, int max, int step, int value);
	// Create new double spin widget
	TreeGuiWidget *addDoubleSpin(const char *name, const char *label, double min, double max, double step, double value);
	// Create new label widget
	TreeGuiWidget *addLabel(const char* text);
	// Create new edit widget
	TreeGuiWidget *addEdit(const char *name, const char *label, const char *text);
	// Create new checkbox widget
	TreeGuiWidget *addCheck(const char *name, const char *label, int state);
	// Create new tab widget
	TreeGuiWidget *addTabs(const char *name);
	// Create new group box
	TreeGuiWidget *addGroup(const char *name, const char *label);
	// Create new (invisible) radio group
	TreeGuiWidget *addRadioGroup(const char *name);
	// Create new page in specified tab (called by TreeGuiWidget)
	TreeGuiWidget *addPageToTab(const char *name, const char *label, TreeGuiWidget *tabWidget);
	// Create new radio button in specified radio group (called by TreeGuiWidget)
	TreeGuiWidget *addButtonToGroup(const char *name, const char *label, TreeGuiWidget *groupWidget, int buttonId);


	/*
	// Widget Value Set / Get
	*/
	public:
	// Set named widget's value from integer
	bool setWidgetValue(const char* name, int i);
	// Set named widget's value from double
	bool setWidgetValue(const char *name, double d);
	// Set named widget's value from string
	bool setWidgetValue(const char *name, const char *s);
	// Return value in named widget as integer
	int asInteger(const char *name);
	// Return value in named widget as double
	double asDouble(const char *name);
	// Return value in named widget as character string
	const char *asCharacter(const char *name);
	// Return values in named widgets as Vec3<double>
	Vec3<double> asVec3(const char *name1, const char *name2, const char *name3);


	/*
	// Dialog Execution
	*/
	public:
	// Show Qt dialog (if it exists)
	bool execute();
};

#endif
