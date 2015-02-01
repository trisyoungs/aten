/*
	*** Tree GUI for CLI
	*** src/parser/treegui.h
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

#ifndef ATEN_TREEGUI_H
#define ATEN_TREEGUI_H

#include "templates/list.h"
#include "templates/reflist.h"
#include "templates/vector3.h"
#include "returnvalue.h"

// Forward declarations
class AtenTreeGuiDialog;
class QtWidgetObject;
class TreeGuiWidget;
class TreeGui;

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
	enum EventQualifier { ClickQualifier, IntegerQualifier, DoubleQualifier, StringQualifier, nEventQualifiers };
	// Event Type
	enum EventType { ActivateType, ExecuteFunctionType, SendBoolType, SendDoubleType, SendIntegerType, SendStringType, SetPropertyType, nEventTypes };
	static EventType eventType(const char *s, bool reportError = FALSE);
	static const char *eventType(EventType i);
	// Event Target Property
	enum EventProperty { DisabledProperty, EnabledProperty, InvisibleProperty, ItemsProperty, MaximumProperty, MinimumProperty, TextProperty, ValueProperty, VisibleProperty, nEventProperties };
	static EventProperty eventProperty(const char *s, bool reportError = FALSE);
	static const char *eventProperty(EventProperty i);

	private:
	// Type of qualifying data
	EventQualifier qualifier_;
	// Type of event action
	EventType type_;
	// Target property
	TreeGuiWidgetEvent::EventProperty targetProperty_;
	// Target Widget
	TreeGuiWidget *targetWidget_;
	// Qualifying (min/max) integer values 
	int minimumI_, maximumI_;
	// Qualifying (min/max) double values 
	double minimumD_, maximumD_;
	// Qualifying string value(s)
	List<Dnchar> matchS_;
	// Send value(s) (if explicitly set)
	List<ReturnValue> sendValues_;

	public:
	// Return qualifier type
	TreeGuiWidgetEvent::EventQualifier qualifier();
	// Return event type
	TreeGuiWidgetEvent::EventType type();
	// Return event property
	TreeGuiWidgetEvent::EventProperty targetProperty();
	// Return target widget
	TreeGuiWidget *targetWidget();
	// Set integer qualifying event
	void setQualifiers(int imin, int imax);
	// Set double qualifying event
	void setQualifiers(double dmin, double dmax);
	// Set string qualifying event (comma-separated list)
	void setQualifiers(const char *s);
	// Set remaining event data
	void setEventData(TreeGuiWidgetEvent::EventType type, TreeGuiWidget *targetwidget, TreeGuiWidgetEvent::EventProperty property);
	// Add send data to event
	ReturnValue *addSendValue();
	// Return number of send values defined
	int nSendValues();
	// Return first send value in list
	ReturnValue *firstSendValue();
	// Return specified send data based on supplied widget value
	ReturnValue *sendValue(int widgetValue);
	// Return whether supplied integer value qualifies
	bool qualifies(int i);
	// Return whether supplied double value qualifies
	bool qualifies(double d);
	// Return whether supplied character value qualifies
	bool qualifies(const char *s);
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
	enum WidgetType { ButtonWidget, CheckWidget, ComboWidget, DialogWidget, DoubleSpinWidget, EditWidget, FrameWidget, GroupWidget, IntegerSpinWidget, LabelWidget, PageWidget, RadioButtonWidget, RadioGroupWidget, StackWidget, TabWidget, nWidgetTypes };
	static WidgetType widgetType(const char *s, bool reportError = FALSE);
	static const char *widgetType(WidgetType i);


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
	// Whether widget is enabled
	bool enabled_;
	// Whether widget is visible
	bool visible_;
	// Array of flags indicating whether properties have changed since last event check / update
	bool propertyChanged_[TreeGuiWidgetEvent::nEventProperties];
	
	public:
	// Set integer properties
	bool setInitialProperties(int min, int max, int value);
	// Set double properties
	bool setInitialProperties(double min, double max, double value);
	// Set string properties
	bool setInitialProperties(const char *s);
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
	double minimumD();
	// Return current double maximum
	double maximumD();
	// Return current double value
	double valueD();
	// Return current text
	const char *text();
	// Return head of items list
	Dnchar *items();
	// Set whether widget is enabled
	void setEnabled(bool b);
	// Return whether widget is enabled
	bool enabled();
	// Set whether widget is visible
	void setVisible(bool b);
	// Return whether widget is visible
	bool visible();
	// Set specified property
	bool setProperty(TreeGuiWidgetEvent::EventProperty property, ReturnValue rv);
	// 'Activate' property, setting flag and executing events, but leaving value unchanged
	bool activateProperty(TreeGuiWidgetEvent::EventProperty property);
	// Return whether specified property has changed
	bool propertyChanged(TreeGuiWidgetEvent::EventProperty property);
	// Reset changed flag for specified property
	void resetChanged(TreeGuiWidgetEvent::EventProperty property);


	/*
	// Layout / Grouping
	*/
	private:
	// List of buttons contained within this widget (if ButtonGroupWidget)
	Reflist<TreeGuiWidget,int> buttonList_;

	private:
	// Add widget to the layout in this widget (if it has one) at specified geometry, returning added widget for convenience
	void addWidget(TreeGuiWidget* widget, int left, int top, int addToWidth = 0, int addToHeight = 0);

	public:
	// Create new button widget
	TreeGuiWidget *addButton(const char *name, const char *label, int l, int t, int xw = 0, int xh = 0);
	// Create new checkbox widget
	TreeGuiWidget *addCheck(const char *name, const char *label, int state, int l, int t, int xw = 0, int xh = 0);
	// Create new combo widget
	TreeGuiWidget *addCombo(const char *name, const char *label, const char *items, int index, int l, int t, int xw = 0, int xh = 0);
	// Create new double spin widget
	TreeGuiWidget *addDoubleSpin(const char *name, const char *label, double min, double max, double step, double value, int l, int t, int xw = 0, int xh = 0);
	// Create new edit widget
	TreeGuiWidget *addEdit(const char *name, const char *label, const char *text, int l, int t, int xw = 0, int xh = 0);
	// Create new frame widget
	TreeGuiWidget *addFrame(const char *name, int l, int t, int xw = 0, int xh = 0);
	// Create new group box
	TreeGuiWidget *addGroup(const char *name, const char *label, int l, int t, int xw = 0, int xh = 0);
	// Create new integer spin widget
	TreeGuiWidget *addIntegerSpin(const char *name, const char *label, int min, int max, int step, int value, int l, int t, int xw = 0, int xh = 0);
	// Create new label widget
	TreeGuiWidget *addLabel(const char *name, const char *text, int l, int t, int xw = 0, int xh = 0);
	// Create new page (only valid for TabWidget)
	TreeGuiWidget *addPage(const char *name, const char *label);
	// Create new radio button in specified group
	TreeGuiWidget *addRadioButton(const char* name, const char* label, const char *radioGroup, int state, int l, int t, int xw = 0, int xh = 0);
	// Create new (invisible) radio group
	TreeGuiWidget *addRadioGroup(const char *name);
	// Create new spacer
	bool addSpacer(bool expandHorizontal, bool expandVertical, int l, int t, int xw = 0, int xh = 0);
	// Create new stack widget
	TreeGuiWidget *addStack(const char *name, int l, int t, int xw = 0, int xh = 0);
	// Create new tab widget
	TreeGuiWidget *addTabs(const char *name, int l, int t, int xw = 0, int xh = 0);


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
	// Add widget event
	TreeGuiWidgetEvent *addEvent(TreeGuiWidgetEvent::EventType type, TreeGuiWidget *targetWidget, TreeGuiWidgetEvent::EventProperty property);
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
	// Friend classes
	friend class TreeGuiWidget;
	

	/*
	// Widgets
	*/
	private:
	// List of user-defined widgets for custom dialog / filter options
	List<TreeGuiWidget> widgets_;
	// Qt dialog containing ready-created set of controls
	AtenTreeGuiDialog *qtTreeGui_;
	
	protected:
	// Return qtTreeGui pointer
	AtenTreeGuiDialog *qtTreeGui();
	// Create basic widget of specified type
	TreeGuiWidget *createWidget(const char *name, TreeGuiWidget::WidgetType type);
	// Create new page in specified tab (called by TreeGuiWidget)
	TreeGuiWidget *addPageToTab(const char *name, const char *label, TreeGuiWidget *tabWidget);

	public:
	// Return number of defined widgets in GUI
	int nWidgets();
	// Search for named widget
	TreeGuiWidget *findWidget(const char *name);


	/*
	// Widget Value Set / Get
	*/
	private:
	// List available widget names, types, and values
	void listWidgets();

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
