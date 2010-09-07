/*
	*** GUI Widget Node
	*** src/parser/widgetnode.h
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

#ifndef ATEN_WIDGETNODE_H
#define ATEN_WIDGETNODE_H

#include "parser/treenode.h"
#include "templates/list.h"
#include "templates/kvtable.h"
#include "base/dnchar.h"

// Forward Declarations
class QWidget;

// User-defined GUI filter option
class WidgetNode : public TreeNode
{
	public:
	// Constructor / destructor
	WidgetNode();
	~WidgetNode();
	// User-defined GUI option types
	enum GuiControl { CheckControl, ComboControl, DoubleSpinControl, EditControl, IntegerComboControl, IntegerSpinControl, LabelControl, nGuiControls };
	static GuiControl guiControl(const char *s, bool reporterror = FALSE);
	static const char *guiControl(GuiControl got);
	// Options for Qt layout
	enum GuiQtOption { CentreOption, DisabledOption, GroupNameOption, LabelSpanOption, LeftOption, NewLineOption, ParentSpanOption, SpanOption, TabsOption, nGuiQtOptions };
	static GuiQtOption guiQtOption(const char *s, bool reporterror = FALSE);
	static const char *guiQtOption(GuiQtOption gqo);
	// Widget parent types
	enum GuiWidgetParent { NoParent, GroupBoxParent, TabWidgetParent };

	/*
	// Definitions
	*/
	private:
	// Return value of node
	ReturnValue returnValue_;
	// Type of GUI control
	GuiControl controlType_;
	// Option name
	Dnchar name_;
	// Data for control
	KVTable<Dnchar,Dnchar> data_;
	// Set associated data
	bool setData(const char *name, TreeNode *arg, const char *errormsg, bool critical, const char *def);

	public:
	// Set return value
	void setReturnValue(const ReturnValue &rv);
	// Set argument list from parser-joined treenodes
	bool addJoinedArguments(TreeNode *arglist);
	// Return type of GUI control
	GuiControl controlType();
	// Return name of option
	const char *name();
	// Retrieve associated data
	bool data(const char *name, Dnchar &value);


	/*
	// Associated Qt Widget and Options
	*/
	private:
	// Pointer to associated QWidget
	QWidget *widget_;
	// Type of widget parent (if any)
	GuiWidgetParent widgetParentType_;
	// Name of parent in which the control exists
	Dnchar widgetParentName_;
	// Span width of the associated parent (if any, set on creation)
	int widgetParentSpan_;
	// Span width of the widget control
	int widgetSpan_;
	// Span width of the label for the widget
	int widgetLabelSpan_;
	// Label alignment type
	int widgetLabelAlignment_;
	// Whether this widget should be placed on a new row in the current layout
	bool widgetNewLine_;
	// Whether the widget is enabled
	bool widgetEnabled_;

	public:
	// Set option from argument
	void setOption(TreeNode *arg);
	// Return whether a parent exists
	GuiWidgetParent widgetParentType();
	// Return parent name
	const char *widgetParentName();
	// Return parent span
	int widgetParentSpan();
	// Return label span
	int widgetLabelSpan();
	// Return whether to centre lable text
	int widgetLabelAlignment();
	// Return widget span
	int widgetSpan();
	// Return newline flag
	bool widgetNewLine();
	// Set widget pointer
	void setWidget(QWidget *w);
	// Return widget pointer
	QWidget *widget();
	// Return whether widget is enabled
	bool widgetEnabled();


	/*
	// Inherited Virtuals
	*/
	public:
	// Execute node
	bool execute(ReturnValue &rv);
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");
	// Set from returnvalue node
	bool set(ReturnValue &rv);
	// Initialise node
	bool initialise();
};

#endif
