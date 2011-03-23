/*
	*** Qt Custom Dialog
	*** src/gui/customdialog_funcs.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "base/sysfunc.h"

// Constructor
AtenCustomDialog::AtenCustomDialog(QWidget *parent) : QDialog(parent)
{
	refreshing_ = FALSE;
	ui.setupUi(this);
	// Add grid layout to MainFrame
	mainLayout_ = new QGridLayout(ui.MainFrame);
	parentTree_ = NULL;
}

// Perform specified state change
void AtenCustomDialog::performStateChange(StateChange *sc)
{
	// First, find relevant widget definition
	WidgetNode *node = parentTree_->findWidget(sc->targetWidget());
	if (node == NULL)
	{
		printf("AtenCustomDialog::performStateChange - Unable to locate widget '%s'.\n", sc->targetWidget());
		return;
	}
	// Proceed based on widget type
	QComboBox *combo;
	QSpinBox *spin;
	QDoubleSpinBox *doublespin;
	QCheckBox *check;
	QLineEdit *line;
	QStackedWidget *stack;
	QButtonGroup *buttongroup;
	QAbstractButton *button;
	QLabel *label;
	Dnchar data;
	LineParser lp;
	int n;
	switch (node->controlType())
	{
		// Check Box
		case (WidgetNode::CheckControl):
			check = (QCheckBox*) node->widget();
			switch (sc->changeAction())
			{
				case (StateChange::CheckedAction):
					check->setChecked(sc->changeDataAsBool());
					break;
				case (StateChange::DisableAction):
					check->setEnabled(FALSE);
					break;
				case (StateChange::EnableAction):
					check->setEnabled(TRUE);
					break;
				default:
					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
			}
			break;
		// RadioGroup
		case (WidgetNode::RadioGroupControl):
			buttongroup = (QButtonGroup*) node->widget();
			switch (sc->changeAction())
			{
				case (StateChange::ValueAction):
					// Search for button with supplied id
					button = buttongroup->button(sc->changeDataAsInteger());
					if (button == NULL) printf("Warning - Couldn't find button %i in buttongroup.\n", sc->changeDataAsInteger());
					else button->setChecked(TRUE);
					break;
				default:
					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
			}
			break;
		// Combo Box
		case (WidgetNode::IntegerComboControl):
		case (WidgetNode::ComboControl):
			combo = (QComboBox*) node->widget();
			switch (sc->changeAction())
			{
				case (StateChange::DisableAction):
					combo->setEnabled(FALSE);
					break;
				case (StateChange::EnableAction):
					combo->setEnabled(TRUE);
					break;
				case (StateChange::ItemsAction):
					lp.getArgsDelim(LineParser::UseQuotes, sc->changeData());
					for (n=0; n<lp.nArgs(); ++n) combo->addItem(lp.argc(n));
					combo->setCurrentIndex(0);
					break;
				case (StateChange::OriginalItemsAction):
					if (!node->data("items", data)) printf("Critical: No items list found when constructing QComboBox.\n");
					lp.getArgsDelim(LineParser::UseQuotes, data.get());
					for (n=0; n<lp.nArgs(); ++n) combo->addItem(lp.argc(n));
					combo->setCurrentIndex(0);
					break;
				case (StateChange::ValueAction):
					combo->setCurrentIndex(sc->changeDataAsInteger());
					break;
				default:
					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
			}
			break;
		// Double Spin Edit
		case (WidgetNode::DoubleSpinControl):
			doublespin = (QDoubleSpinBox*) node->widget();
			switch (sc->changeAction())
			{
				case (StateChange::DisableAction):
					doublespin->setEnabled(FALSE);
					break;
				case (StateChange::EnableAction):
					doublespin->setEnabled(TRUE);
					break;
				case (StateChange::MinimumAction):
					doublespin->setMinimum(sc->changeDataAsDouble());
					break;
				case (StateChange::MaximumAction):
					doublespin->setMaximum(sc->changeDataAsDouble());
					break;
				case (StateChange::StepAction):
					doublespin->setSingleStep(sc->changeDataAsDouble());
					break;
				case (StateChange::ValueAction):
					doublespin->setValue(sc->changeDataAsDouble());
					break;
				default:
					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
			}
			break;
		// Text Edit
		case (WidgetNode::EditControl):
			line = (QLineEdit*) node->widget();
			switch (sc->changeAction())
			{
				case (StateChange::DisableAction):
					line->setEnabled(FALSE);
					break;
				case (StateChange::EnableAction):
					line->setEnabled(TRUE);
					break;
				default:
					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
			}
			break;
		// Integer spin edit
		case (WidgetNode::IntegerSpinControl):
			spin = (QSpinBox*) node->widget();
			switch (sc->changeAction())
			{
				case (StateChange::DisableAction):
					spin->setEnabled(FALSE);
					break;
				case (StateChange::EnableAction):
					spin->setEnabled(TRUE);
					break;
				case (StateChange::MinimumAction):
					spin->setMinimum(sc->changeDataAsInteger());
					break;
				case (StateChange::MaximumAction):
					spin->setMaximum(sc->changeDataAsInteger());
					break;
				case (StateChange::StepAction):
					spin->setSingleStep(sc->changeDataAsInteger());
					break;
				case (StateChange::ValueAction):
					spin->setValue(sc->changeDataAsInteger());
					break;
				default:
					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
			}
			break;
		// Label
		case (WidgetNode::LabelControl):
			label = (QLabel*) node->widget();
			switch (sc->changeAction())
			{
				case (StateChange::ValueAction):
					label->setText(sc->changeData());
					break;
				default:
					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
			}
			break;
		// Stack control
		case (WidgetNode::StackControl):
			stack = (QStackedWidget*) node->widget();
			switch (sc->changeAction())
			{
				case (StateChange::DisableAction):
					stack->setEnabled(FALSE);
					break;
				case (StateChange::EnableAction):
					stack->setEnabled(TRUE);
					break;
				case (StateChange::SwitchStackAcion):
					// Get new index
					n = sc->stateValueAsInteger() - 1;
					stack->setCurrentIndex(n);
					break;
				default:
					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
			}
			break;
	}
}

// Generic function for checkbox activation
void AtenCustomDialog::checkBoxWidget_clicked(bool checked)
{
	if (!isVisible()) return;
	// Cast sender into checkbox
	refreshing_ = TRUE;
	QCheckBox *check = (QCheckBox*) sender();
	if (!check)
	{
		printf("AtenCustomDialog::checkBoxWidget_clicked - Sender could not be cast to a QCheckBox.\n");
		return;
	}
	// Search for widget definition in original tree...
	WidgetNode *node = parentTree_->findWidget(check);
	if (node == NULL)
	{
		printf("AtenCustomDialog::checkBoxWidget_clicked - couldn't find associated WidgetNode.\n");
		return;
	}
	// Check all states defined in the widgetnode
	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
	{
		if (sc->dynamicValue())
		{
			sc->setStateValue(checked);
			performStateChange(sc);
		}
		else if (checked && ((int)sc->stateValueAsInteger() > 0)) performStateChange(sc);
		else if ((!checked) && ((int)sc->stateValueAsInteger() < 1)) performStateChange(sc);
	}
	refreshing_ = FALSE;
}

// Generic function for combobox activation
void AtenCustomDialog::comboWidget_currentIndexChanged(int row)
{
	if (!isVisible()) return;
	// Cast sender into combobox
	refreshing_ = TRUE;
	QComboBox *combo = (QComboBox*) sender();
	if (!combo)
	{
		printf("AtenCustomDialog::comboWidget_currentIndexChanged - Sender could not be cast to a QComboBox.\n");
		return;
	}
	// Search for widget definition in original tree...
	WidgetNode *node = parentTree_->findWidget(combo);
	if (node == NULL)
	{
		printf("AtenCustomDialog::comboWidget_currentIndexChanged - couldn't find associated WidgetNode.\n");
		return;
	}
	// Check all states defined in the widgetnode
	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
	{
		// If StateChange has a dynamic value, set it now, otherwise compare by integer value or string depending on control type
		if (sc->dynamicValue())
		{
			sc->setStateValue(row+1);
			performStateChange(sc);
		}
		else if (node->controlType() == WidgetNode::IntegerComboControl)
		{
			if ((row+1) == sc->stateValueAsInteger()) performStateChange(sc);
		}
		else if (node->controlType() == WidgetNode::ComboControl)
		{
			if (strcmp(qPrintable(combo->currentText()), sc->stateValue()) == 0) performStateChange(sc);
		}
	}
	refreshing_ = FALSE;
}

// Generic function for double spin activation
void AtenCustomDialog::doubleSpinWidget_valueChanged(double d)
{
	if (!isVisible()) return;
	// Cast sender into checkbox
	refreshing_ = TRUE;
	QDoubleSpinBox *spin = (QDoubleSpinBox*) sender();
	if (!spin)
	{
		printf("AtenCustomDialog::doubleSpinWidget_valueChanged - Sender could not be cast to a QDoubleSpinBox.\n");
		return;
	}
	// Search for widget definition in original tree...
	WidgetNode *node = parentTree_->findWidget(spin);
	if (node == NULL)
	{
		printf("AtenCustomDialog::doubleSpinWidget_valueChanged - couldn't find associated WidgetNode.\n");
		return;
	}
	// Check all states defined in the widgetnode
	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
	{
		if (sc->dynamicValue())
		{
			sc->setStateValue(d);
			performStateChange(sc);
		}
		else if (d == sc->stateValueAsDouble()) performStateChange(sc);
	}
	refreshing_ = FALSE;
}

// Generic function for integer spin activation
void AtenCustomDialog::integerSpinWidget_valueChanged(int i)
{
	if (!isVisible()) return;
	// Cast sender into checkbox
	refreshing_ = TRUE;
	QSpinBox *spin = (QSpinBox*) sender();
	if (!spin)
	{
		printf("AtenCustomDialog::integerSpinWidget_valueChanged - Sender could not be cast to a QSpinBox.\n");
		return;
	}
	// Search for widget definition in original tree...
	WidgetNode *node = parentTree_->findWidget(spin);
	if (node == NULL)
	{
		printf("AtenCustomDialog::integerSpinWidget_valueChanged - couldn't find associated WidgetNode.\n");
		return;
	}
	// Check all states defined in the widgetnode
	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
	{
		if (sc->dynamicValue())
		{
			sc->setStateValue(i);
			performStateChange(sc);
		}
		else if (i == sc->stateValueAsInteger()) performStateChange(sc);
	}
	refreshing_ = FALSE;
}

// Generic function for radio group button activation
void AtenCustomDialog::buttonGroupWidget_buttonClicked(int index)
{
	if (!isVisible()) return;
	// Cast sender into checkbox
	refreshing_ = TRUE;
	QButtonGroup *radio = (QButtonGroup*) sender();
	if (!radio)
	{
		printf("AtenCustomDialog::buttonGroupWidget_buttonClicked - Sender could not be cast to a QButtonGroup.\n");
		return;
	}
	// Search for widget definition in original tree...
	WidgetNode *node = parentTree_->findWidgetObject(radio);
	if (node == NULL)
	{
		printf("AtenCustomDialog::buttonGroupWidget_buttonClicked - couldn't find associated WidgetNode.\n");
		return;
	}
	// Check all states defined in the widgetnode
	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
	{
		if (sc->dynamicValue())
		{
			sc->setStateValue(index+1);
			performStateChange(sc);
		}
		else if (index == sc->stateValueAsInteger()) performStateChange(sc);
	}
	refreshing_ = FALSE;
}

// Generic function for radio button activation
void AtenCustomDialog::radioButtonWidget_clicked(bool checked)
{
	if (!isVisible()) return;
	// Cast sender into radiobutton
	refreshing_ = TRUE;
	QRadioButton *check = (QRadioButton*) sender();
	if (!check)
	{
		printf("AtenCustomDialog::radioButtonWidget_clicked - Sender could not be cast to a QRadioButton.\n");
		return;
	}
	// Search for widget definition in original tree...
	WidgetNode *node = parentTree_->findWidget(check);
	if (node == NULL)
	{
		printf("AtenCustomDialog::radioButtonWidget_clicked - couldn't find associated WidgetNode.\n");
		return;
	}
	// Check all states defined in the widgetnode
	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
	{
		if (sc->dynamicValue())
		{
			sc->setStateValue(checked);
			performStateChange(sc);
		}
		else if (checked && ((int)sc->stateValueAsInteger() > 0)) performStateChange(sc);
		else if ((!checked) && ((int)sc->stateValueAsInteger() < 1)) performStateChange(sc);
	}
	refreshing_ = FALSE;
}

// Create simple label
QLabel *AtenCustomDialog::createLabel(const char *text, int alignment)
{
	msg.enter("AtenCustomDialog::createLabel");
	QLabel *label = new QLabel(text);
	label->setAlignment(((Qt::Alignment) alignment)|Qt::AlignVCenter);
	label->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
	msg.exit("AtenCustomDialog::createLabel");
	return label;
}

// Create empty grid layout
QGridLayout *AtenCustomDialog::createGridLayout(QWidget *parent)
{
	msg.enter("AtenCustomDialog::createGridLayout");
	QGridLayout *layout = new QGridLayout(parent);
#ifndef ATEN_OLDQT
	layout->setContentsMargins(2,2,2,2);
#endif
	layout->setSpacing(2);
	msg.exit("AtenCustomDialog::createGridLayout");
	return layout;
}

// Create check box from data in specified GuiFilterOption
QCheckBox *AtenCustomDialog::createCheckBox(WidgetNode *gfo)
{
	msg.enter("AtenCustomDialog::createCheckBox");
	QCheckBox *check = new QCheckBox(gfo->name());
	// Critical : state
	Dnchar data;
	if (!gfo->data("state", data)) printf("Critical: No state found when constructing QCheckBox.\n");
	check->setChecked(data.asInteger());
	msg.exit("AtenCustomDialog::createCheckBox");
	return check;
}

// Create radio button from data in specified WidgetNode
QRadioButton *AtenCustomDialog::createRadioButton(WidgetNode *gfo, KVTable<Dnchar,QButtonGroup*> &buttonGroups)
{
	msg.enter("AtenCustomDialog::createRadioButton");
	QRadioButton *radio = new QRadioButton(gfo->name());
	Dnchar data;
	// Critical : parent buttongroup
	if (!gfo->data("buttongroup", data)) printf("Critical: No parent buttongroup found when constructing QRadioButton.\n");
	// Search to see if specific key is in the table
	KVData<Dnchar,QButtonGroup*> *bg = buttonGroups.search(data);
	if (bg == NULL)
	{
		QButtonGroup *butgroup = new QButtonGroup();
		butgroup->addButton(radio, 1);
		buttonGroups.add(data, butgroup);
	}
	else bg->value()->addButton(radio, bg->value()->buttons().count()+1);
	// Critical : state
	if (!gfo->data("state", data)) printf("Critical: No state found when constructing QRadioButton.\n");
	radio->setChecked(data.asInteger());
	msg.exit("AtenCustomDialog::createRadioButton");
	return radio;
}

// Create radiogroup from data in specified GuiFilterOption
QButtonGroup *AtenCustomDialog::createRadioGroup(WidgetNode *gfo, KVTable<Dnchar,QButtonGroup*> &buttonGroups)
{
	msg.enter("AtenCustomDialog::createRadioGroup");
	// Search for existing button group
	QButtonGroup *buttongroup;
	KVData<Dnchar,QButtonGroup*> *bg = buttonGroups.search(gfo->name());
	if (bg == NULL)
	{
		buttongroup = new QButtonGroup(this);
		buttonGroups.add(gfo->name(), buttongroup);
		QObject::connect(buttongroup, SIGNAL(buttonClicked(int)), this, SLOT(buttonGroupWidget_buttonClicked(int)));
	}
	else buttongroup = bg->value();
	msg.exit("AtenCustomDialog::createRadioGroup");
	return buttongroup;
}

// Create combo box from data in specified GuiFilterOption
QComboBox *AtenCustomDialog::createComboBox(WidgetNode *gfo)
{
	msg.enter("AtenCustomDialog::createComboBox");
	QComboBox *combo = new QComboBox();
	QObject::connect(combo, SIGNAL(currentIndexChanged(int)), this, SLOT(comboWidget_currentIndexChanged(int)));
	// Critical : items list
	Dnchar data;
	if (!gfo->data("items", data)) printf("Critical: No items list found when constructing QComboBox.\n");
	LineParser lp;
	lp.getArgsDelim(LineParser::UseQuotes, data.get());
	for (int n=0; n<lp.nArgs(); ++n) combo->addItem(lp.argc(n));
	// Optional : default index (+1)
	if (!gfo->data("default", data)) printf("Warning: Default value for QComboBox not set.\n");
	combo->setCurrentIndex(data.asInteger()-1);
	combo->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	msg.exit("AtenCustomDialog::createComboBox");
	return combo;
}

// Create double spin edit from data in specified GuiFilterOption
QDoubleSpinBox *AtenCustomDialog::createDoubleSpinBox(WidgetNode *gfo)
{
	msg.enter("AtenCustomDialog::createDoubleSpinBox");
	QDoubleSpinBox *spin = new QDoubleSpinBox();
	spin->setDecimals(5);
	// Critical : minimum, maximum, start, and step values
	Dnchar data;
	if (!gfo->data("min", data)) printf("Critical: No minimum value found when constructing QDoubleSpinBox.\n");
	double min = data.asDouble();
	if (!gfo->data("max", data)) printf("Critical: No maximum value found when constructing QDoubleSpinBox.\n");
	double max = data.asDouble();
	spin->setRange(min, max);
	if (!gfo->data("start", data)) printf("Critical: No start value found when constructing QDoubleSpinBox.\n");
	double start = data.asDouble();
	spin->setValue(start);
	if (!gfo->data("step", data)) printf("Critical: No step value found when constructing QDoubleSpinBox.\n");
	double step = data.asDouble();
	spin->setSingleStep(step);
	spin->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	msg.exit("AtenCustomDialog::createDoubleSpinBox");
	return spin;
}

// Create line edit from data in specified GuiFilterOption
QLineEdit *AtenCustomDialog::createLineEdit(WidgetNode *gfo)
{
	msg.enter("AtenCustomDialog::createLineEdit");
	QLineEdit *lineedit = new QLineEdit();
	// Critical : text
	Dnchar data;
	if (!gfo->data("text", data)) printf("Critical: No text found when constructing QLineEdit.\n");
	lineedit->setText(data.get());
	lineedit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	msg.exit("AtenCustomDialog::createLineEdit");
	return lineedit;
}

// Create spin box from data in specified GuiFilterOption
QSpinBox *AtenCustomDialog::createSpinBox(WidgetNode *gfo)
{
	msg.enter("AtenCustomDialog::createSpinBox");
	QSpinBox *spin = new QSpinBox();
	// Critical : minimum, maximum, start, and step values
	Dnchar data;
	if (!gfo->data("min", data)) printf("Critical: No minimum value found when constructing QSpinBox.\n");
	int min = data.asInteger();
	if (!gfo->data("max", data)) printf("Critical: No maximum value found when constructing QSpinBox.\n");
	int max = data.asInteger();
	spin->setRange(min, max);
	if (!gfo->data("start", data)) printf("Critical: No start value found when constructing QSpinBox.\n");
	int start = data.asInteger();
	spin->setValue(start);
	if (!gfo->data("step", data)) printf("Critical: No step value found when constructing QSpinBox.\n");
	int step = data.asInteger();
	spin->setSingleStep(step);
	spin->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	msg.exit("AtenCustomDialog::createSpinBox");
	return spin;
}

// Create line edit from data in specified GuiFilterOption
QStackedWidget *AtenCustomDialog::createStackedWidget(WidgetNode* gfo, LayoutList& layoutList)
{
	msg.enter("AtenCustomDialog::createStackedWidget");
	QStackedWidget *stack = new QStackedWidget();
	// Critical : pages
	Dnchar data, name;
	if (!gfo->data("pages", data)) printf("Critical: Number of pages not found whild constructing QStackWidget.\n");
	else for (int n=0; n<data.asInteger(); ++n)
	{
		name.sprintf("%s_%i", gfo->name(), n+1);
		LayoutData *ld = layoutList.find(name);
		if (ld != NULL) printf("Critical: A stack named '%s' already exists...\n", gfo->name());
		else
		{
			// Add a new page to the current QStackedWidget, along with an empty widget and a layout
			QWidget *widget = new QWidget();
			stack->addWidget(widget);
			ld = layoutList.add(name, createGridLayout(widget));
		}
	}
	// Critical : currentindex
	if (!gfo->data("index", data)) printf("Critical: Initial page index not found whild constructing QStackWidget.\n");
	else stack->setCurrentIndex(data.asInteger()-1);
	
	msg.exit("AtenCustomDialog::createStackedWidget");
	return stack;
}

// Construct filter option widgets for specified tree
bool AtenCustomDialog::createWidgets(const char *title, Tree *t)
{
	msg.enter("AtenCustomDialog::createWidgets");
	Refitem<WidgetNode,int> *ri;
	WidgetNode *gfo;
	QGroupBox *group;
	QGridLayout *gridl;
	QTabWidget *tabw;
	int span, labelspan, alignment;
	bool newline;
	LayoutList layouts;
	KVTable<Dnchar,QButtonGroup*> buttonGroups, *bg;
	KVTable<Dnchar,QStackWidget*> stackWidgets;
	Reflist<QTabWidget,Dnchar> tabwidgets;
	Refitem<QTabWidget,Dnchar> *tabref;
	Dnchar name;
	LayoutData *mainlayout, *currentlayout;
	QWidget *widget;
	QObject *object;

	// Set title of window and store target tree
	setWindowTitle(title);
	parentTree_ = t;

	// Get start of list of defined options - if there are none, do nothing
	if (parentTree_->widgets() == NULL)
	{
		msg.exit("AtenCustomDialog::createWidgets");
		return TRUE;
	}

	// Create main layout widget for the other widgets
	layouts.clear();
	tabwidgets.clear();

	mainlayout = layouts.add("_MAIN_", mainLayout_);

	// Create widgets
	for (ri = t->widgets(); ri != NULL; ri = ri->next)
	{
		gfo = ri->item;
		// We will add to the main widget, unless another is specified (i.e. a group)
		if (gfo->widgetParentType() == WidgetNode::NoParent) currentlayout = mainlayout;
		else
		{
			// Find the specified layout (or create it)
			currentlayout = layouts.find(gfo->widgetParentName());
			if (currentlayout == NULL)
			{
				if (gfo->widgetParentType() == WidgetNode::GroupBoxParent)
				{
					// Create a new QGroupBox in the main widget (for now...)
					group = new QGroupBox();
					group->setTitle(gfo->widgetParentName());
					mainlayout->addWidget(group, gfo->widgetParentSpan(), gfo->widgetNewLine());
					gridl = createGridLayout(group);
					currentlayout = layouts.add(gfo->widgetParentName(), gridl);
				}
				else if (gfo->widgetParentType() == WidgetNode::TabWidgetParent)
				{
					// Format of name should be 'page@tabwidget'
					// Two possibilities -  1) the 'tabwidget' does exist but the page doesn't
					//			2) the 'tabwidget' doesn't exit, and nor does the page
					// So, search for tabwidget...
					name = afterChar(gfo->widgetParentName(), '@');
// 					printf("Searching for tab '%s'\n", name.get());
					for (tabref = tabwidgets.first(); tabref != NULL; tabref = tabref->next)
						if (name == tabref->data) break;
					if (tabref == NULL)
					{
// 						printf("No tab found. Creating both tab and page.\n");
						tabw = new QTabWidget();
						mainlayout->addWidget(tabw, gfo->widgetParentSpan(), gfo->widgetNewLine());
						tabwidgets.add(tabw, name);
					}
					else
					{
// 						printf("Tab widget found - adding new page.\n");
						tabw = tabref->item;
					}
					widget = new QWidget(tabw);
					gridl = createGridLayout(widget);
					currentlayout = layouts.add(gfo->widgetParentName(), gridl);
					name = beforeChar(gfo->widgetParentName(), '@');
					tabw->addTab(widget, name.get());
				}
			}
		}

		// Grab spans for widget and its label
		span = gfo->widgetSpan();
		labelspan = gfo->widgetLabelSpan();
		newline = gfo->widgetNewLine();
		alignment = gfo->widgetLabelAlignment();

		// Now create the widget
		switch (gfo->controlType())
		{
			// Check Box - data: state)
			case (WidgetNode::CheckControl):
				widget = createCheckBox(gfo);
				currentlayout->addWidget(widget, span, newline);
				gfo->setWidget(widget);
				break;
			// RadioButton - data: buttongroup, state)
			case (WidgetNode::RadioButtonControl):
				widget = createRadioButton(gfo, buttonGroups);
				currentlayout->addWidget(widget, span, newline);
				gfo->setWidget(widget);
				break;
			// RadioGroup
			case (WidgetNode::RadioGroupControl):
			case (WidgetNode::StringRadioGroupControl):
				object = createRadioGroup(gfo, buttonGroups);
				gfo->setObject(object);
				break;
			// Combo Box - data:  items, default
			case (WidgetNode::IntegerComboControl):
			case (WidgetNode::ComboControl):
				widget = createLabel(gfo->name(), alignment);
				currentlayout->addWidget(widget, labelspan, newline);
				widget = createComboBox(gfo);
				currentlayout->addWidget(widget, span, FALSE);
				gfo->setWidget(widget);
				break;
			// Double Spin Edit - data: min, max, start
			case (WidgetNode::DoubleSpinControl):
				widget = createLabel(gfo->name(), alignment);
				currentlayout->addWidget(widget, labelspan, newline);
				widget = createDoubleSpinBox(gfo);
				currentlayout->addWidget(widget, span, FALSE);
				gfo->setWidget(widget);
				break;
			// Text Edit - data: text
			case (WidgetNode::EditControl):
				widget = createLabel(gfo->name(), alignment);
				currentlayout->addWidget(widget, labelspan, newline);
				widget = createLineEdit(gfo);
				currentlayout->addWidget(widget, span, FALSE);
				gfo->setWidget(widget);
				break;
			// Integer Spin Edit - data: min, max, start
			case (WidgetNode::IntegerSpinControl):
				widget = createLabel(gfo->name(), alignment);
				currentlayout->addWidget(widget, labelspan, newline);
				widget = createSpinBox(gfo);
				currentlayout->addWidget(widget, span, FALSE);
				gfo->setWidget(widget);
				break;
			// Stack - data: npages, index
			case (WidgetNode::StackControl):
				widget = createStackedWidget(gfo, layouts);
				currentlayout->addWidget(widget, span, newline);
				gfo->setWidget(widget);
				break;
			// Label
			case (WidgetNode::LabelControl):
				widget = createLabel(gfo->name(), alignment);
				currentlayout->addWidget(widget, labelspan, newline);
				gfo->setWidget(widget);
				break;
		}

		// Apply general options
		if (!gfo->widgetEnabled()) widget->setEnabled(FALSE);
	}
	msg.exit("AtenCustomDialog::createWidgets");
	return TRUE;
}

// Store widget values back into the associated tree variables
void AtenCustomDialog::storeValues()
{
	msg.enter("AtenCustomDialog::storeValues");
	WidgetNode *gfo;
	ReturnValue rv;
	QAbstractButton *button;
	for (Refitem<WidgetNode,int> *ri = parentTree_->widgets(); ri != NULL; ri = ri->next)
	{
		gfo = ri->item;
		rv.reset();
		switch (gfo->controlType())
		{
			case (WidgetNode::CheckControl):
				rv.set( ((QCheckBox*) (gfo->widget()))->isChecked());
				break;
			case (WidgetNode::RadioGroupControl):
				button = ((QButtonGroup*) (gfo->object()))->checkedButton();
				rv.set( ((QButtonGroup*) (gfo->object()))->checkedId());
				break;
			case (WidgetNode::StringRadioGroupControl):
				button = ((QButtonGroup*) (gfo->object()))->checkedButton();
				rv.set( button == 0 ? "" : qPrintable(button->text()) );
				break;
			case (WidgetNode::IntegerComboControl):
				rv.set( ((QComboBox*) (gfo->widget()))->currentIndex()+1);
				break;
			case (WidgetNode::ComboControl):
				rv.set( qPrintable(((QComboBox*) (gfo->widget()))->currentText()));
				break;
			case (WidgetNode::DoubleSpinControl):
				rv.set( ((QDoubleSpinBox*) (gfo->widget()))->value());
				break;
			case (WidgetNode::EditControl):
				rv.set( qPrintable(((QLineEdit*) (gfo->widget()))->text()));
				break;
			case (WidgetNode::IntegerSpinControl):
				rv.set( ((QSpinBox*) (gfo->widget()))->value());
				break;
			case (WidgetNode::LabelControl):
				break;
		}
		gfo->setReturnValue(rv);
	}
	msg.exit("AtenCustomDialog::storeValues");
}

// Call the dialog, displaying options for the specified filter
bool AtenCustomDialog::showDialog()
{
	msg.enter("AtenCustomDialog::showDialog");
	if (parentTree_ == NULL)
	{
		printf("Error - NULL Tree pointer found when in AtenCustomDialog::showDialog\n");
		msg.exit("AtenCustomDialog::showDialog");
		return FALSE;
	}
	refreshing_ = FALSE;
	bool result = (exec() == 1 ? TRUE : FALSE);
	refreshing_ = TRUE;
	if (result) storeValues();
	msg.exit("AtenCustomDialog::showDialog");
	return result;
}
