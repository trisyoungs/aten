/*
	*** Qt Custom Dialog
	*** src/gui/customdialog_funcs.cpp
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

#include "gui/customdialog.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "base/sysfunc.h"

// Static singleton
QHBoxLayout AtenCustomDialog::masterLayout_;

// Constructor
AtenCustomDialog::AtenCustomDialog(QWidget *parent) : QDialog(parent)
{
	refreshing_ = FALSE;
	ui.setupUi(this);
}

// Create simple label
QLabel *AtenCustomDialog::createLabel(const char *text, int alignment)
{
	QLabel *label = new QLabel(text);
	label->setAlignment(((Qt::Alignment) alignment)|Qt::AlignVCenter);
	return label;
}

// Create empty grid layout
QGridLayout *AtenCustomDialog::createGridLayout(QWidget *parent)
{
	QGridLayout *layout = new QGridLayout(parent);
	layout->setContentsMargins(2,2,2,2);
	layout->setSpacing(2);
	return layout;
}

// Create check box from data in specified GuiFilterOption
QCheckBox *AtenCustomDialog::createCheckBox(WidgetNode *gfo)
{
	QCheckBox *check = new QCheckBox(gfo->name());
	// Critical : state
	Dnchar data;
	if (!gfo->data("state", data)) printf("Critical: No state found when constructing QCheckBox.\n");
	check->setChecked(data.asInteger());
	return check;
}

// Create combo box from data in specified GuiFilterOption
QComboBox *AtenCustomDialog::createComboBox(WidgetNode *gfo)
{
	QComboBox *combo = new QComboBox();
	// Critical : items list
	Dnchar data;
	if (!gfo->data("items", data)) printf("Critical: No items list found when constructing QComboBox.\n");
	LineParser lp;
	lp.getArgsDelim(data.get());
	for (int n=0; n<lp.nArgs(); ++n) combo->addItem(lp.argc(n));
	// Optional : default index (+1)
	if (!gfo->data("default", data)) printf("Warning: Default value for QComboBox not set.\n");
	combo->setCurrentIndex(data.asInteger()-1);
	return combo;
}

// Create double spin edit from data in specified GuiFilterOption
QDoubleSpinBox *AtenCustomDialog::createDoubleSpinBox(WidgetNode *gfo)
{
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
	return spin;
}

// Create line edit from data in specified GuiFilterOption
QLineEdit *AtenCustomDialog::createLineEdit(WidgetNode *gfo)
{
	QLineEdit *lineedit = new QLineEdit();
	// Critical : text
	Dnchar data;
	if (!gfo->data("text", data)) printf("Critical: No text found when constructing QLineEdit.\n");
	lineedit->setText(data.get());
	return lineedit;
}

// Create spin box from data in specified GuiFilterOption
QSpinBox *AtenCustomDialog::createSpinBox(WidgetNode *gfo)
{
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
	return spin;
}

// Construct filter option widgets for specified tree
bool AtenCustomDialog::createWidgets(Tree *t)
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
	Reflist<QTabWidget,Dnchar> tabwidgets;
	Refitem<QTabWidget,Dnchar> *tabref;
	Dnchar name;
	LayoutData *mainlayout, *currentlayout, *newlayout;
	QWidget *widget;

	// Get start of list of defined options - if there are none, do nothing
	if (t->widgets() == NULL)
	{
		t->setMainWidget(NULL);
		msg.exit("AtenCustomDialog::createWidgets");
		return TRUE;
	}

	// Create main layout widget for the other widgets
	layouts.clear();
	tabwidgets.clear();
	QWidget *mainwidget = new QWidget(NULL);
	masterLayout_.addWidget(mainwidget);
	t->setMainWidget(mainwidget);
	gridl = createGridLayout(mainwidget);
	mainlayout = layouts.add("_MAIN_", gridl);

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
void AtenCustomDialog::storeValues(Tree *filter)
{
	msg.enter("AtenCustomDialog::storeValues");
	WidgetNode *gfo;
	QWidget *widget;
	ReturnValue rv;
	for (Refitem<WidgetNode,int> *ri = filter->widgets(); ri != NULL; ri = ri->next)
	{
		gfo = ri->item;
		rv.reset();
		switch (gfo->controlType())
		{
			case (WidgetNode::CheckControl):
				rv.set( ((QCheckBox*) (gfo->widget()))->isChecked());
				break;
			case (WidgetNode::IntegerComboControl):
				rv.set( ((QComboBox*) (gfo->widget()))->currentIndex());
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
bool AtenCustomDialog::show(QString title, Tree *t)
{
	msg.enter("AtenCustomDialog::show");
	if (t == NULL)
	{
		printf("NULL Tree pointer passed to AtenCustomDialog::show\n");
		msg.exit("AtenCustomDialog::show");
		return FALSE;
	}
	// If there is no layout return immediately
	if (t->mainWidget() == NULL) return TRUE;
	// Create a temporary dialog and add widget layout to it...
	AtenCustomDialog *dialog = new AtenCustomDialog(gui.mainWindow);
	// Set title and widget
	dialog->setWindowTitle(title);
	dialog->ui.MainLayout->addWidget(t->mainWidget());
	bool result = (dialog->exec() == 1 ? TRUE : FALSE);
	if (result) AtenCustomDialog::storeValues(t);
	dialog->ui.MainLayout->removeWidget(t->mainWidget());
	// Need to reparent the widget so it doesn't get deleted along with the dialog
	masterLayout_.addWidget(t->mainWidget());
	delete dialog;
	msg.exit("AtenCustomDialog::show");
	return result;
}
