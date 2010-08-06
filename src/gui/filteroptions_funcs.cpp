/*
	*** Qt Filter Options dialog
	*** src/gui/filteroptions_funcs.cpp
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

#include "gui/filteroptions.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "base/sysfunc.h"

// Constructor
AtenFilterOptions::AtenFilterOptions(QWidget *parent) : QDialog(parent)
{
	refreshing_ = FALSE;
	ui.setupUi(this);
}

// Create simple label
QLabel *AtenFilterOptions::createLabel(const char *text)
{
	QLabel *label = new QLabel(text);
	label->setAlignment(Qt::AlignRight|Qt::AlignVCenter);
	return label;
}

// Create empty grid layout
QGridLayout *AtenFilterOptions::createGridLayout(QWidget *parent)
{
	QGridLayout *layout = new QGridLayout(parent);
	layout->setContentsMargins(2,2,2,2);
	layout->setSpacing(2);
	return layout;
}

// Create check box from data in specified GuiFilterOption
QCheckBox *AtenFilterOptions::createCheckBox(GuiFilterOptionNode *gfo)
{
	QCheckBox *check = new QCheckBox(gfo->name());
	// Critical : state
	Dnchar data;
	if (!gfo->data("state", data)) printf("Critical: No state found when constructing QCheckBox.\n");
	check->setChecked(data.asInteger());
	return check;
}

// Create combo box from data in specified GuiFilterOption
QComboBox *AtenFilterOptions::createComboBox(GuiFilterOptionNode *gfo)
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
QDoubleSpinBox *AtenFilterOptions::createDoubleSpinBox(GuiFilterOptionNode *gfo)
{
	QDoubleSpinBox *spin = new QDoubleSpinBox();
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
QLineEdit *AtenFilterOptions::createLineEdit(GuiFilterOptionNode *gfo)
{
	QLineEdit *lineedit = new QLineEdit();
	// Critical : text
	Dnchar data;
	if (!gfo->data("text", data)) printf("Critical: No text found when constructing QLineEdit.\n");
	lineedit->setText(data.get());
	return lineedit;
}

// Create spin box from data in specified GuiFilterOption
QSpinBox *AtenFilterOptions::createSpinBox(GuiFilterOptionNode *gfo)
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

// Construct filter option widgets
bool AtenFilterOptions::createFilterOptionWidgets()
{
	msg.enter("AtenFilterOptions::createFilterOptionWidgets");
	Refitem<Tree,int> *ri, *rj;
	Tree *filter;
	Refitem<GuiFilterOptionNode,int> *firstri, *gfori;
	GuiFilterOptionNode *gfo;
	QGroupBox *group;
	QGridLayout *gridl;
	QTabWidget *tabw;
	int span, labelspan;
	bool newline;
	LayoutList layouts;
	Reflist<QTabWidget,Dnchar> tabwidgets;
	Refitem<QTabWidget,Dnchar> *tabref;
	Dnchar name;
	LayoutData *mainlayout, *currentlayout, *newlayout;
	QWidget *widget;
	// Cycle over filter types
	for (int n=0; n<FilterData::nFilterTypes; ++n)
	{
		for (ri = aten.filters((FilterData::FilterType) n); ri != NULL; ri = ri->next)
		{
			filter = ri->item;

			// Create main layout widget for the other widgets
			layouts.clear();
			tabwidgets.clear();
			gridl = createGridLayout(NULL);
			filter->setLayout(gridl);
			mainlayout = layouts.add("_MAIN_", gridl);

			// Get start of list of defined options - if there are none, add a simple label and continue
			firstri = filter->guiFilterOptions();
			if (firstri == NULL)
			{
				widget = new QLabel("No options have been defined for this filter.");
				mainlayout->addWidget(widget, 1, FALSE);
				continue;
			}

			// Create widgets
			for (gfori = firstri; gfori != NULL; gfori = gfori->next)
			{
				gfo = gfori->item;
				// We will add to the main widget, unless another is specified (i.e. a group)
				if (gfo->widgetParentType() == GuiFilterOptionNode::NoParent) currentlayout = mainlayout;
				else
				{
					// Find the specified layout (or create it)
					currentlayout = layouts.find(gfo->widgetParentName());
					if (currentlayout == NULL)
					{
						if (gfo->widgetParentType() == GuiFilterOptionNode::GroupBoxParent)
						{
							// Create a new QGroupBox in the main widget (for now...)
							group = new QGroupBox();
							group->setTitle(gfo->widgetParentName());
							mainlayout->addWidget(group, gfo->widgetParentSpan(), gfo->widgetNewLine());
							gridl = createGridLayout(group);
							currentlayout = layouts.add(gfo->widgetParentName(), gridl);
						}
						else if (gfo->widgetParentType() == GuiFilterOptionNode::TabWidgetParent)
						{
							// Format of name should be 'page@tabwidget'
							// Two possibilities -  1) the 'tabwidget' does exist but the page doesn't
							//			2) the 'tabwidget' doesn't exit, and nor does the page
							// So, search for tabwidget...
							name = afterChar(gfo->widgetParentName(), '@');
// 							printf("Searching for tab '%s'\n", name.get());
							for (tabref = tabwidgets.first(); tabref != NULL; tabref = tabref->next)
								if (name == tabref->data) break;
							if (tabref == NULL)
							{
// 								printf("No tab found. Creating both tab and page.\n");
								tabw = new QTabWidget();
								mainlayout->addWidget(tabw, gfo->widgetParentSpan(), gfo->widgetNewLine());
								tabwidgets.add(tabw, name);
							}
							else
							{
// 								printf("Tab widget found - adding new page.\n");
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

				// Now create the widget
				switch (gfo->controlType())
				{
					// Check Box - data: state)
					case (GuiFilterOptionNode::CheckType):
						widget = createCheckBox(gfo);
						currentlayout->addWidget(widget, span, newline);
						gfo->setWidget(widget);
						break;
					// Combo Box - data:  items, default
					case (GuiFilterOptionNode::IntegerComboType):
					case (GuiFilterOptionNode::ComboType):
						widget = createLabel(gfo->name());
						currentlayout->addWidget(widget, labelspan, newline);
						widget = createComboBox(gfo);
						currentlayout->addWidget(widget, span, FALSE);
						gfo->setWidget(widget);
						break;
					// Spin Edit - data: min, max, start
					case (GuiFilterOptionNode::DoubleSpinType):
						widget = createLabel(gfo->name());
						currentlayout->addWidget(widget, labelspan, newline);
						widget = createDoubleSpinBox(gfo);
						currentlayout->addWidget(widget, span, FALSE);
						gfo->setWidget(widget);
						break;
					// Spin Edit - data: text
					case (GuiFilterOptionNode::EditType):
						widget = createLabel(gfo->name());
						currentlayout->addWidget(widget, labelspan, newline);
						widget = createLineEdit(gfo);
						currentlayout->addWidget(widget, span, FALSE);
						gfo->setWidget(widget);
						break;
					// Spin Edit - data: min, max, start
					case (GuiFilterOptionNode::SpinType):
						widget = createLabel(gfo->name());
						currentlayout->addWidget(widget, labelspan, newline);
						widget = createSpinBox(gfo);
						currentlayout->addWidget(widget, span, FALSE);
						gfo->setWidget(widget);
						break;
				}

				// Apply general options
				if (!gfo->widgetEnabled()) widget->setEnabled(FALSE);
			}
		}
	}
	msg.exit("AtenFilterOptions::createFilterOptionWidgets");
}

// Store widget values back into the filter options structures
void AtenFilterOptions::storeFilterOptions(Tree *filter)
{
	msg.enter("AtenFilterOptions::storeFilterOptions");
	GuiFilterOptionNode *gfo;
	QWidget *widget;
	ReturnValue rv;
	for (Refitem<GuiFilterOptionNode,int> *ri = filter->guiFilterOptions(); ri != NULL; ri = ri->next)
	{
		gfo = ri->item;
		rv.reset();
		switch (gfo->controlType())
		{
			case (GuiFilterOptionNode::CheckType):
				rv.set( ((QCheckBox*) (gfo->widget()))->isChecked());
				break;
			case (GuiFilterOptionNode::IntegerComboType):
				rv.set( ((QComboBox*) (gfo->widget()))->currentIndex());
				break;
			case (GuiFilterOptionNode::ComboType):
				rv.set( qPrintable(((QComboBox*) (gfo->widget()))->currentText()));
				break;
			case (GuiFilterOptionNode::DoubleSpinType):
				rv.set( ((QDoubleSpinBox*) (gfo->widget()))->value());
				break;
			case (GuiFilterOptionNode::EditType):
				rv.set( qPrintable(((QLineEdit*) (gfo->widget()))->text()));
				break;
			case (GuiFilterOptionNode::SpinType):
				rv.set( ((QSpinBox*) (gfo->widget()))->value());
				break;
		}
		gfo->setReturnValue(rv);
	}
	msg.exit("AtenFilterOptions::storeFilterOptions");
}

// Call the dialog, displaying options for the specified filter
bool AtenFilterOptions::show(Tree *filter)
{
	msg.enter("AtenFilterOptions::show");
	if (filter == NULL)
	{
		printf("NULL pointer passed to AtenFilterOptions::show\n");
		msg.exit("AtenFilterOptions::show");
		return FALSE;
	}
	// If there is no layout (i.e. no options) return immediately
	if (filter->layout() == NULL) return TRUE;
	// Set title and central widgets
	ui.FilterTypeLabel->setText( filter->filter.description() );
	ui.CentralWidget->setLayout(filter->layout());
	bool result = (exec() == 1 ? TRUE : FALSE);
	if (result) storeFilterOptions(filter);
	msg.exit("AtenFilterOptions::show");
	return result;
}
