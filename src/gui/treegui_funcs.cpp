/*
	*** Tree GUI for Qt
	*** src/gui/treegui_funcs.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "base/sysfunc.h"

/*
// Qt/TreeGuiWidget Map Object
*/

// Constructor
QtWidgetObject::QtWidgetObject()
{
	// Public variables
	prev = NULL;
	next = NULL;
	
	// Private variables
	treeGuiWidget_ = NULL;
	qWidget_ = NULL;
	qObject_ = NULL;
}

// Set TreeGuiWidget/QWidget pair
void QtWidgetObject::set(TreeGuiWidget *widget, QWidget *wid)
{
	treeGuiWidget_ = widget;
	qWidget_ = wid;
}

// Set TreeGuiWidget/QObject pair
void QtWidgetObject::set(TreeGuiWidget *widget, QObject *obj)
{
	treeGuiWidget_ = widget;
	qObject_ = obj;
}

// Update associated QWidget / QObject based on treeGuiWidget_ data
void QtWidgetObject::update()
{
	refreshing_ = TRUE;
	// Check treeGuiWidget_ pointer first
	if (treeGuiWidget_ == NULL)
	{
		printf("Critical Error: treeGuiWidget_ pointer has not been set in QtWidgetObject.\n");
		return;
	}
	// Now, check widget type to see what we do
	if (treeGuiWidget_->type() == TreeGuiWidget::ButtonGroupWidget)
	{
		QButtonGroup *butgroup = (QButtonGroup*) qObject_;
		if (!butgroup) printf("Critical Error: Couldn't cast stored qObject_ pointer into QButtonGroup.\n");
		else
		{
			QAbstractButton *button = butgroup->button(treeGuiWidget_->valueI());
			if (!button) printf("Critical Error: Couldn't find button with id %i in button group.\n", treeGuiWidget_->valueI());
			else button->setChecked(TRUE);
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::CheckWidget)
	{
		QCheckBox *check = (QCheckBox*) qWidget_;
		if (!check) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QCheckBox.\n");
		else
		{
			check->setChecked(treeGuiWidget_->valueI() == 1);
			check->setEnabled(treeGuiWidget_->enabled());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::ComboWidget)
	{
		QComboBox *combo = (QComboBox*) qWidget_;
		if (!combo) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QComboBox.\n");
		else
		{
			// Has items list been modified since last update
			if (treeGuiWidget_->itemsChanged())
			{
				combo->clear();
				for (Dnchar *d = treeGuiWidget_->items(); d != NULL; d = d->next) combo->addItem(d->get());
				treeGuiWidget_->resetItemsChanged();
			}
			// Set current index
			combo->setCurrentIndex(treeGuiWidget_->valueI());
			// Set enabled property
			combo->setEnabled(treeGuiWidget_->enabled());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::DoubleSpinWidget)
	{
		QDoubleSpinBox *spin = (QDoubleSpinBox*) qWidget_;
		if (!spin) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QDoubleSpinBox.\n");
		else
		{
			spin->setRange(treeGuiWidget_->minimumD(), treeGuiWidget_->maximumD());
			spin->setValue(treeGuiWidget_->valueD());
			spin->setEnabled(treeGuiWidget_->enabled());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::EditWidget)
	{
		QTextEdit *edit = (QTextEdit*) qWidget_;
		if (!edit) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QTextEdit.\n");
		else
		{
			edit->setText(treeGuiWidget_->text());
			edit->setEnabled(treeGuiWidget_->enabled());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::IntegerSpinWidget)
	{
		QSpinBox *spin = (QSpinBox*) qWidget_;
		if (!spin) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QSpinBox.\n");
		else
		{
			spin->setRange(treeGuiWidget_->minimumI(), treeGuiWidget_->maximumI());
			spin->setValue(treeGuiWidget_->valueI());
			spin->setEnabled(treeGuiWidget_->enabled());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::LabelWidget)
	{
		QLabel *label = (QLabel*) qWidget_;
		if (!label) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QLabel.\n");
		else
		{
			label->setText(treeGuiWidget_->text());
			label->setEnabled(treeGuiWidget_->enabled());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::RadioButtonWidget)
	{
		QRadioButton *button = (QRadioButton*) qWidget_;
		if (!button) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QRadioButton.\n");
		else
		{
			button->setChecked(treeGuiWidget_->valueI() == 1);
			button->setEnabled(treeGuiWidget_->enabled());
		}
	}
	else printf("Critical Error: No handler written to update Qt controls of this type.\n");
	refreshing_ = FALSE;
}

// Return whether currently refreshing
bool QtWidgetObject::refreshing()
{
	return refreshing_;
}

/*
// AtenTreeGuiDialog
*/

// Constructor
AtenTreeGuiDialog::AtenTreeGuiDialog(TreeGui *parent) : QDialog(NULL)
{
	refreshing_ = FALSE;
	ui.setupUi(this);
	// Add grid layout to MainFrame
	mainLayout_ = new QGridLayout(ui.MainFrame);
	parentTree_ = parent;
}

// Destructor
AtenTreeGuiDialog::~AtenTreeGuiDialog()
{
	// Remove any self-generated QButtonGroups
// 	for (KVData<Dnchar,QButtonGroup*> *bg = buttonGroups_.pairs(); bg != NULL; bg = bg->next) delete bg->value();
}

// Create new combo widget
QWidget *AtenTreeGuiDialog::addCombo(TreeGuiWidget *widget, const char *label, const char *items, int index)
{
	QtWidgetObject *qtwo = widgetObjects_.add();
	QComboBox *combo = new QComboBox;
	qtwo->set(widget, combo);
}

// Create new integer spin widget
QWidget *AtenTreeGuiDialog::addIntegerSpin(TreeGuiWidget *widget, const char *label, int min, int max, int step, int value)
{
}

// Create new double spin widget
QWidget *AtenTreeGuiDialog::addDoubleSpin(TreeGuiWidget *widget, const char *label, double min, double max, double step, double value)
{
}

// Create new label widget
QWidget *AtenTreeGuiDialog::addLabel(TreeGuiWidget *widget, const char *text)
{
}

// Create new edit widget
QWidget *AtenTreeGuiDialog::addEdit(TreeGuiWidget *widget, const char *label, const char *text)
{
}

// Create new checkbox widget
QWidget *AtenTreeGuiDialog::addCheck(TreeGuiWidget *widget, const char *label, int state)
{
}

// Create new tab widget
QWidget *AtenTreeGuiDialog::addTabs(TreeGuiWidget *widget)
{
}

// Create new page (only in tab widget)
QWidget *AtenTreeGuiDialog::addPage(TreeGuiWidget *widget, const char *label)
{
}

// Create new group box
QWidget *AtenTreeGuiDialog::addGroup(TreeGuiWidget *widget, const char *name)
{
}

// Create new (invisible) radio group
QObject *AtenTreeGuiDialog::addRadioGroup(const char *name)
{
}

// Create new radio button
QWidget *AtenTreeGuiDialog::addRadioButton(const char *name, const char *label, int state)
{
}



// // Perform specified state change
// void AtenTreeGuiDialog::performStateChange(StateChange *sc)
// {
// 	// First, find relevant widget definition
// 	WidgetNode *node = parentTree_->findWidget(sc->targetWidget());
// 	if (node == NULL)
// 	{
// 		printf("AtenTreeGuiDialog::performStateChange - Unable to locate widget '%s'.\n", sc->targetWidget());
// 		return;
// 	}
// 	// Proceed based on widget type
// 	QComboBox *combo;
// 	QSpinBox *spin;
// 	QDoubleSpinBox *doublespin;
// 	QCheckBox *check;
// 	QLineEdit *line;
// 	QStackedWidget *stack;
// 	QButtonGroup *buttongroup;
// 	QAbstractButton *button;
// 	QLabel *label;
// 	Dnchar data;
// 	LineParser lp;
// 	int n;
// 	switch (node->controlType())
// 	{
// 		// Check Box
// 		case (WidgetNode::CheckControl):
// 			check = (QCheckBox*) node->widget();
// 			switch (sc->changeAction())
// 			{
// 				case (StateChange::CheckedAction):
// 					check->setChecked(sc->changeDataAsBool());
// 					break;
// 				case (StateChange::DisableAction):
// 					check->setEnabled(FALSE);
// 					break;
// 				case (StateChange::EnableAction):
// 					check->setEnabled(TRUE);
// 					break;
// 				default:
// 					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
// 			}
// 			break;
// 		// RadioGroup
// 		case (WidgetNode::RadioGroupControl):
// 			buttongroup = (QButtonGroup*) node->widget();
// 			switch (sc->changeAction())
// 			{
// 				case (StateChange::ValueAction):
// 					// Search for button with supplied id
// 					button = buttongroup->button(sc->changeDataAsInteger());
// 					if (button == NULL) printf("Warning - Couldn't find button %i in buttongroup.\n", sc->changeDataAsInteger());
// 					else button->setChecked(TRUE);
// 					break;
// 				default:
// 					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
// 			}
// 			break;
// 		// Combo Box
// 		case (WidgetNode::IntegerComboControl):
// 		case (WidgetNode::ComboControl):
// 			combo = (QComboBox*) node->widget();
// 			switch (sc->changeAction())
// 			{
// 				case (StateChange::DisableAction):
// 					combo->setEnabled(FALSE);
// 					break;
// 				case (StateChange::EnableAction):
// 					combo->setEnabled(TRUE);
// 					break;
// 				case (StateChange::ItemsAction):
// 					lp.getArgsDelim(LineParser::UseQuotes, sc->changeData());
// 					for (n=0; n<lp.nArgs(); ++n) combo->addItem(lp.argc(n));
// 					combo->setCurrentIndex(0);
// 					break;
// 				case (StateChange::OriginalItemsAction):
// 					if (!node->data("items", data)) printf("Critical: No items list found when constructing QComboBox.\n");
// 					lp.getArgsDelim(LineParser::UseQuotes, data.get());
// 					for (n=0; n<lp.nArgs(); ++n) combo->addItem(lp.argc(n));
// 					combo->setCurrentIndex(0);
// 					break;
// 				case (StateChange::ValueAction):
// 					combo->setCurrentIndex(sc->changeDataAsInteger());
// 					break;
// 				default:
// 					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
// 			}
// 			break;
// 		// Double Spin Edit
// 		case (WidgetNode::DoubleSpinControl):
// 			doublespin = (QDoubleSpinBox*) node->widget();
// 			switch (sc->changeAction())
// 			{
// 				case (StateChange::DisableAction):
// 					doublespin->setEnabled(FALSE);
// 					break;
// 				case (StateChange::EnableAction):
// 					doublespin->setEnabled(TRUE);
// 					break;
// 				case (StateChange::MinimumAction):
// 					doublespin->setMinimum(sc->changeDataAsDouble());
// 					break;
// 				case (StateChange::MaximumAction):
// 					doublespin->setMaximum(sc->changeDataAsDouble());
// 					break;
// 				case (StateChange::StepAction):
// 					doublespin->setSingleStep(sc->changeDataAsDouble());
// 					break;
// 				case (StateChange::ValueAction):
// 					doublespin->setValue(sc->changeDataAsDouble());
// 					break;
// 				default:
// 					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
// 			}
// 			break;
// 		// Text Edit
// 		case (WidgetNode::EditControl):
// 			line = (QLineEdit*) node->widget();
// 			switch (sc->changeAction())
// 			{
// 				case (StateChange::DisableAction):
// 					line->setEnabled(FALSE);
// 					break;
// 				case (StateChange::EnableAction):
// 					line->setEnabled(TRUE);
// 					break;
// 				default:
// 					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
// 			}
// 			break;
// 		// Integer spin edit
// 		case (WidgetNode::IntegerSpinControl):
// 			spin = (QSpinBox*) node->widget();
// 			switch (sc->changeAction())
// 			{
// 				case (StateChange::DisableAction):
// 					spin->setEnabled(FALSE);
// 					break;
// 				case (StateChange::EnableAction):
// 					spin->setEnabled(TRUE);
// 					break;
// 				case (StateChange::MinimumAction):
// 					spin->setMinimum(sc->changeDataAsInteger());
// 					break;
// 				case (StateChange::MaximumAction):
// 					spin->setMaximum(sc->changeDataAsInteger());
// 					break;
// 				case (StateChange::StepAction):
// 					spin->setSingleStep(sc->changeDataAsInteger());
// 					break;
// 				case (StateChange::ValueAction):
// 					spin->setValue(sc->changeDataAsInteger());
// 					break;
// 				default:
// 					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
// 			}
// 			break;
// 		// Label
// 		case (WidgetNode::LabelControl):
// 			label = (QLabel*) node->widget();
// 			switch (sc->changeAction())
// 			{
// 				case (StateChange::ValueAction):
// 					label->setText(sc->changeData());
// 					break;
// 				default:
// 					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
// 			}
// 			break;
// 		// Stack control
// 		case (WidgetNode::StackControl):
// 			stack = (QStackedWidget*) node->widget();
// 			switch (sc->changeAction())
// 			{
// 				case (StateChange::DisableAction):
// 					stack->setEnabled(FALSE);
// 					break;
// 				case (StateChange::EnableAction):
// 					stack->setEnabled(TRUE);
// 					break;
// 				case (StateChange::SwitchStackAcion):
// 					// Get new index
// 					n = sc->stateValueAsInteger() - 1;
// 					stack->setCurrentIndex(n);
// 					break;
// 				default:
// 					msg.print("Warning - State change '%s' is not valid for a control of type '%s'.\n", StateChange::stateAction(sc->changeAction()), WidgetNode::guiControl(node->controlType()));
// 			}
// 			break;
// // 	}
// }

// Generic function for checkbox activation
void AtenTreeGuiDialog::checkBoxWidget_clicked(bool checked)
{
	if (!isVisible()) return;
	// Cast sender into checkbox
	refreshing_ = TRUE;
	QCheckBox *check = (QCheckBox*) sender();
	if (!check)
	{
		printf("AtenTreeGuiDialog::checkBoxWidget_clicked - Sender could not be cast to a QCheckBox.\n");
		return;
	}
// 	// Search for widget definition in original tree...
// 	WidgetNode *node = parentTree_->findWidget(check);
// 	if (node == NULL)
// 	{
// 		printf("AtenTreeGuiDialog::checkBoxWidget_clicked - couldn't find associated WidgetNode.\n");
// 		return;
// 	}
// 	// Check all states defined in the widgetnode
// 	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
// 	{
// 		if (sc->dynamicValue())
// 		{
// 			sc->setStateValue(checked);
// 			performStateChange(sc);
// 		}
// 		else if (checked && ((int)sc->stateValueAsInteger() > 0)) performStateChange(sc);
// 		else if ((!checked) && ((int)sc->stateValueAsInteger() < 1)) performStateChange(sc);
// 	}
	refreshing_ = FALSE;
}

// Generic function for combobox activation
void AtenTreeGuiDialog::comboWidget_currentIndexChanged(int row)
{
	if (!isVisible()) return;
	// Cast sender into combobox
	refreshing_ = TRUE;
	QComboBox *combo = (QComboBox*) sender();
	if (!combo)
	{
		printf("AtenTreeGuiDialog::comboWidget_currentIndexChanged - Sender could not be cast to a QComboBox.\n");
		return;
	}
// 	// Search for widget definition in original tree...
// 	WidgetNode *node = parentTree_->findWidget(combo);
// 	if (node == NULL)
// 	{
// 		printf("AtenTreeGuiDialog::comboWidget_currentIndexChanged - couldn't find associated WidgetNode.\n");
// 		return;
// 	}
// 	// Check all states defined in the widgetnode
// 	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
// 	{
// 		// If StateChange has a dynamic value, set it now, otherwise compare by integer value or string depending on control type
// 		if (sc->dynamicValue())
// 		{
// 			sc->setStateValue(row+1);
// 			performStateChange(sc);
// 		}
// 		else if (node->controlType() == WidgetNode::IntegerComboControl)
// 		{
// 			if ((row+1) == sc->stateValueAsInteger()) performStateChange(sc);
// 			else if (strcmp(sc->stateValue(),"*") == 0) performStateChange(sc);
// 		}
// 		else if (node->controlType() == WidgetNode::ComboControl)
// 		{
// 			if (strcmp(qPrintable(combo->currentText()), sc->stateValue()) == 0) performStateChange(sc);
// 			else if (strcmp(sc->stateValue(),"*") == 0) performStateChange(sc);
// 		}
// 	}
	refreshing_ = FALSE;
}

// Generic function for double spin activation
void AtenTreeGuiDialog::doubleSpinWidget_valueChanged(double d)
{
	if (!isVisible()) return;
	// Cast sender into checkbox
	refreshing_ = TRUE;
	QDoubleSpinBox *spin = (QDoubleSpinBox*) sender();
	if (!spin)
	{
		printf("AtenTreeGuiDialog::doubleSpinWidget_valueChanged - Sender could not be cast to a QDoubleSpinBox.\n");
		return;
	}
// 	// Search for widget definition in original tree...
// 	WidgetNode *node = parentTree_->findWidget(spin);
// 	if (node == NULL)
// 	{
// 		printf("AtenTreeGuiDialog::doubleSpinWidget_valueChanged - couldn't find associated WidgetNode.\n");
// 		return;
// 	}
// 	// Check all states defined in the widgetnode
// 	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
// 	{
// 		if (sc->dynamicValue())
// 		{
// 			sc->setStateValue(d);
// 			performStateChange(sc);
// 		}
// 		else if (d == sc->stateValueAsDouble()) performStateChange(sc);
// 	}
	refreshing_ = FALSE;
}

// Generic function for integer spin activation
void AtenTreeGuiDialog::integerSpinWidget_valueChanged(int i)
{
	if (!isVisible()) return;
	// Cast sender into checkbox
	refreshing_ = TRUE;
	QSpinBox *spin = (QSpinBox*) sender();
	if (!spin)
	{
		printf("AtenTreeGuiDialog::integerSpinWidget_valueChanged - Sender could not be cast to a QSpinBox.\n");
		return;
	}
// 	// Search for widget definition in original tree...
// 	WidgetNode *node = parentTree_->findWidget(spin);
// 	if (node == NULL)
// 	{
// 		printf("AtenTreeGuiDialog::integerSpinWidget_valueChanged - couldn't find associated WidgetNode.\n");
// 		return;
// 	}
// 	// Check all states defined in the widgetnode
// 	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
// 	{
// 		if (sc->dynamicValue())
// 		{
// 			sc->setStateValue(i);
// 			performStateChange(sc);
// 		}
// 		else if (i == sc->stateValueAsInteger()) performStateChange(sc);
// 	}
	refreshing_ = FALSE;
}

// Generic function for radio group button activation
void AtenTreeGuiDialog::buttonGroupWidget_buttonClicked(int index)
{
	if (!isVisible()) return;
	// Cast sender into checkbox
	refreshing_ = TRUE;
	QButtonGroup *radio = (QButtonGroup*) sender();
	if (!radio)
	{
		printf("AtenTreeGuiDialog::buttonGroupWidget_buttonClicked - Sender could not be cast to a QButtonGroup.\n");
		return;
	}
// 	// Search for widget definition in original tree...
// 	WidgetNode *node = parentTree_->findWidgetObject(radio);
// 	if (node == NULL)
// 	{
// 		printf("AtenTreeGuiDialog::buttonGroupWidget_buttonClicked - couldn't find associated WidgetNode.\n");
// 		return;
// 	}
// 	// Check all states defined in the widgetnode
// 	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
// 	{
// 		if (sc->dynamicValue())
// 		{
// 			sc->setStateValue(index+1);
// 			performStateChange(sc);
// 		}
// 		else if (index == sc->stateValueAsInteger()) performStateChange(sc);
// 	}
	refreshing_ = FALSE;
}

// Generic function for radio button activation
void AtenTreeGuiDialog::radioButtonWidget_clicked(bool checked)
{
	if (!isVisible()) return;
	// Cast sender into radiobutton
	refreshing_ = TRUE;
	QRadioButton *check = (QRadioButton*) sender();
	if (!check)
	{
		printf("AtenTreeGuiDialog::radioButtonWidget_clicked - Sender could not be cast to a QRadioButton.\n");
		return;
	}
// 	// Search for widget definition in original tree...
// 	WidgetNode *node = parentTree_->findWidget(check);
// 	if (node == NULL)
// 	{
// 		printf("AtenTreeGuiDialog::radioButtonWidget_clicked - couldn't find associated WidgetNode.\n");
// 		return;
// 	}
// 	// Check all states defined in the widgetnode
// 	for (StateChange *sc = node->stateChanges(); sc != NULL; sc = sc->next)
// 	{
// 		if (sc->dynamicValue())
// 		{
// 			sc->setStateValue(checked);
// 			performStateChange(sc);
// 		}
// 		else if (checked && ((int)sc->stateValueAsInteger() > 0)) performStateChange(sc);
// 		else if ((!checked) && ((int)sc->stateValueAsInteger() < 1)) performStateChange(sc);
// 	}
	refreshing_ = FALSE;
}

/*// Create simple label
QLabel *AtenTreeGuiDialog::createLabel(const char *text, int alignment)
{
	msg.enter("AtenTreeGuiDialog::createLabel");
	QLabel *label = new QLabel(text);
	label->setAlignment(((Qt::Alignment) alignment)|Qt::AlignVCenter);
	label->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
	msg.exit("AtenTreeGuiDialog::createLabel");
	return label;
}

// Create empty grid layout
QGridLayout *AtenTreeGuiDialog::createGridLayout(QWidget *parent)
{
	msg.enter("AtenTreeGuiDialog::createGridLayout");
	QGridLayout *layout = new QGridLayout(parent);
	#if QT_VERSION >= 0x040600
	layout->setContentsMargins(2,2,2,2);
	#endif
	layout->setSpacing(2);
	msg.exit("AtenTreeGuiDialog::createGridLayout");
	return layout;
}

// Create check box from data in specified GuiFilterOption
QCheckBox *AtenTreeGuiDialog::createCheckBox(WidgetNode *gfo)
{
	msg.enter("AtenTreeGuiDialog::createCheckBox");
	QCheckBox *check = new QCheckBox(gfo->name());
	// Critical : state
	Dnchar data;
	if (!gfo->data("state", data)) printf("Critical: No state found when constructing QCheckBox.\n");
	check->setChecked(data.asInteger());
	msg.exit("AtenTreeGuiDialog::createCheckBox");
	return check;
}

// Create radio button from data in specified WidgetNode
QRadioButton *AtenTreeGuiDialog::createRadioButton(WidgetNode *gfo)
{
	msg.enter("AtenTreeGuiDialog::createRadioButton");
	QRadioButton *radio = new QRadioButton(gfo->name());
	Dnchar data;
	// Critical : parent buttongroup
	if (!gfo->data("buttongroup", data)) printf("Critical: No parent buttongroup found when constructing QRadioButton.\n");
	// Search to see if specific key is in the table
	KVData<Dnchar,QButtonGroup*> *bg = buttonGroups_.search(data);
	if (bg == NULL)
	{
		QButtonGroup *butgroup = new QButtonGroup();
		butgroup->addButton(radio, 1);
		buttonGroups_.add(data, butgroup);
	}
	else bg->value()->addButton(radio, bg->value()->buttons().count()+1);
	// Critical : state
	if (!gfo->data("state", data)) printf("Critical: No state found when constructing QRadioButton.\n");
	radio->setChecked(data.asInteger());
	msg.exit("AtenTreeGuiDialog::createRadioButton");
	return radio;
}

// Create radiogroup from data in specified GuiFilterOption
QButtonGroup *AtenTreeGuiDialog::createRadioGroup(WidgetNode* gfo)
{
	msg.enter("AtenTreeGuiDialog::createRadioGroup");
	// Search for existing button group
	QButtonGroup *buttongroup;
	KVData<Dnchar,QButtonGroup*> *bg = buttonGroups_.search(gfo->name());
	if (bg == NULL)
	{
		buttongroup = new QButtonGroup(this);
		buttonGroups_.add(gfo->name(), buttongroup);
		QObject::connect(buttongroup, SIGNAL(buttonClicked(int)), this, SLOT(buttonGroupWidget_buttonClicked(int)));
	}
	else buttongroup = bg->value();
	msg.exit("AtenTreeGuiDialog::createRadioGroup");
	return buttongroup;
}

// Create combo box from data in specified GuiFilterOption
QComboBox *AtenTreeGuiDialog::createComboBox(WidgetNode *gfo)
{
	msg.enter("AtenTreeGuiDialog::createComboBox");
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
	msg.exit("AtenTreeGuiDialog::createComboBox");
	return combo;
}

// Create double spin edit from data in specified GuiFilterOption
QDoubleSpinBox *AtenTreeGuiDialog::createDoubleSpinBox(WidgetNode *gfo)
{
	msg.enter("AtenTreeGuiDialog::createDoubleSpinBox");
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
	msg.exit("AtenTreeGuiDialog::createDoubleSpinBox");
	return spin;
}

// Create line edit from data in specified GuiFilterOption
QLineEdit *AtenTreeGuiDialog::createLineEdit(WidgetNode *gfo)
{
	msg.enter("AtenTreeGuiDialog::createLineEdit");
	QLineEdit *lineedit = new QLineEdit();
	// Critical : text
	Dnchar data;
	if (!gfo->data("text", data)) printf("Critical: No text found when constructing QLineEdit.\n");
	lineedit->setText(data.get());
	lineedit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	msg.exit("AtenTreeGuiDialog::createLineEdit");
	return lineedit;
}

// Create spin box from data in specified GuiFilterOption
QSpinBox *AtenTreeGuiDialog::createSpinBox(WidgetNode *gfo)
{
	msg.enter("AtenTreeGuiDialog::createSpinBox");
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
	msg.exit("AtenTreeGuiDialog::createSpinBox");
	return spin;
}

// Create line edit from data in specified GuiFilterOption
QStackedWidget *AtenTreeGuiDialog::createStackedWidget(WidgetNode* gfo, LayoutList& layoutList)
{
	msg.enter("AtenTreeGuiDialog::createStackedWidget");
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
	
	msg.exit("AtenTreeGuiDialog::createStackedWidget");
	return stack;
}
*/


// Call the dialog, displaying options for the specified filter
bool AtenTreeGuiDialog::execute()
{
	msg.enter("AtenTreeGuiDialog::execute");
	if (parentTree_ == NULL)
	{
		printf("Error - NULL Tree pointer found when in AtenTreeGuiDialog::execute\n");
		msg.exit("AtenTreeGuiDialog::execute");
		return FALSE;
	}
	refreshing_ = FALSE;
	bool result = (exec() == 1 ? TRUE : FALSE);
	refreshing_ = TRUE;
// 	if (result) storeValues();
	msg.exit("AtenTreeGuiDialog::execute");
	return result;
}
