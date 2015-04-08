/*
	*** Tree GUI for Qt
	*** src/gui/treegui_funcs.cpp
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

#include <QtWidgets/QRadioButton>
#include <QtWidgets/QStackedWidget>
#include "gui/treegui.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

/*
// Qt/TreeGuiWidget Map Object
*/

// Constructor
QtWidgetObject::QtWidgetObject() : ListItem<QtWidgetObject>()
{
	// Private variables
	treeGuiWidget_ = NULL;
	qWidget_ = NULL;
	qObject_ = NULL;
	layout_ = NULL;
	labelWidget_ = NULL;
	nextLeft_ = 0;
	nextTop_ = 0;
	autoFillVertical_ = false;
}

// Set TreeGuiWidget/QWidget pair
void QtWidgetObject::set(TreeGuiWidget* widget, QWidget* wid, QString label, QGridLayout *layout)
{
	treeGuiWidget_ = widget;
	qWidget_ = wid;
	labelText_ = label;
	layout_ = layout;
}

// Set TreeGuiWidget/QObject pair
void QtWidgetObject::set(TreeGuiWidget* widget, QObject* obj)
{
	treeGuiWidget_ = widget;
	qObject_ = obj;
	layout_ = NULL;
}

// Return TreeGuiWidget to which Qt widget/object is associated
TreeGuiWidget* QtWidgetObject::treeGuiWidget()
{
	return treeGuiWidget_;
}

// Return associated QWidget (if not QObject)
QWidget* QtWidgetObject::qWidget()
{
	return qWidget_;
}

// Return associated QObject (if not QWidget)
QObject *QtWidgetObject::qObject()
{
	return qObject_;
}

// Return widget's layout, if it has one
QGridLayout *QtWidgetObject::layout()
{
	return layout_;
}

// Return whether to add widgets horizontally or vertically when using automatic fill
bool QtWidgetObject::autoFillVertical()
{
	return autoFillVertical_;
}

// Set whether to add widgets horizontally or vertically when using automatic fill
void QtWidgetObject:: setAutoFillVertical(bool b)
{
	autoFillVertical_ = b;
}

// Update associated QWidget / QObject based on treeGuiWidget_ data
void QtWidgetObject::updateQt()
{
	// Check treeGuiWidget_ pointer first
	if (treeGuiWidget_ == NULL)
	{
		printf("Critical Error: treeGuiWidget_ pointer has not been set in QtWidgetObject.\n");
		return;
	}

	// Do generic properties here
	if (qWidget_ != NULL)
	{
		qWidget_->setEnabled(treeGuiWidget_->enabled());
		qWidget_->setVisible(treeGuiWidget_->visible());
		// And corresponding label
		if (labelWidget_ != NULL)
		{
			labelWidget_->setEnabled(treeGuiWidget_->enabled());
			labelWidget_->setVisible(treeGuiWidget_->visible());
		}
	}

	// Now, check widget type to see what we do
	if (treeGuiWidget_->type() == TreeGuiWidget::ButtonWidget)
	{
		QPushButton *button = static_cast<QPushButton*>(qWidget_);
		if (!button) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QPushButton.\n");
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::CheckWidget)
	{
		QCheckBox *check = static_cast<QCheckBox*>(qWidget_);
		if (!check) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QCheckBox.\n");
		else
		{
			check->setChecked(treeGuiWidget_->valueI() == 1);
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::ComboWidget)
	{
		QComboBox* combo = static_cast<QComboBox*>(qWidget_);
		if (!combo) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QComboBox.\n");
		else
		{
			// Has items list been modified since last update
			if (treeGuiWidget_->propertyChanged(TreeGuiWidgetEvent::ItemsProperty))
			{
				combo->clear();
				for (int n=0; n<treeGuiWidget_->comboItems().count(); ++n) combo->addItem(treeGuiWidget_->comboItems().at(n));
				treeGuiWidget_->resetChanged(TreeGuiWidgetEvent::ItemsProperty);
			}
			// Set current index
			combo->setCurrentIndex(treeGuiWidget_->valueI()-1);
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::DoubleSpinWidget)
	{
		QDoubleSpinBox* spin = static_cast<QDoubleSpinBox*>(qWidget_);
		if (!spin) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QDoubleSpinBox.\n");
		else
		{
			spin->setRange(treeGuiWidget_->minimumD(), treeGuiWidget_->maximumD());
			spin->setValue(treeGuiWidget_->valueD());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::EditWidget)
	{
		QLineEdit *edit = static_cast<QLineEdit*>(qWidget_);
		if (!edit) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QTextEdit.\n");
		else
		{
			edit->setText(treeGuiWidget_->text());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::IntegerSpinWidget)
	{
		QSpinBox *spin = static_cast<QSpinBox*>(qWidget_);
		if (!spin) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QSpinBox.\n");
		else
		{
			spin->setRange(treeGuiWidget_->minimumI(), treeGuiWidget_->maximumI());
			spin->setValue(treeGuiWidget_->valueI());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::LabelWidget)
	{
		QLabel *label = static_cast<QLabel*>(qWidget_);
		if (!label) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QLabel.\n");
		else
		{
			label->setText(treeGuiWidget_->text());
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::RadioButtonWidget)
	{
		QRadioButton *button = static_cast<QRadioButton*>(qWidget_);
		if (!button) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QRadioButton.\n");
		else
		{
			button->setChecked(treeGuiWidget_->valueI() == 1);
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::RadioGroupWidget)
	{
		QButtonGroup *butgroup = static_cast<QButtonGroup*>(qObject_);
		if (!butgroup) printf("Critical Error: Couldn't cast stored qObject_ pointer into QButtonGroup.\n");
		else
		{
			QAbstractButton *button = butgroup->button(treeGuiWidget_->valueI());
			if (!button) printf("Critical Error: Couldn't find button with id %i in button group.\n", treeGuiWidget_->valueI());
			else button->setChecked(true);
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::TabWidget)
	{
		QTabWidget *tabs = static_cast<QTabWidget*>(qWidget_);
		if (!tabs) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QTabWidget.\n");
		else
		{
			tabs->setCurrentIndex(treeGuiWidget_->valueI()-1);
		}
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::StackWidget)
	{
		QStackedWidget *stack = static_cast<QStackedWidget*>(qWidget_);
		if (!stack) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QStackedWidget.\n");
		else
		{
			stack->setCurrentIndex(treeGuiWidget_->valueI()-1);
		}
	}
}

// Update associated CLI widget based on QWidget / QObject data
void QtWidgetObject::updateCLI()
{
	// Check treeGuiWidget_ pointer first
	if (treeGuiWidget_ == NULL)
	{
		printf("Internal Error: treeGuiWidget_ pointer is NULL in updateCLI().\n");
		return;
	}

	// Now, check widget type to see what we do
	if (treeGuiWidget_->type() == TreeGuiWidget::RadioGroupWidget)
	{
		QButtonGroup *butgroup = static_cast<QButtonGroup*>(qObject_);
		if (!butgroup) printf("Critical Error: Couldn't cast stored qObject_ pointer into QButtonGroup.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, butgroup->checkedId());
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::CheckWidget)
	{
		QCheckBox *check = static_cast<QCheckBox*>(qWidget_);
		if (!check) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QCheckBox.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, check->isChecked());
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::ComboWidget)
	{
		QComboBox* combo = static_cast<QComboBox*>(qWidget_);
		if (!combo) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QComboBox.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, combo->currentIndex()+1);
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::DoubleSpinWidget)
	{
		QDoubleSpinBox* spin = static_cast<QDoubleSpinBox*>(qWidget_);
		if (!spin) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QDoubleSpinBox.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, spin->value());
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::EditWidget)
	{
		QLineEdit *edit = static_cast<QLineEdit*>(qWidget_);
		if (!edit) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QTextEdit.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, qPrintable(edit->text()));
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::IntegerSpinWidget)
	{
		QSpinBox *spin = static_cast<QSpinBox*>(qWidget_);
		if (!spin) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QSpinBox.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, spin->value());
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::LabelWidget)
	{
		QLabel *label = static_cast<QLabel*>(qWidget_);
		if (!label) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QLabel.\n");
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::RadioButtonWidget)
	{
		QRadioButton *button = static_cast<QRadioButton*>(qWidget_);
		if (!button) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QRadioButton.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, button->isChecked());
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::TabWidget)
	{
		QTabWidget *tabs = static_cast<QTabWidget*>(qWidget_);
		if (!tabs) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QTabWidget.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, tabs->currentIndex()+1);
	}
	else if (treeGuiWidget_->type() == TreeGuiWidget::StackWidget)
	{
		QStackedWidget *stack = static_cast<QStackedWidget*>(qWidget_);
		if (!stack) printf("Critical Error: Couldn't cast stored qWidget_ pointer into QStackedWidget.\n");
		else treeGuiWidget_->setProperty(TreeGuiWidgetEvent::ValueProperty, stack->currentIndex()+1);
	}
	else printf("Internal Error: No handler written to update CLI controls of this type (%s).\n", TreeGuiWidget::widgetType(treeGuiWidget_->type()));
}

// Add widget to the layout in this widget (if it has one) at specified geometry, and with the specified label
bool QtWidgetObject::addWidget(QtWidgetObject* qtwo, int l, int t, int addToWidth, int addToHeight)
{
	// Safety check - make sure we have a layout
	if (layout_ == NULL)
	{
		printf("Internal Error: No layout to add widget to.\n");
		return false;
	}
	
	// Check that the TreeGuiWidget contains an actual Qt Widget
	if (qtwo->qWidget_ == NULL)
	{
		printf("Internal Error: TreeGuiWidget contains no QWidget to add to layout (or contains a QObject instead).\n");
		return false;
	}
	
	// Are we adding in a specific position, or an automatic one?
	if (l != -1)
	{
		nextLeft_ = l-1;
		nextTop_ = t-1;
	}
	
	// Some widgets have associated labels and take up two+ cells, and some don't.....
	if (qtwo->labelText_.isEmpty() || (qtwo->treeGuiWidget()->type() == TreeGuiWidget::LabelWidget))
	{
		layout_->addWidget(qtwo->qWidget_, nextTop_, nextLeft_, addToHeight+1, addToWidth+1); //, Qt::AlignVCenter);
		if (autoFillVertical_) nextTop_ += addToHeight+1;
		else nextLeft_ += addToWidth+1;
	}
	else
	{
		qtwo->labelWidget_ = new QLabel(qtwo->labelText_, qtwo->qWidget_);
		qtwo->labelWidget_->setMinimumHeight(WIDGETHEIGHT);
		qtwo->labelWidget_->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
		layout_->addWidget(qtwo->labelWidget_, nextTop_, nextLeft_, 1, 1, Qt::AlignRight);
		layout_->addWidget(qtwo->qWidget_, nextTop_, nextLeft_+1, addToHeight+1, addToWidth+1, Qt::AlignVCenter);
		if (autoFillVertical_) nextTop_ += addToHeight+1;
		else nextLeft_ += addToWidth+2;
	}
	return true;
}

// Add widget to the stored layout (provided it has one) at specified geometry
bool QtWidgetObject::addSpacer(bool expandHorizontal, bool expandVertical, int l, int t, int addToWidth, int addToHeight)
{
	// Safety check - make sure we have a layout
	if (layout_ == NULL)
	{
		printf("Internal Error: No layout to add spacer to.\n");
		return false;
	}

	// Are we adding in a specific position, or an automatic one?
	if (l != -1)
	{
		nextLeft_ = l-1;
		nextTop_ = t-1;
	}
	
	// Some widgets have associated labels and take up two+ cells, and some don't.....
	QSpacerItem *spacer = new QSpacerItem(0, WIDGETHEIGHT, expandHorizontal ? QSizePolicy::Expanding : QSizePolicy::Minimum, expandVertical ? QSizePolicy::Expanding : QSizePolicy::Minimum);
	layout_->addItem(spacer, nextTop_, nextLeft_, addToHeight+1, addToWidth+1);
	if (autoFillVertical_) nextTop_ += addToHeight+1;
	else nextLeft_ += addToWidth+1;
	return true;
}

/*
// AtenTreeGuiDialog
*/

// Constructor
AtenTreeGuiDialog::AtenTreeGuiDialog(TreeGui *parent) : QDialog()
{
	updating_ = false;
// 	setVisible(false);
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

// Generic function for button activation
void AtenTreeGuiDialog::buttonWidget_clicked(bool checked)
{
	if (updating_ || (!isVisible())) return;
	// Cast sender into button
	updating_ = true;
	QPushButton *button = (QPushButton*) sender();
	if (!button)
	{
		printf("AtenTreeGuiDialog::checkBoxWidget_clicked - Sender could not be cast into a QPushButton.\n");
		return;
	}

	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == button) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::buttonWidget_valueChanged - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();
	updating_ = false;
}

// Generic function for checkbox activation
void AtenTreeGuiDialog::checkBoxWidget_clicked(bool checked)
{
	if (updating_ || (!isVisible())) return;
	// Cast sender into checkbox
	updating_ = true;
	QCheckBox *check = (QCheckBox*) sender();
	if (!check)
	{
		printf("AtenTreeGuiDialog::checkBoxWidget_clicked - Sender could not be cast into a QCheckBox.\n");
		return;
	}
	
	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == check) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::checkBoxWidget_valueChanged - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();
	updating_ = false;
}

// Generic function for combobox activation
void AtenTreeGuiDialog::comboWidget_currentIndexChanged(int row)
{
	if (updating_ || (!isVisible())) return;
	// Cast sender into combobox
	updating_ = true;
	QComboBox* combo = (QComboBox*) sender();
	if (!combo)
	{
		printf("AtenTreeGuiDialog::comboWidget_currentIndexChanged - Sender could not be cast into a QComboBox.\n");
		return;
	}
	
	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == combo) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::comboWidget_valueChanged - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();
	updating_ = false;
}

// Generic function for double spin activation
void AtenTreeGuiDialog::doubleSpinWidget_valueChanged(double d)
{
	if (updating_ || (!isVisible())) return;
	// Cast sender into double spin widget
	updating_ = true;
	QDoubleSpinBox* spin = static_cast<QDoubleSpinBox*>(sender());
	if (!spin)
	{
		printf("AtenTreeGuiDialog::doubleSpinWidget_valueChanged - Sender could not be cast into a QDoubleSpinBox.\n");
		return;
	}

	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == spin) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::doubleSpinWidget_valueChanged - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();
	updating_ = false;
}

// Generic function for integer spin activation
void AtenTreeGuiDialog::integerSpinWidget_valueChanged(int i)
{
	if (updating_ || (!isVisible())) return;
	// Cast sender into integer spin widget
	updating_ = true;
	QSpinBox *spin = static_cast<QSpinBox*>(sender());
	if (!spin)
	{
		printf("AtenTreeGuiDialog::integerSpinWidget_valueChanged - Sender could not be cast into a QSpinBox.\n");
		return;
	}

	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == spin) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::integerSpinWidget_valueChanged - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();
	updating_ = false;
}

// Generic function for radio group button activation
void AtenTreeGuiDialog::radioGroupWidget_buttonClicked(QAbstractButton *button)
{
	if (updating_ || (!isVisible())) return;

	// Cast sender into button group
	updating_ = true;
	QButtonGroup *group = static_cast<QButtonGroup*>(sender());
	if (!group)
	{
		printf("AtenTreeGuiDialog::radioGroupWidget_buttonClicked - Sender could not be cast into a QButtonGroup.\n");
		return;
	}
	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qObject() == group) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::radioGroupWidget_buttonClicked - couldn't find associated QtWidgetObject for group.\n");
		return;
	}
	qtwo->updateCLI();

	// Now cast supplied abstract button into radio button
	updating_ = true;
	QRadioButton *radio = static_cast<QRadioButton*>(button);
	if (!radio)
	{
		printf("AtenTreeGuiDialog::radioGroupWidget_buttonClicked - Sent button* could not be cast into a QRadioButton.\n");
		return;
	}
	// Search for widget definition in original tree...
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == radio) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::radioGroupWidget_buttonClicked - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();

	updating_ = false;
}

// Generic function for radio button activation
void AtenTreeGuiDialog::radioButtonWidget_clicked(bool checked)
{
	if (updating_ || (!isVisible())) return;
	// Cast sender into radiobutton
	updating_ = true;
	QRadioButton *radio = static_cast<QRadioButton*>(sender());
	if (!radio)
	{
		printf("AtenTreeGuiDialog::radioButtonWidget_clicked - Sender could not be cast into a QRadioButton.\n");
		return;
	}
	
	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == radio) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::radioButtonWidget_clicked - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();
	updating_ = false;
}

// Generic function for radio button activation
void AtenTreeGuiDialog::editWidget_editingFinished()
{
	if (updating_ || (!isVisible())) return;
	// Cast sender into lineedit
	updating_ = true;
	QLineEdit *edit = static_cast<QLineEdit*>(sender());
	if (!edit)
	{
		printf("AtenTreeGuiDialog::editWidget_editingFinished - Sender could not be cast into a QLineEdit.\n");
		return;
	}
	
	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == edit) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::editWidget_editingFinished - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();
	updating_ = false;
}

// Generic function for radio button activation
void AtenTreeGuiDialog::tabsWidget_currentChanged(int id)
{
	if (updating_ || (!isVisible())) return;
	// Cast sender into lineedit
	updating_ = true;
	QTabWidget *tabs = static_cast<QTabWidget*>(sender());
	if (!tabs)
	{
		printf("AtenTreeGuiDialog::tabsWidget_currentChanged - Sender could not be cast into a QTabWidget.\n");
		return;
	}
	
	// Search for widget definition in original tree...
	QtWidgetObject* qtwo;
	for (qtwo = widgetObjects_.first(); qtwo != NULL; qtwo = qtwo->next) if (qtwo->qWidget() == tabs) break;
	if (qtwo == NULL)
	{
		printf("AtenTreeGuiDialog::tabsWidget_currentChanged - couldn't find associated QtWidgetObject.\n");
		return;
	}
	qtwo->updateCLI();
	updating_ = false;
}

// Create new general layout for specified widget
QGridLayout *AtenTreeGuiDialog::addLayout(QWidget* widget)
{
	QGridLayout *layout = new QGridLayout(widget);
	#if QT_VERSION >= 0x040600
	layout->setContentsMargins(2,2,2,2);
	#endif
	layout->setSpacing(2);
	return layout;
}

// Create new button widget
QtWidgetObject* AtenTreeGuiDialog::addButton(TreeGuiWidget* widget, QString label)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QPushButton *button = new QPushButton(this);
	qtwo->set(widget, button);
	button->setText(label);
	button->setEnabled(widget->enabled());
	button->setVisible(widget->visible());
	button->setMinimumHeight(WIDGETHEIGHT);
	button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	// Connect signal to master slot
	QObject::connect(button, SIGNAL(clicked(bool)), this, SLOT(buttonWidget_clicked(bool)));
	return qtwo;
}

// Create new checkbox widget
QtWidgetObject* AtenTreeGuiDialog::addCheck(TreeGuiWidget* widget, QString label)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QCheckBox *check = new QCheckBox(this);
	qtwo->set(widget, check);
	check->setText(label);
	check->setChecked(widget->valueI());
	check->setEnabled(widget->enabled());
	check->setVisible(widget->visible());
	check->setMinimumHeight(WIDGETHEIGHT);
	check->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	// Connect signal to master slot
	QObject::connect(check, SIGNAL(clicked(bool)), this, SLOT(checkBoxWidget_clicked(bool)));
	return qtwo;
}

// Create new combo widget
QtWidgetObject* AtenTreeGuiDialog::addCombo(TreeGuiWidget* widget, QString label)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QComboBox* combo = new QComboBox(this);
	qtwo->set(widget, combo, label);
	// Add items to combo and set current index
	for (int n=0; n<widget->comboItems().count(); ++n) combo->addItem(widget->comboItems().at(n));
	combo->setCurrentIndex(widget->valueI() - 1);
	combo->setEnabled(widget->enabled());
	combo->setVisible(widget->visible());
	combo->setMinimumHeight(WIDGETHEIGHT);
	combo->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	// Connect signal to master slot
	QObject::connect(combo, SIGNAL(currentIndexChanged(int)), this, SLOT(comboWidget_currentIndexChanged(int)));
	return qtwo;
}

// Create new dialog layout
QtWidgetObject* AtenTreeGuiDialog::addDialogLayout(TreeGui *treeGui)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	qtwo->set(treeGui, this, NULL, mainLayout_);
	return qtwo;
}

// Create new double spin widget
QtWidgetObject* AtenTreeGuiDialog::addDoubleSpin(TreeGuiWidget* widget, QString label, double step)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QDoubleSpinBox* spin = new QDoubleSpinBox(this);
	qtwo->set(widget, spin, label);
	spin->setRange(widget->minimumD(), widget->maximumD());
	spin->setValue(widget->valueD());
	spin->setSingleStep(step);
	spin->setDecimals(5);
	spin->setEnabled(widget->enabled());
	spin->setVisible(widget->visible());
	spin->setMinimumHeight(WIDGETHEIGHT);
	spin->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	// Connect signal to master slot
	QObject::connect(spin, SIGNAL(valueChanged(double)), this, SLOT(doubleSpinWidget_valueChanged(double)));
	return qtwo;
}

// Create new edit widget
QtWidgetObject* AtenTreeGuiDialog::addEdit(TreeGuiWidget* widget, QString label)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QLineEdit *edit = new QLineEdit(this);
	qtwo->set(widget, edit, label);
	edit->setText(widget->text());
	edit->setEnabled(widget->enabled());
	edit->setVisible(widget->visible());
	edit->setMinimumHeight(WIDGETHEIGHT);
	edit->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	// Connect signal to master slot
	QObject::connect(edit, SIGNAL(editingFinished()), this, SLOT(editWidget_editingFinished()));
	return qtwo;
}

// Create new frame widget
QtWidgetObject* AtenTreeGuiDialog::addFrame(TreeGuiWidget* widget)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QFrame *frame = new QFrame;

	QGridLayout *layout = addLayout(frame);

	qtwo->set(widget, frame, NULL, layout);
	frame->setEnabled(widget->enabled());
	frame->setVisible(widget->visible());
	frame->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
	return qtwo;
}

// Create new group box
QtWidgetObject* AtenTreeGuiDialog::addGroup(TreeGuiWidget* widget, QString label)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QGroupBox *group = new QGroupBox(label);

	QGridLayout *layout = addLayout(group);

	qtwo->set(widget, group, NULL, layout);
	group->setEnabled(widget->enabled());
	group->setVisible(widget->visible());
	group->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
	return qtwo;
}

// Create new integer spin widget
QtWidgetObject* AtenTreeGuiDialog::addIntegerSpin(TreeGuiWidget* widget, QString label, int step)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QSpinBox *spin = new QSpinBox(this);
	qtwo->set(widget, spin, label);
	spin->setRange(widget->minimumI(), widget->maximumI());
	spin->setValue(widget->valueI());
	spin->setSingleStep(step);
	spin->setEnabled(widget->enabled());
	spin->setVisible(widget->visible());
	spin->setMinimumHeight(WIDGETHEIGHT);
	spin->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
	// Connect signal to master slot
	QObject::connect(spin, SIGNAL(valueChanged(int)), this, SLOT(integerSpinWidget_valueChanged(int)));
	return qtwo;
}

// Create new label widget
QtWidgetObject* AtenTreeGuiDialog::addLabel(TreeGuiWidget* widget, QString text)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QLabel *label = new QLabel(this);
	qtwo->set(widget, label, text);
	label->setText(text);
	label->setEnabled(widget->enabled());
	label->setVisible(widget->visible());
	label->setMinimumHeight(WIDGETHEIGHT);
	label->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
	return qtwo;
}

// Create new page (only in Tab widget or Stack widget)
QtWidgetObject* AtenTreeGuiDialog::addPage(TreeGuiWidget* widget, TreeGuiWidget* parentWidget, QString label)
{
	// Grab widget object...
	QtWidgetObject* wo = parentWidget->qtWidgetObject();
	if (wo == NULL)
	{
		printf("Internal Error: Can't add page to tabwidget since supplied tabwidget doesn't have an associated QtWidgetObject.\n");
		return NULL;
	}

	// Create new page widget and layout
	QWidget* pageWidget = new QWidget(this);
	QGridLayout *layout = addLayout(pageWidget);

	// Create widget object to return
	QtWidgetObject* qtwo = widgetObjects_.add();
	qtwo->set(widget, pageWidget, NULL, layout);
	pageWidget->setEnabled(widget->enabled());
	pageWidget->setVisible(widget->visible());
	pageWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

	// Cast parent widget into correct type, and add page
	if (parentWidget->type() == TreeGuiWidget::TabWidget)
	{
		// Cast QWidget in parentWidget into QTabWidget
		QTabWidget *tabs = static_cast<QTabWidget*>(wo->qWidget());
		if (!tabs)
		{
			printf("Internal Error: Couldn't cast QWidget into QTabWidget.\n");
			return NULL;
		}
		int id = tabs->addTab(pageWidget, label);
	}
	else if (parentWidget->type() == TreeGuiWidget::StackWidget)
	{
		// Cast QWidget in parentWidget into QStackedWidget
		QStackedWidget *stack = static_cast<QStackedWidget*>(wo->qWidget());
		if (!stack)
		{
			printf("Internal Error: Couldn't cast QWidget into QStackedWidget.\n");
			return NULL;
		}
		int id = stack->addWidget(pageWidget);
	}
	else printf("Internal Error: Tried to add a page into a widget type (%s) that doesn't support it.\n", TreeGuiWidget::widgetType(widget->type()));

	return qtwo;
}

// Create new (invisible) radio group
QtWidgetObject* AtenTreeGuiDialog::addRadioGroup(TreeGuiWidget* widget)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QButtonGroup *group = new QButtonGroup(this);
	qtwo->set(widget, group);
	// Connect signal to master slot
	QObject::connect(group, SIGNAL(buttonClicked(QAbstractButton*)), this, SLOT(radioGroupWidget_buttonClicked(QAbstractButton*)));
	return qtwo;
}

// Create new radio button
QtWidgetObject* AtenTreeGuiDialog::addRadioButton(TreeGuiWidget* widget, TreeGuiWidget* groupWidget, QString name, QString label, int id)
{
	// Cast QObject in groupWidget into QButtonGroup
	QtWidgetObject* wo = groupWidget->qtWidgetObject();
	if (wo == NULL)
	{
		printf("Internal Error: Can't add button to radiogroup widget since supplied widget doesn't have an associated QtWidgetObject.\n");
		return NULL;
	}
	QButtonGroup *group = static_cast<QButtonGroup*>(wo->qObject());
	if (!group)
	{
		printf("Internal Error: Couldn't cast QObject into QButtonGroup.\n");
		return NULL;
	}
	// Create new QtWidgetObject for page
	QRadioButton *radio = new QRadioButton(label, this);
	group->addButton(radio, id);
	QtWidgetObject* qtwo = widgetObjects_.add();
	qtwo->set(widget, radio);
	radio->setEnabled(widget->enabled());
	radio->setVisible(widget->visible());
	radio->setChecked(widget->valueI() == 1);
	radio->setMinimumHeight(WIDGETHEIGHT);
	radio->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
	// Connect signal to master slot
	QObject::connect(radio, SIGNAL(clicked(bool)), this, SLOT(radioButtonWidget_clicked(bool)));
	return qtwo;
}

// Create new tab widget
QtWidgetObject* AtenTreeGuiDialog::addStack(TreeGuiWidget* widget)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QStackedWidget *stack = new QStackedWidget(this);
	qtwo->set(widget, stack, "");
	stack->setEnabled(widget->enabled());
	stack->setVisible(widget->visible());
	stack->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
	return qtwo;
}

// Create new tab widget
QtWidgetObject* AtenTreeGuiDialog::addTabs(TreeGuiWidget* widget)
{
	QtWidgetObject* qtwo = widgetObjects_.add();
	QTabWidget *tabs = new QTabWidget(this);
	qtwo->set(widget, tabs, "");
	tabs->setEnabled(widget->enabled());
	tabs->setVisible(widget->visible());
	tabs->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
	// Connect signal to master slot
	QObject::connect(tabs, SIGNAL(currentChanged(int)), this, SLOT(tabsWidget_currentChanged(int)));
	return qtwo;
}

// Execute (show) the custom dialog
bool AtenTreeGuiDialog::execute(QString title)
{
	Messenger::enter("AtenTreeGuiDialog::execute");
	if (parentTree_ == NULL)
	{
		printf("Error - NULL Tree pointer found when in AtenTreeGuiDialog::execute\n");
		Messenger::exit("AtenTreeGuiDialog::execute");
		return false;
	}

	// Set the title of the dialog...
	setWindowTitle(title);
	
	// Run it...
	updating_ = false;
	bool result = (exec() == 1);
	updating_ = true;
	Messenger::exit("AtenTreeGuiDialog::execute");
	return result;
}
