/*
	*** Qt Custom Dialog
	*** src/gui/customdialog.h
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

#ifndef ATEN_CUSTOMDIALOG_H
#define ATEN_CUSTOMDIALOG_H

#include "gui/gui.h"
#include "gui/ui_customdialog.h"
#include "gui/layoutlist.h"

// Forward Declarations
class Tree;
class QComboBox;
class QSpinEdit;
class QDoubleSpinEdit;
class QLineEdit;
class QCheckBox;
class QRadioButton;
class QStackWidget;
class QButtonGroup;
class WidgetNode;
class StateChange;

// Filter Options Dialog
class AtenCustomDialog : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Generic Widget Functions
	*/
	private:
	void performStateChange(StateChange *sc);
	private slots:
	void checkBoxWidget_clicked(bool checked);
	void comboWidget_currentIndexChanged(int row);
	void doubleSpinWidget_valueChanged(double d);
	void integerSpinWidget_valueChanged(int i);
	void buttonGroupWidget_buttonClicked(int id);
	void radioButtonWidget_clicked(bool checked);

	/*
	// Data
	*/
	private:
	// Whether the window is currently refreshing its controls
	bool refreshing_;
	// Main Grid layout
	QGridLayout *mainLayout_;
	// Parent tree target
	Tree *parentTree_;
	// Create simple label
	QLabel *createLabel(const char *text, int alignment);
	// Create empty grid layout
	QGridLayout *createGridLayout(QWidget *parent);
	// Create check box from data in specified WidgetNode
	QCheckBox *createCheckBox(WidgetNode *gfo);
	// Create radio button from data in specified WidgetNode
	QRadioButton *createRadioButton(WidgetNode *gfo, KVTable<Dnchar,QButtonGroup*> &buttonGroups);
	// Create radiogroup from data in specified WidgetNode
	QButtonGroup *createRadioGroup(WidgetNode *gfo, KVTable<Dnchar,QButtonGroup*> &buttonGroups);
	// Create combo box from data in specified GuiFilterOption
	QComboBox *createComboBox(WidgetNode *gfo);
	// Create double spin edit from data in specified GuiFilterOption
	QDoubleSpinBox *createDoubleSpinBox(WidgetNode *gfo);
	// Create line edit from data in specified GuiFilterOption
	QLineEdit *createLineEdit(WidgetNode *gfo);
	// Create spin edit from data in specified GuiFilterOption
	QSpinBox *createSpinBox(WidgetNode *gfo);
	// Create stack widget from data in specified GuiFilterOption
	QStackedWidget *createStackedWidget(WidgetNode* gfo, LayoutList& layoutList);
	
	public:
	// Construct filter option widgets
	bool createWidgets(const char *title, Tree *t);
	// Store widget values back into the target variables
	void storeValues();
	// Show defined dialog, displaying options for the specified tree and setting variables within
	bool showDialog();

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenCustomDialog(QWidget *parent = 0);
	// Main form declaration
	Ui::AtenCustomDialog ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
