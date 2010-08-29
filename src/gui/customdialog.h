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
class WidgetNode;

// Filter Options Dialog
class AtenCustomDialog : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Widget Functions
	*/
	private slots:


	/*
	// Data
	*/
	private:
	// Whether the window is currently refreshing its controls
	bool refreshing_;
	// Create simple label
	static QLabel *createLabel(const char *text, int alignment);
	// Create empty grid layout
	static QGridLayout *createGridLayout(QWidget *parent);
	// Create check box from data in specified WidgetNode
	static QCheckBox *createCheckBox(WidgetNode *gfo);
	// Create combo box from data in specified GuiFilterOption
	static QComboBox *createComboBox(WidgetNode *gfo);
	// Create double spin edit from data in specified GuiFilterOption
	static QDoubleSpinBox *createDoubleSpinBox(WidgetNode *gfo);
	// Create line edit from data in specified GuiFilterOption
	static QLineEdit *createLineEdit(WidgetNode *gfo);
	// Create spin edit from data in specified GuiFilterOption
	static QSpinBox *createSpinBox(WidgetNode *gfo);
	// Store widget values back into the target variables
	static void storeValues(Tree *filter);

	public:
	// Construct filter option widgets
	static bool createWidgets(Tree *t);
	// Create temporary dialog, displaying options for the specified tree and setting variables within
	static bool show(QString title, Tree *t);

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
