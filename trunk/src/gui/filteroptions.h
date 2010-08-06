/*
	*** Qt Filter Options dialog
	*** src/gui/filteroptions.h
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

#ifndef ATEN_FILTEROPTIONSWINDOW_H
#define ATEN_FILTEROPTIONSWINDOW_H

#include "gui/gui.h"
#include "gui/ui_filteroptions.h"
#include "gui/layoutlist.h"

// Forward Declarations
class Tree;
class QComboBox;
class QSpinEdit;
class QDoubleSpinEdit;
class QLineEdit;
class QCheckBox;
class GuiFilterOptionNode;

// Filter Options Dialog
class AtenFilterOptions : public QDialog
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
	QLabel *createLabel(const char *text);
	// Create empty grid layout
	QGridLayout *createGridLayout(QWidget *parent);
	// Create check box from data in specified GuiFilterOption
	QCheckBox *createCheckBox(GuiFilterOptionNode *gfo);
	// Create combo box from data in specified GuiFilterOption
	QComboBox *createComboBox(GuiFilterOptionNode *gfo);
	// Create double spin edit from data in specified GuiFilterOption
	QDoubleSpinBox *createDoubleSpinBox(GuiFilterOptionNode *gfo);
	// Create line edit from data in specified GuiFilterOption
	QLineEdit *createLineEdit(GuiFilterOptionNode *gfo);
	// Create spin edit from data in specified GuiFilterOption
	QSpinBox *createSpinBox(GuiFilterOptionNode *gfo);
	// Store widget values back into the filter options structures
	void storeFilterOptions(Tree *filter);

	public:
	// Construct filter option widgets
	bool createFilterOptionWidgets();
	// Call the dialog, displaying options for the specified filter
	bool show(Tree *filter);

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenFilterOptions(QWidget *parent = 0);
	// Main form declaration
	Ui::FilterOptionsDialog ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
