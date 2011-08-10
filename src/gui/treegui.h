/*
	*** Tree GUI for Qt
	*** src/gui/treegui.h
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

#ifndef ATEN_TREEGUIQT_H
#define ATEN_TREEGUIQT_H

#include "gui/gui.h"
#include "gui/ui_treegui.h"
#include "templates/list.h"

// Forward Declarations
class TreeGui;

// Qt/TreeGuiWidget Map Object
class QtWidgetObject
{
	public:
	// Constructor
	QtWidgetObject();
	// List pointers
	QtWidgetObject *prev, *next;
	
	private:
	// TreeGuiWidget to which Qt widget/object is associated
	TreeGuiWidget *treeGuiWidget_;
	// Associated QWidget (if not QObject)
	QWidget *qWidget_;
	// Associated QObject (if not QWidget)
	QObject *qObject_;
	// Whether widget(s) are being refreshed
	static bool refreshing_;

	public:
	// Set TreeGuiWidget/QWidget pair
	void set(TreeGuiWidget *widget, QWidget *wid);
	// Set TreeGuiWidget/QObject pair
	void set(TreeGuiWidget *widget, QObject *obj);
	// Update associated QWidget / QObject based on treeGuiWidget_ data
	void update();
	// Return whether currently refreshing
	bool refreshing();
};

// Tree Gui Qt Dialog
class AtenTreeGuiDialog : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Generic Widget Functions
	*/
	private slots:
	void checkBoxWidget_clicked(bool checked);
	void comboWidget_currentIndexChanged(int row);
	void doubleSpinWidget_valueChanged(double d);
	void integerSpinWidget_valueChanged(int i);
	void buttonGroupWidget_buttonClicked(int id);
	void radioButtonWidget_clicked(bool checked);


	/*
	// Widgets
	*/
	private:
	// Whether the window is currently refreshing its controls
	bool refreshing_;
	// Main Grid layout
	QGridLayout *mainLayout_;
	// Associated TreeGui parent
	TreeGui *parentTree_;
	// List of Qt/TreeGui pairs
	List<QtWidgetObject> widgetObjects_;
	
	public:
	// Create new combo widget
	QWidget *addCombo(TreeGuiWidget *widget, const char *label, const char *items, int index);
	// Create new integer spin widget
	QWidget *addIntegerSpin(TreeGuiWidget *widget, const char *label, int min, int max, int step, int value);
	// Create new double spin widget
	QWidget *addDoubleSpin(TreeGuiWidget *widget, const char *label, double min, double max, double step, double value);
	// Create new label widget
	QWidget *addLabel(TreeGuiWidget *widget, const char *text);
	// Create new edit widget
	QWidget *addEdit(TreeGuiWidget *widget, const char *label, const char *text);
	// Create new checkbox widget
	QWidget *addCheck(TreeGuiWidget *widget, const char *label, int state);
	// Create new tab widget
	QWidget *addTabs(TreeGuiWidget *widget);
	// Create new page (only in tab widget)
	QWidget *addPage(TreeGuiWidget *widget, const char *label);
	// Create new group box
	QWidget *addGroup(TreeGuiWidget *widget, const char *label);
	// Create new (invisible) radio group
	QObject *addRadioGroup(const char *name);
	// Create new radio button
	QWidget *addRadioButton(const char *name, const char *label, int state);
	// Perform specified state change
// 	void performStateChange(StateChange *sc);
	// Execute (show) dialog
	bool execute();


	/*
	// Widgets
	*/
	public:
	// Constructor / Destructor
	AtenTreeGuiDialog(TreeGui *parent = 0);
	~AtenTreeGuiDialog();
	// Main form declaration
	Ui::AtenTreeGuiDialog ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
