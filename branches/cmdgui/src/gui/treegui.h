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
	// Whether widget(s) are being refreshed
	static bool refreshing_;
	// TreeGuiWidget to which Qt widget/object is associated
	TreeGuiWidget *treeGuiWidget_;
	// Associated QWidget (if not QObject)
	QWidget *qWidget_;
	// Associated QObject (if not QWidget)
	QObject *qObject_;
	// Text for associated label
	Dnchar label_;
	// Widget's layout, if it has one
	QGridLayout *layout_;

	public:
	// Set TreeGuiWidget/QWidget pair
	void set(TreeGuiWidget *widget, QWidget *wid, const char *label, QGridLayout *layout = NULL);
	// Set TreeGuiWidget/QObject pair
	void set(TreeGuiWidget *widget, QObject *obj, const char *label, QGridLayout *layout = NULL);
	// Return whether currently refreshing
	bool refreshing();
	// Return TreeGuiWidget to which Qt widget/object is associated
	TreeGuiWidget *treeGuiWidget();
	// Return associated QWidget (if not QObject)
	QWidget *qWidget();
	// Return associated QObject (if not QWidget)
	QObject *qObject();
	// Return text for associated label
	const char *label();
	// Return widget's layout, if it has one
	QGridLayout *layout();


	/*
	// Methods
	*/
	public:
	// Update associated QWidget / QObject based on treeGuiWidget_ data
	void update();
	// Add widget to the stored layout (provided it has one) at specified geometry
	bool addWidget(TreeGuiWidget *widget, int l, int r, int addToWidth, int addToHeight);
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
	// Create new dialog layout
	QtWidgetObject *addDialogLayout(TreeGui *widget);
	// Create new combo widget
	QtWidgetObject *addCombo(TreeGuiWidget *widget, const char *label);
	// Create new integer spin widget
	QtWidgetObject *addIntegerSpin(TreeGuiWidget *widget, const char *label, int step);
	// Create new double spin widget
	QtWidgetObject *addDoubleSpin(TreeGuiWidget *widget, const char *label, double step);
	// Create new label widget
	QtWidgetObject *addLabel(TreeGuiWidget *widget, const char *text);
	// Create new edit widget
	QtWidgetObject *addEdit(TreeGuiWidget *widget, const char *label);
	// Create new checkbox widget
	QtWidgetObject *addCheck(TreeGuiWidget *widget, const char *label);
	// Create new tab widget
	QtWidgetObject *addTabs(TreeGuiWidget *widget);
	// Create new page (only in tab widget)
	QtWidgetObject *addPage(TreeGuiWidget *widget, TreeGuiWidget *tabWidget, const char *label);
	// Create new group box
	QtWidgetObject *addGroup(TreeGuiWidget *widget, const char *label);
	// Create new (invisible) radio group
	QtWidgetObject *addRadioGroup(TreeGuiWidget *widget);
	// Create new radio button
	QtWidgetObject *addRadioButton(TreeGuiWidget *widget, TreeGuiWidget *groupWidget, const char *name, const char *label, int id);
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
