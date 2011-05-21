/*
	*** Atom List Dock Widget
	*** src/gui/atomlist.h
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

#ifndef ATEN_ATOMLISTWIDGET_H
#define ATEN_ATOMLISTWIDGET_H

#include "gui/ui_atomlist.h"
#include <QtCore/QThread>

// Forward declarations
class Model;

// AtomList dock widget
class AtomListWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Atom data columns
	enum DataColumns { IdData=1, ElementData, RxData, RyData, RzData };

	/*
	// Window Functions
	*/
	public:
	void showWidget();
	void refresh();
	private:
	void peekScrollBar();
	void pokeScrollBar();
	void setColumns(TTreeWidgetItem *twi);
	TTreeWidgetItem *itemUnderMouse(const QPoint &pos);
	void toggleItem(TTreeWidgetItem *twi);
	void selectItem(TTreeWidgetItem *twi);
	void deselectItem(TTreeWidgetItem *twi);
	private slots:
	void on_ViewStyleCombo_currentIndexChanged(int index);
	void on_ShiftUpButton_clicked(bool checked);
	void on_ShiftDownButton_clicked(bool checked);
	void on_MoveToStartButton_clicked(bool checked);
	void on_MoveToEndButton_clicked(bool checked);
	void updateSelection();
	void treeMousePressEvent(QMouseEvent *event);
	void treeMouseReleaseEvent(QMouseEvent *event);
	void treeMouseMoveEvent(QMouseEvent *event);
	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Local variables
	*/
	private:
	// Log points of model info displayed in list
	int listStructurePoint_, listSelectionPoint_;
	// Whether the current view is by atom (or not)
	bool viewingByAtom_;
	// Last model displayed in list
	Model *listLastModel_;
	// Whether the widget should refresh when it is next shown
	bool shouldRefresh_;
	// Whether widget is currently refreshing
	bool refreshing_;
	// Last clicked and 'moved over' TTreeWidgetItem in the AtomList
	TTreeWidgetItem *lastClicked_, *lastHovered_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtomListWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::AtomListWidget ui;
};

#endif
