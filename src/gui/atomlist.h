/*
	*** Atom List Dock Widget
	*** src/gui/atomlist.h
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

#ifndef ATEN_ATOMLISTWIDGET_H
#define ATEN_ATOMLISTWIDGET_H

#include "gui/ui_atomlist.h"
#include "base/atom.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class QItemDelegate;
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// AtomList dock widget
class AtomListWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	AtomListWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Table Columns
	enum AtomTableItem { AtomIdItem, AtomElementItem, AtomTypeItem, AtomXItem, AtomYItem, AtomZItem, AtomQItem, nAtomItems };
	// Main form declaration
	Ui::AtomListWidget ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Local variables
	 */
	private:
	// Custom item delegates for each column
	QItemDelegate* AtomListItemDelegates[AtomListWidget::nAtomItems];
	// Log points of model info displayed in list
	int listStructurePoint_, listSelectionPoint_;
	// Whether the current view is by atom (or not)
	bool viewingByAtom_;
	// Array of currently-visible items
	bool visibleItems_[nAtomItems];
	// List of currently-visible atom data
	QList<int> displayItems_;
	// Last model displayed in list
	Model* listLastModel_;
	// Current root atom id of model (ID displayed in row 1)
	int currentRootId_;
	// Whether the widget should refresh when it is next shown
	bool shouldRefresh_;
	// Whether widget is currently refreshing
	bool refreshing_;
	// Number of rows displayed in AtomTable
	int maxTableRows_;
	// Reflist of currently-displayed atoms
	Reflist<Atom,int> atomItems_;
	// Last clicked and 'moved over' Atom
	Atom* prevClicked_, *lastClicked_, *lastHovered_;


	/*
	 * Window Functions
	 */
	public:
	void showWidget();
	void refresh();

	private:
	void recalculateRowSize();
	void updateRow(int row);
	void updateDisplayItems();
	void updateSelection();
	Atom* atomInRow(int row);
	void toggleItem(Atom* i);

	private slots:
	void on_AtomTableScrollBar_valueChanged(int value);
	void on_ViewStyleCombo_currentIndexChanged(int index);
	void on_ShiftUpButton_clicked(bool checked);
	void on_ShiftDownButton_clicked(bool checked);
	void on_MoveToStartButton_clicked(bool checked);
	void on_MoveToEndButton_clicked(bool checked);
	void on_ViewElementCheck_clicked(bool checked);
	void on_ViewIdCheck_clicked(bool checked);
	void on_ViewTypeCheck_clicked(bool checked);
	void on_ViewXCheck_clicked(bool checked);
	void on_ViewYCheck_clicked(bool checked);
	void on_ViewZCheck_clicked(bool checked);
	void on_ViewChargeCheck_clicked(bool checked);

	protected:
	void closeEvent(QCloseEvent* event);
	void resizeEvent(QResizeEvent* event);
	void wheelEvent(QWheelEvent* event);

	public slots:
	void tableMousePressEvent(QMouseEvent* event);
	void tableMouseReleaseEvent(QMouseEvent* event);
	void tableMouseMoveEvent(QMouseEvent* event);
	void tableMouseDoubleClickEvent(QMouseEvent* event);
	void tableItemChanged(QTableWidgetItem *item);
};

#endif
