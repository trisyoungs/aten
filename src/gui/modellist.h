/*
	*** Model List Dock Widget
	*** src/gui/modellist.h
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

#ifndef ATEN_MODELLISTWIDGET_H
#define ATEN_MODELLISTWIDGET_H

#include "gui/ui_modellist.h"
#include "gui/textratreewidgetitem.h"
#include "gui/ttreewidgetitem.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_USING_NAMESPACE

// Model List dock widget
class ModelListWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWidget();
	void refresh();
	private:
	void updateItem(TExtraTreeWidgetItem *item);
	TExtraTreeWidgetItem *itemUnderMouse(const QPoint &pos);
	void toggleItem(TExtraTreeWidgetItem *twi);
	void deselectAll(TExtraTreeWidgetItem* selectitem);
	private slots:
	void on_RefreshIconsButton_clicked(bool checked);
	void on_ModelsPerRowSpin_valueChanged(int value);
	void treeMousePressEvent(QMouseEvent *event);
	void treeMouseReleaseEvent(QMouseEvent *event);
	void treeMouseMoveEvent(QMouseEvent *event);
	void treeMouseDoubleClickEvent(QMouseEvent *event);
	void renameModel(bool checked);
	void closeSelectedModels(bool checked);
	void closeUnselectedModels(bool checked);
	void closeThisModel(bool checked);

	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Local variables
	*/
	private:
	// Context Menu
	QMenu *contextMenu_;
	// Whether the widget is currently refreshing
	bool refreshing_;
	// Last clicked and 'moved over' TExtraTreeWidgetItem in the ModelList
	TExtraTreeWidgetItem *lastClicked_, *lastHovered_;
	
	/*
	// Dialog
	*/
	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	ModelListWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::ModelListWidget ui;
};

#endif
