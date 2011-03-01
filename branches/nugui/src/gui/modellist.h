/*
	*** Model List Dock Widget
	*** src/gui/modellist.h
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

#ifndef ATEN_MODELLISTWIDGET_H
#define ATEN_MODELLISTWIDGET_H

#include "gui/ui_modellist.h"
#include "gui/tlistwidgetitem.h"

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
	TTreeWidgetItem *itemUnderMouse(const QPoint &pos);
	void toggleItem(TTreeWidgetItem *twi);
	void selectItem(TTreeWidgetItem *twi);
	void deselectItem(TTreeWidgetItem *twi);
	private slots:
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
	// Whether the widget is currently refreshing
	bool refreshing_;
	// Last clicked and 'moved over' TTreeWidgetItem in the ModelList
	TTreeWidgetItem *lastClicked_, *lastHovered_;
	
	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	ModelListWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~ModelListWidget();
	// Main form declaration
	Ui::ModelListWidget ui;
};

#endif
