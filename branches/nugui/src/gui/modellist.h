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

// Model List dock widget
class ModelListWidget : public QDockWidget
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

	/*
	// Local variables
	*/
	private:

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
