/*
	*** Qt GUI: ZMatrix Window
	*** src/gui/zmatrix.h
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

#ifndef ATEN_ZMATRIXWINDOW_H
#define ATEN_ZMATRIXWINDOW_H

#include "gui/ui_zmatrix.h"

// Forward declarations
class Model;

// ZMatrix Window
class AtenZMatrix : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Data columns
	enum DataColumns { IdData=1, ElementData, RxData, RyData, RzData };

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	void refresh(bool forceupdate = FALSE);
	private:

	/*
	// Local variables
	*/
	private:
	// Whether the widget should refresh when it is next shown
	bool shouldRefresh_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenZMatrix(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~AtenZMatrix();
	// Main form declaration
	Ui::ZMatrixDialog ui;
};

#endif
