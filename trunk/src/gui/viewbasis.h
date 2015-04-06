/*
	*** View Basis Dialog
	*** src/gui/viewbasis.h
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

#ifndef ATEN_VIEWBASISWINDOW_H
#define ATEN_VIEWBASISWINDOW_H

#include "gui/ui_viewbasis.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// ViewBasis Window
class AtenViewBasis : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	AtenViewBasis(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::ViewBasisDialog ui;
	// Table columns
	enum TableColumns { AtomIdColumn, ShellColumn, TypeColumn, ExponentColumn, CoefficientColumn, nColumns };


	/*
	 * Local Variables
	 */
	private:
	// Current model target
	Model* target_;


	/*
	 * Window Functions
	 */
	public:
	void showWindow(Model* m);
	private slots:
	void dialogFinished(int result);
};

#endif
