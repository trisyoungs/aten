/*
	*** Select Variable Dialog
	*** src/gui/selectvariable.h
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

#ifndef ATEN_SELECTVARIABLEDIALOG_H
#define ATEN_SELECTVARIABLEDIALOG_H

#include "gui/ui_selectvariable.h"

// Forward Declarations
class Variable;
class ZMatrix;
class AtenWindow;

// Select Variable Dialog
class AtenSelectVariable : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor
	AtenSelectVariable(AtenWindow& parent);
	// Main form declaration
	Ui::AtenSelectVariable ui;


	/*
	 * Widget Functions
	 */
	private slots:
	void on_VariableTable_doubleClicked(const QModelIndex &index);
	void on_VariableTable_itemSelectionChanged();

	
	/*
	 * Local Variables
	 */
	private:
	// Whether the list is refreshing
	bool refreshing_;
	// Source ZMatrix
	ZMatrix* zMatrix_;
	// Target variable type
	int variableType_;
	// Variable selected in list
	Variable* selectedVariable_;
	// Get variable of given type, based on Table row number
	Variable* getVariable(int row);


	/*
	 * Methods
	 */
	public:
	// Select a variable from the specified ZMatrix
	Variable* selectVariable(ZMatrix* zmat, int vartype, Variable* currentVar, bool currentNegate);
	// Return status of negate checkbox
	bool isNegated();

};

#endif
