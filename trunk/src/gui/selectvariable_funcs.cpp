/*
	*** Qt select variable functions interface
	*** src/gui/selectvariable_funcs.cpp
	Copyright T. Youngs 2007-2013

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

#include "gui/selectvariable.h"
#include "classes/zmatrix.h"

// Constructor
AtenSelectVariable::AtenSelectVariable(QWidget *parent) : QDialog(parent)
{
	// Private variables
	selectedVariable_ = NULL;
	zMatrix_ = NULL;
	variableType_ = -1;
	refreshing_ = FALSE;

	ui.setupUi(this);
}

// Get variable of given type, based on Table row number
Variable *AtenSelectVariable::getVariable(int row)
{
	Variable *v = NULL;
	if (row == -1) v = NULL;
	else
	{
		switch (variableType_)
		{
			case (0):
				v = zMatrix_->distance(row);
				break;
			case (1):
				v = zMatrix_->angle(row);
				break;
			case (2):
				v = zMatrix_->torsion(row);
				break;
			default:
				printf("Internal Error: VariableType is unrecognised in AtenSelectVariable.\n");
				break;
		}
	}
	return v;
}

void AtenSelectVariable::on_VariableTable_doubleClicked(const QModelIndex &index)
{
	selectedVariable_ = getVariable(ui.VariableTable->currentRow());
	accept();
}

// Item selection changed
void AtenSelectVariable::on_VariableTable_itemSelectionChanged()
{
	selectedVariable_ = getVariable(ui.VariableTable->currentRow());
}

// Select a variable of the specified type from the zmatrix provided
Variable *AtenSelectVariable::selectVariable(ZMatrix *zmat, int vartype, Variable *currentVar, bool currentNegate)
{
	// Check source
	if (zmat == NULL) return NULL;
	zMatrix_ = zmat;

	// Clear list and repopulate with variables of the correct type
	QTableWidgetItem *item;
	int count, nvars;
	Variable *vars;
	// Get number of relevant variables of specified type, and start of list...
	variableType_ = vartype;
	switch (variableType_)
	{
		case (0):
			nvars = zmat->nDistances();
			vars = zmat->distances();
			break;
		case (1):
			nvars = zmat->nAngles();
			vars = zmat->angles();
			break;
		case (2):
			nvars = zmat->nTorsions();
			vars = zmat->torsions();
			break;
		default:
			printf("Internal Error: Specified vartype given to AtenSelectVariable::selectVariable is unrecognised.\n");
			return NULL;
			break;
	}

	// Clear and update list
	refreshing_ = TRUE;
	ui.VariableTable->clear();
	ui.VariableTable->setHorizontalHeaderLabels(QStringList() << "Variable" << "Current Value");
	ui.VariableTable->setRowCount(nvars);
	ReturnValue rv;
	count = 0;
	for (Variable *v = vars; v != NULL; v = (Variable*) v->next)
	{
		// Set variable name
		item = new QTableWidgetItem(v->name());
		ui.VariableTable->setItem(count, 0, item);
		// Set value
		v->execute(rv);
		item = new QTableWidgetItem(rv.asString());
		ui.VariableTable->setItem(count, 1, item);
		// Select?
		if (currentVar == v) ui.VariableTable->setCurrentItem(item);
		count ++;
	}
	for (count=0; count<2; count++) ui.VariableTable->resizeColumnToContents(count);
	ui.NegateCheck->setChecked(currentNegate);
	refreshing_ = FALSE;

	// Execute the dialog and check on the result
	return (exec() == 1 ? selectedVariable_ : NULL);
}

// Return status of negate checkbox
bool AtenSelectVariable::isNegated()
{
	return ui.NegateCheck->isChecked();
}
