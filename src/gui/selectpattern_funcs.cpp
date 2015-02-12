/*
	*** Qt selectpattern functions interface
	*** src/gui/selectpattern_funcs.cpp
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

#include "gui/mainwindow.h"
#include "gui/selectpattern.h"
#include "ff/forcefield.h"
#include "model/model.h"
#include "base/pattern.h"

// Constructor
AtenSelectPattern::AtenSelectPattern(AtenWindow& parent) : QDialog(&parent), parent_(parent)
{
	ui.setupUi(this);

	// Private variables
	currentModel_ = NULL;
	selectedPattern_ = NULL;
}

void AtenSelectPattern::on_PatternTable_doubleClicked(const QModelIndex &index)
{
	int row = index.row();
	if (row == -1) selectedPattern_ = NULL;
	else selectedPattern_ = currentModel_->pattern(row);
	accept();
}

// Item selection changed
void AtenSelectPattern::on_PatternTable_itemSelectionChanged()
{
	int row = ui.PatternTable->currentRow();
	if (row == -1) selectedPattern_ = NULL;
	else selectedPattern_ = currentModel_->pattern(row);
}

// Select a pattern from the specified model
Pattern* AtenSelectPattern::selectPattern(Model* source)
{
	// Set the currentmodel
	currentModel_ = source;

	// If model is NULL return NULL
	if (currentModel_ == NULL) return NULL;

	// First, call createPatterns...
	if (!currentModel_->createPatterns()) return NULL;

	// If there are no patterns in the model, return NULL
	if (currentModel_->nPatterns() == 0) return NULL;

	// Clear list and repopulate
	QTableWidgetItem *item;
	int count = 0;
	Forcefield *ff;
	ui.PatternTable->clear();
	ui.PatternTable->setHorizontalHeaderLabels(QStringList() << "Pattern" << "Forcefield");
	ui.PatternTable->setRowCount(currentModel_->nPatterns());
	for (Pattern* p = currentModel_->patterns(); p != NULL; p = p->next)
	{
		// Set pattern name
		item = new QTableWidgetItem(p->name());
		ui.PatternTable->setItem(count, 0, item);
		// Set forcefield name
		ff = p->forcefield();
		item = new QTableWidgetItem(ff == NULL ? "<Inherited>" : ff->name());
		ui.PatternTable->setItem(count, 1, item);
		count ++;
	}
	for (count=0; count<2; count++) ui.PatternTable->resizeColumnToContents(count);

	// Execute the dialog and check on the result
	return (exec() == 1 ? selectedPattern_ : NULL);
}
