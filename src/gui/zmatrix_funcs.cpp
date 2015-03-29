/*
	*** ZMatrix Dock Widget
	*** src/gui/zmatrix_funcs.cpp
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

#include "main/aten.h"
#include "gui/zmatrix.h"
#include "gui/selectvariable.h"
#include "gui/mainwindow.h"
#include "base/messenger.h"
#include "base/sysfunc.h"
#include "parser/commandnode.h"

// Constructor
AtenZMatrix::AtenZMatrix(AtenWindow& parent, Qt::WindowFlags flags) : QDialog(&parent, flags), parent_(parent)
{
	refreshing_ = false;
	zMatrix_ = NULL;
	ui.setupUi(this);
}

void AtenZMatrix::showWidget()
{
	refresh();
	show();
}

// Refresh the zmatrix
void AtenZMatrix::refresh(bool forceupdate)
{
	Messenger::enter("AtenZMatrix::refresh");

	// Grab (and create) zmatrix for current model
	Model* m = parent_.aten().currentModelOrFrame();
	zMatrix_ = m->zMatrix();
	if (zMatrix_ == NULL) return;
	
	refreshing_ = true;
	// ZMatrix 'Connectivity' Table
	int count = 0;
	QTableWidgetItem *item;
	Atom* i;
	ui.ZMatrixTable->setRowCount(zMatrix_->nElements());
	ui.ZMatrixTable->setColumnCount(AtenZMatrix::nColumns);
	ui.ZMatrixTable->setHorizontalHeaderLabels(QStringList() << "El" << "Id" << "D" << "Id" << "A" << "Id" << "T");
	for (ZMatrixElement* zel = zMatrix_->elements(); zel != NULL; zel = zel->next)
	{
		// First atom (the creation target)
		i = zel->atom(0);
		item = new QTableWidgetItem(Elements().symbol(i));
		ui.ZMatrixTable->setItem(count, AtenZMatrix::SymbolColumn, item);
		// Second atom (distance specifier)
		i = zel->atom(1);
		if (i != NULL)
		{
			item = new QTableWidgetItem(QString::number(i->id()+1));
			ui.ZMatrixTable->setItem(count, AtenZMatrix::DistanceAtomColumn, item);
			item = new QTableWidgetItem(zel->distanceVariable()->name());
			ui.ZMatrixTable->setItem(count, AtenZMatrix::DistanceColumn, item);
			
			// Third atom (angle specifier)
			i = zel->atom(2);
			if (i != NULL)
			{
				item = new QTableWidgetItem(QString::number(i->id()+1));
				ui.ZMatrixTable->setItem(count, AtenZMatrix::AngleAtomColumn, item);
				item = new QTableWidgetItem(zel->angleVariable()->name());
				ui.ZMatrixTable->setItem(count, AtenZMatrix::AngleColumn, item);

				// Fourth atom (torsion specifier)
				i = zel->atom(3);
				if (i != NULL)
				{
					item = new QTableWidgetItem(QString::number(i->id()+1));
					ui.ZMatrixTable->setItem(count, AtenZMatrix::TorsionAtomColumn, item);
					item = new QTableWidgetItem(zel->torsionVariable()->name());
					ui.ZMatrixTable->setItem(count, AtenZMatrix::TorsionColumn, item);
				}
			}
		}
		++count;
	}
	for (count=0; count<AtenZMatrix::nColumns; count++) ui.ZMatrixTable->resizeColumnToContents(count);
	// Variable list
	ui.VariablesTable->setRowCount(zMatrix_->nVariables());
	ui.VariablesTable->setColumnCount(2);
	ui.VariablesTable->setHorizontalHeaderLabels(QStringList() << "Var" << "Value");
	ReturnValue rv;
	Variable* var;
	count = 0;
	for (int n=0; n<zMatrix_->nDistances(); ++n)
	{
		Variable* var = zMatrix_->distance(n);
		item = new QTableWidgetItem(var->name());
		ui.VariablesTable->setItem(count, 0, item);
		var->execute(rv);
		item = new QTableWidgetItem(rv.asString());
		ui.VariablesTable->setItem(count, 1, item);
		++count;
	}
	for (int n=0; n<zMatrix_->nAngles(); ++n)
	{
		Variable* var = zMatrix_->angle(n);
		item = new QTableWidgetItem( var->name());
		ui.VariablesTable->setItem(count, 0, item);
		var->execute(rv);
		item = new QTableWidgetItem(rv.asString());
		ui.VariablesTable->setItem(count, 1, item);
		++count;
	}
	for (int n=0; n<zMatrix_->nTorsions(); ++n)
	{
		Variable* var = zMatrix_->torsion(n);
		item = new QTableWidgetItem( var->name());
		ui.VariablesTable->setItem(count, 0, item);
		var->execute(rv);
		item = new QTableWidgetItem(rv.asString());
		ui.VariablesTable->setItem(count, 1, item);
		++count;
	}
	for (count=0; count<2; count++) ui.VariablesTable->resizeColumnToContents(count);
	refreshing_ = false;
	Messenger::exit("AtenZMatrix::refresh");
}

void AtenZMatrix::on_ZMatrixTable_cellDoubleClicked(int row, int column)
{
	if (refreshing_) return;
	Variable* newvar, *oldvar;
	ZMatrixElement* el;
	bool changed = false;

	// Create a variable selection dialog ready...
	AtenSelectVariable variableSelect(parent_);

	// We are only interested in the event *if* a variable name was clicked, since we will not allow atom IDs to be edited (yet)
	switch (column)
	{
		case (AtenZMatrix::DistanceColumn):
			// Only valid for the second atom and above
			if (row < 1) break;
			// Select a new variable of distance type
			oldvar = zMatrix_->distance(row-1);
			el = zMatrix_->element(row-1);
			newvar = variableSelect.selectVariable(zMatrix_, 0, oldvar, el->negated(0));
			if (newvar != NULL)
			{
				// Set new variable if it is different
				if (newvar != oldvar)
				{
					el->setDistanceVariable(newvar);
					el->setNegated(0, variableSelect.isNegated());
				}
				changed = true;
			}
			break;
		case (AtenZMatrix::AngleColumn):
			// Only valid for the third atom and above
			if (row < 2) break;
			// Select a new variable of angle type
			oldvar = zMatrix_->angle(row-2);
			el = zMatrix_->element(row-2);
			newvar = variableSelect.selectVariable(zMatrix_, 1, oldvar, el->negated(1));
			if (newvar != NULL)
			{
				// Set new variable if it is different
				if (newvar != oldvar)
				{
					el->setAngleVariable(newvar);
					el->setNegated(1, variableSelect.isNegated());
				}
				changed = true;
			}
			break;
		case (AtenZMatrix::TorsionColumn):
			// Only valid for the fourth atom and above
			if (row < 3) break;
			// Select a new variable of angle type
			oldvar = zMatrix_->angle(row-3);
			el = zMatrix_->element(row-3);
			newvar = variableSelect.selectVariable(zMatrix_, 2, oldvar, el->negated(2));
			if (newvar != NULL)
			{
				// Set new variable if it is different
				if (newvar != oldvar)
				{
					el->setTorsionVariable(newvar);
					el->setNegated(2, variableSelect.isNegated());
				}
				changed = true;
			}
			break;
	}
	// Has anything changed?
	if (changed)
	{
		// New value has already been put into zmatrix structure, so update model and refresh window
		parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
		refresh();
	}
}

void AtenZMatrix::on_VariablesTable_itemChanged(QTableWidgetItem *w)
{
	if (refreshing_) return;
	int row = ui.VariablesTable->row(w);
	int column = ui.VariablesTable->column(w);
	Variable* var;

	// Grab existing variable
	if (row < zMatrix_->nDistances()) var = zMatrix_->distance(row);
	else if (row < (zMatrix_->nDistances()+zMatrix_->nAngles())) var = zMatrix_->angle(row-zMatrix_->nDistances());
	else var = zMatrix_->torsion(row-zMatrix_->nDistances()-zMatrix_->nAngles());

	// Change variable name?
	if (column == 0)
	{
		if (var != NULL) var->setName(qPrintable(w->text()));
		refresh();
	}
	// Change variable value?
	if (column == 1)
	{
		if (var != NULL) zMatrix_->setVariable(var, atof(qPrintable(w->text())));
		parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
		refresh();
	}
}

void AtenZMatrix::on_ShiftUpButton_clicked(bool checked)
{
	int row = ui.ZMatrixTable->currentRow();
	if (row == -1) return;
	zMatrix_->parent()->selectNone();
	zMatrix_->parent()->selectAtom(row);
	CommandNode::run(Commands::ShiftUp, "i", 1);
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void AtenZMatrix::on_ShiftDownButton_clicked(bool checked)
{
	int row = ui.ZMatrixTable->currentRow();
	if (row == -1) return;
	zMatrix_->parent()->selectNone();
	zMatrix_->parent()->selectAtom(row);
	CommandNode::run(Commands::ShiftDown, "i", 1);
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void AtenZMatrix::on_MoveToStartButton_clicked(bool checked)
{
	int row = ui.ZMatrixTable->currentRow();
	if (row == -1) return;
	zMatrix_->parent()->selectNone();
	zMatrix_->parent()->selectAtom(row);
	CommandNode::run(Commands::MoveToStart, "");
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void AtenZMatrix::on_MoveToEndButton_clicked(bool checked)
{
	int row = ui.ZMatrixTable->currentRow();
	if (row == -1) return;
	zMatrix_->parent()->selectNone();
	zMatrix_->parent()->selectAtom(row);
	CommandNode::run(Commands::MoveToEnd, "");
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void AtenZMatrix::dialogFinished(int result)
{
}
