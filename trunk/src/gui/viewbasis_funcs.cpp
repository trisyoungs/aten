/*
	*** Qt GUI: View Basis dialog functions
	*** src/gui/viewbasis_funcs.cpp
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
#include "gui/viewbasis.h"
#include "gui/mainwindow.h"
#include "base/messenger.h"
#include "base/sysfunc.h"
#include "model/model.h"

// Constructor
AtenViewBasis::AtenViewBasis(QWidget* parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	// Private variables
	target_ = NULL;

	ui.setupUi(this);
}

void AtenViewBasis::showWindow(Model* m)
{
	// Clear old contents
	ui.BasisTable->clear();
	ui.BasisTable->setColumnCount(10);
	ui.BasisTable->setHorizontalHeaderLabels(QStringList() << "Atom" << "Shell" << "Type" << "Exponent" << "Coefficients" << " " << " " << " " << " " << " " );
	// Check model pointer
	if (m == NULL) return;
	// If no basis definition exists in the supplied model, try to look at the trajectory parent
	target_ = m;
	if ((target_->basisShells() == NULL) && (target_->parent() != NULL))
	{
		target_ = target_->parent();
		Messenger::print("No basis definition found in trajectory frame - looking in parent model...");
	}
	// Determine total row count
	BasisShell *bas;
	BasisPrimitive* prim;
	int row = 0, shell, lastid = -1, n, ncartesians = 0;
	Dnchar text;
	for (bas = target_->basisShells(); bas != NULL; bas = bas->next) row += bas->nPrimitives();
	ui.BasisTable->setRowCount(row);
	ui.BasisShellsLabel->setText(QString::number(target_->nBasisShells()));
	// Populate table
	QTableWidgetItem *tabitem;
	row = 0;
	shell = 0;
	for (bas = target_->basisShells(); bas != NULL; bas = bas->next)
	{
		++shell;
		ncartesians += BasisShell::nCartesianFunctions(bas->type());
		// Create atom id item?
		if (lastid != bas->atomId())
		{
			lastid = bas->atomId();
			tabitem = new QTableWidgetItem();
			text.sprintf("%i (%s)\n", lastid+1, m->atom(lastid) != NULL ? Elements().symbol(m->atom(lastid)) : "NULL");
			tabitem->setText(text.get());
			ui.BasisTable->setItem(row, AtenViewBasis::AtomIdColumn, tabitem);
		}
		// Add in shell data
		tabitem = new QTableWidgetItem();
		tabitem->setText(QString::number(shell));
		ui.BasisTable->setItem(row, AtenViewBasis::ShellColumn, tabitem);
		tabitem = new QTableWidgetItem();
		tabitem->setText(BasisShell::basisShellType(bas->type()));
		ui.BasisTable->setItem(row, AtenViewBasis::TypeColumn, tabitem);

		// Cycle over primitives
		n = 0;
		for (prim = bas->primitives(); prim != NULL; prim = prim->next)
		{
			tabitem = new QTableWidgetItem();
			tabitem->setText(ftoa(prim->exponent()));
			ui.BasisTable->setItem(row, AtenViewBasis::ExponentColumn, tabitem);
			for (n = 0; n < prim->nCoefficients(); ++n)
			{
				tabitem = new QTableWidgetItem();
				tabitem->setText(ftoa(prim->coefficient(n)));
				ui.BasisTable->setItem(row, AtenViewBasis::CoefficientColumn+n, tabitem);
			}
			row++;
		}
	}
	ui.BasisCartesiansLabel->setText(QString::number(ncartesians));
	// Resize columns
	for (n=0; n<AtenViewBasis::nColumns; ++n) ui.BasisTable->resizeColumnToContents(n);
	show();
}

void AtenViewBasis::dialogFinished(int result)
{
	accept();
}

