/*
	*** Qt GUI: ZMatrix window functions
	*** src/gui/zmatrix_funcs.cpp
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

#include "main/aten.h"
#include "gui/zmatrix.h"
#include "gui/gui.h"
#include "base/messenger.h"

// Constructor
AtenZMatrix::AtenZMatrix(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
}

// Destructor
AtenZMatrix::~AtenZMatrix()
{
}

void AtenZMatrix::showWindow()
{
	refresh();
	show();
}

// Refresh the zmatrix
void AtenZMatrix::refresh(bool forceupdate)
{
	msg.enter("AtenZMatrix::refresh");
	Model *m = aten.currentModelOrFrame();
	// Grab (and create) zmatrix for current model
	ZMatrix *zmat = m->zMatrix();
printf("HJASJDLKASJDLK\n");
	// ZMatrix 'Connectivity' Table
	int count = 0;
	QTableWidgetItem *item;
	Atom *i;
	ui.ZMatrixTable->setRowCount(zmat->nElements());
// 	ui.FFEditorBondsTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6" << "Data 7" << "Data 8" << "Data 9" << "Data 10");
// 	ui.ZMatrixTable->setVerticalHeaderLabels("1");
	for (ZMatrixElement *zel = zmat->elements(); zel != NULL; zel = zel->next)
	{
		// First atom (the creation target)
		i = zel->atom(0);
		item = new QTableWidgetItem(elements().symbol(i));
		ui.ZMatrixTable->setItem(count, AtenZMatrix::SymbolColumn, item);
		// Second atom (distance specifier)
		i = zel->atom(1);
		if (i != NULL)
		{
			item = new QTableWidgetItem(i->id()+1);
			ui.ZMatrixTable->setItem(count, AtenZMatrix::DistanceAtomColumn, item);
			item = new QTableWidgetItem(zel->distance()->name());
			ui.ZMatrixTable->setItem(count, AtenZMatrix::DistanceColumn, item);
			
			// Third atom (angle specifier)
			i = zel->atom(2);
			if (i != NULL)
			{
				item = new QTableWidgetItem(i->id()+1);
				ui.ZMatrixTable->setItem(count, AtenZMatrix::AngleAtomColumn, item);
				item = new QTableWidgetItem(zel->angle()->name());
				ui.ZMatrixTable->setItem(count, AtenZMatrix::AngleColumn, item);

				// Fourth atom (torsion specifier)
				i = zel->atom(3);
				if (i != NULL)
				{
					item = new QTableWidgetItem(i->id()+1);
					ui.ZMatrixTable->setItem(count, AtenZMatrix::TorsionAtomColumn, item);
					item = new QTableWidgetItem(zel->torsion()->name());
					ui.ZMatrixTable->setItem(count, AtenZMatrix::TorsionColumn, item);
				}
			}
		}
		count ++;
	}
	for (count=0; count<AtenZMatrix::nColumns; count++) ui.ZMatrixTable->resizeColumnToContents(count);

	msg.exit("AtenZMatrix::refresh");
}

void AtenZMatrix::on_ZMatrixTable_cellDoubleClicked(int row, int column)
{
	printf("Row, Column = %i, %i\n", row, column);
}