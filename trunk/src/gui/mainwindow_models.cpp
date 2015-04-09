/*
	*** MainWindow - Model Functions
	*** src/gui/mainwindow_model.cpp
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
#include "gui/mainwindow.h"
#include "templates/variantpointer.h"

// Refresh model list
void AtenWindow::refreshModelList()
{
	modelListRefreshing_= true;


	// Clear the current list
	ui.ModelTree->clear();
	ui.ModelTree->setColumnCount(1);
	
	QTreeWidgetItem* item;
	for (Model* m = aten().models(); m != NULL; m = m->next)
	{
		item = new QTreeWidgetItem(ui.ModelTree);
		item->setData(0, Qt::UserRole, VariantPointer<Model>(m));
		item->setIcon(0, m->icon());
// 		item->setToolTop(1,m->name());
		if (m->isVisible()) item->setSelected(true);
	}

	ui.ModelTree->resizeColumnToContents(0);
	
	modelListRefreshing_ = false;
}

void AtenWindow::on_ModelTree_itemSelectionChanged()
{
	if (modelListRefreshing_) return;
}
