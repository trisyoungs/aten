/*
	*** Qt TTreeWidgetItem functions
	*** src/gui-qt/ttreewidgetitem_funcs.cpp
	Copyright T. Youngs 2007

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

#include "base/elements.h"
#include "classes/forcefield.h"
#include "gui-qt/ttreewidgetitem.h"

TTreeWidgetItem::TTreeWidgetItem(QTreeWidgetItem *parent) : QTreeWidgetItem(parent)
{
	i = NULL;
	ffa = NULL;
}

void TTreeWidgetItem::set_atom_columns()
{
	static vec3<double> r;
	if (i == NULL) printf("TTreeWidgetItem::set_atom_columns <<<< Atom has not yet been set >>>>\n");
	else
	{
		setText(TW_A_ID, itoa(i->get_id()+1));
		setText(TW_A_ELEMENT, elements.symbol(i));
		r = i->r();
		setText(TW_A_RX, ftoa(r.x));
		setText(TW_A_RY, ftoa(r.y));
		setText(TW_A_RZ, ftoa(r.z));
	}
}

void TTreeWidgetItem::set_ffatom_columns()
{
	if (ffa == NULL) printf("TTreeWidgetItem::set_ffatom_columns <<<< FFAtom has not yet been set >>>>\n");
	else
	{
		setText(TW_FFA_NAME, ffa->get_name());
		setText(TW_FFA_DESCRIPTION, ffa->get_description());
	}
}
