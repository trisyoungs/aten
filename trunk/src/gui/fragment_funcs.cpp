/*
	*** Qt GUI: Fragment library window functions
	*** src/gui/fragment_funcs.cpp
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

#include "base/sysfunc.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/fragment.h"
#include "gui/ttreewidgetitem.h"
#include "gui/tcanvas.uih"
#include "model/model.h"
#include "model/fragment.h"
#include "main/aten.h"

// Constructor
AtenFragment::AtenFragment(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
	
	// Private variables
	currentFragment_ = NULL;
}

// Destructor
AtenFragment::~AtenFragment()
{
}

void AtenFragment::showWindow()
{
	show();
}

// Return current drawing fragment
Fragment *AtenFragment::currentFragment()
{
	return currentFragment_;
}

// Refresh the atom list
void AtenFragment::refresh()
{
	msg.enter("AtenFragment::refresh");

	TTreeWidgetItem *item, *group;
	ui.FragmentTree->clear();

	// Go through all available fragment groups
	for (FragmentGroup *fg = aten.fragmentGroups(); fg != NULL; fg = fg->next)
	{
		// Are there any fragments in this group?
		if (fg->nFragments() == 0) continue;

		// Create main tree branch
		group = new TTreeWidgetItem(ui.FragmentTree);
		ui.FragmentTree->setItemExpanded(group, TRUE);
		group->setText(0, fg->name());
		group->setFirstColumnSpanned(TRUE);

		// Go through fragments in group and populate branch
		for (Fragment *f = fg->fragments(); f != NULL; f = f->next)
		{
			item = new TTreeWidgetItem(group);
			item->setFragment(f);
			item->setIcon(1,f->icon());
			item->setText(2,itoa(f->model()->nAtoms()));
			item->setText(3,f->model()->name());

			// If the currentFragment_ is NULL, set it as soon as possible
			if (currentFragment_ == NULL) currentFragment_ = f;
			if (currentFragment_ == f) item->setSelected(TRUE);
		}
	}
	// Resize columns
	for (int n=0; n<3; n++) ui.FragmentTree->resizeColumnToContents(n);
	msg.exit("AtenFragment::refresh");
}

void AtenFragment::dialogFinished(int result)
{
	gui.mainWindow->ui.actionFragmentWindow->setChecked(FALSE);
}

void AtenFragment::on_FragmentTree_currentItemChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous)
{
	if (current == NULL) currentFragment_ = NULL;
	else
	{
		// Cast into TTreeWidgetItem
		TTreeWidgetItem *twi = (TTreeWidgetItem*) current;
		if (twi == NULL) currentFragment_ = NULL;
		else
		{
			// If this is a header item in the Tree, the fragment pointer will be NULL
			currentFragment_ = twi->fragment();
		}
	}
}

void AtenFragment::on_FragmentFilterEdit_textChanged(QString &text)
{
	
}

void AtenFragment::on_FragmentShowAllButton_clicked(bool checked)
{
}
