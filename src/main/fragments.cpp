/*
	*** Aten Fragment Functions
	*** src/main/fragment.cpp
	Copyright T. Youngs 2007-2016

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
#include <QDir>

ATEN_USING_NAMESPACE

// Add new fragment model from specified model's current selection
void Aten::addFragmentFromSelection(Model* source, QString parentGroup)
{
	Messenger::enter("Aten::addFragmentFromSelection");

	// Check source model and selection
	if (source == NULL)
	{
		printf("Internal Error : NULL model pointer passed to Aten::addFragmentFromSelection.\n");
		Messenger::exit("Aten::addFragmentFromSelection");
		return;
	}
	if (source->nSelected() == 0)
	{
		Messenger::error("Source model '%s' has no selected atoms from which to make a fragment.", qPrintable(source->name()));
		Messenger::exit("Aten::addFragmentFromSelection");
		return;
	}

	// Redirect model creation to fragment list
	targetModelList_ = Aten::FragmentLibraryList;

	// Create new fragment model and paste in source model selection
	Clipboard clip;
	clip.copySelection(source);
	Model* m = addModel();
	clip.pasteToModel(m, false);

	// Does the named fragment group already exist? If not, create new one
	FragmentGroup* fg = findFragmentGroup( parentGroup == NULL ? "New Fragments" : parentGroup );
	if (fg == NULL)
	{
		// Add default fragment group...
		fg = fragmentGroups_.add();
		fg->setName(parentGroup);
	}

	// Store the last model on the list.
	Fragment* f = fg->addFragment();
	if (!f->setMasterModel(m)) fg->removeFragment(f);

	// Return model creation to main list
	targetModelList_ = Aten::MainModelList;

	Messenger::exit("Aten::addFragmentFromSelection");
}

// Load fragment library
void Aten::loadFragments()
{
	Messenger::enter("Aten::loadFragments");
	int nFailed;

	// Redirect model creation to fragment list
	targetModelList_ = Aten::FragmentLibraryList;

	// Default search path should have already been set by openFilters()...
	QDir path = dataDirectoryFile("fragments");
	Messenger::print(Messenger::Verbose, "Looking for fragments in '%s'...", qPrintable(path.path()));
	nFailed = searchFragmentDir(path, "Ungrouped");

	// Try to load user fragments - we don't mind if the directory doesn't exist...
	path = atenDirectoryFile("fragments");
	Messenger::print(Messenger::Verbose, "Looking for user fragments in '%s'...", qPrintable(path.path()));
	nFailed = searchFragmentDir(path, "Ungrouped");

	// Return model creation to main list
	targetModelList_ = Aten::MainModelList;
	setCurrentModel(NULL);

	// Print out info
	int nFragments = 0;
	for (FragmentGroup* fg = fragmentGroups_.first(); fg != NULL; fg = fg->next) nFragments += fg->nFragments();
	Messenger::print("Loaded %i fragments into library.", nFragments);

	Messenger::exit("Aten::loadFragments");
}

// Parse fragment directory
bool Aten::searchFragmentDir(QDir path, QString groupName)
{
	Messenger::enter("Aten::searchFragmentDir");

	// First check - does this directory actually exist
	if (!path.exists())
	{
		Messenger::warn("Fragment directory '%s' does not exist.", qPrintable(path.path()));
		Messenger::exit("Aten::searchFragmentDir");
		return false;
	}

	int i;
	Tree* t;
	Fragment* f;
	FragmentGroup* fg;
	QPixmap pixmap;

	// Filter the directory contents - show only files and exclude '.' and '..'
	QStringList fragmentList = path.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<fragmentList.size(); i++)
	{
		// Construct full filepath
		QString filename = path.filePath(fragmentList.at(i));
		if (!importModel(filename, NULL)) continue;

		// Does the named fragment group already exist? If not, create new one
		fg = findFragmentGroup(groupName);
		if (fg == NULL)
		{
			// Add default fragment group...
			fg = fragmentGroups_.add();
			fg->setName(groupName);
		}

		// Store the last model on the list.
		f = fg->addFragment();
		if (!f->setMasterModel(fragments_.last())) fg->removeFragment(f);
	}

	// Check for other directories
	QStringList subDirList = path.entryList(QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i< subDirList.size(); ++i)
	{
		// Construct full filepath
		QDir subDir = path.filePath(subDirList.at(i));
		searchFragmentDir(subDir, subDirList.at(i));
	}

	Messenger::exit("Aten::searchFragmentDir");
	return true;
}

// Search for named fragment group
FragmentGroup* Aten::findFragmentGroup(QString name)
{
	for (FragmentGroup* fg = fragmentGroups_.first(); fg != NULL; fg = fg->next) if (name == fg->name()) return fg;
	return NULL;
}

// Return head of fragments list
FragmentGroup* Aten::fragmentGroups()
{
	return fragmentGroups_.first();
}

// Return number of fragments available
int Aten::nFragments()
{
	return fragments_.nItems();
}

// Set current fragment for drawing
void Aten::setCurrentFragment(Fragment* fragment)
{
	currentFragment_ = fragment;
}

// Return current fragment for drawing
Fragment* Aten::currentFragment()
{
	return currentFragment_;
}

// Increment bond id value
void Aten::increaseFragmentBondId()
{
	++fragmentBondId_;
}

// Return fragment bondId (as reference so it can be reset by associated Fragment routines)
int& Aten::fragmentBondId()
{
	return fragmentBondId_;
}

// Update all fragment icons
void Aten::updateFragmentIcons()
{
	for (FragmentGroup* fg = fragmentGroups_.first(); fg != NULL; fg = fg->next)
	{
		for (Fragment* fragment = fg->fragments(); fragment != NULL; fragment = fragment->next)
		{
			fragment->masterModel()->setIcon(atenWindow_->modelPixmap(fragment->masterModel(), QSize(64,64)));
		}
	}
}
