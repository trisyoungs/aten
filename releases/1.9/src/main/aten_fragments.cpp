/*
	*** Aten Fragment Functions
	*** src/main/aten_fragment.cpp
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

// Add new fragment model from specified model's current selection
void Aten::addFragmentFromSelection(Model *source, const char *parentgroup)
{
	msg.enter("Aten::addFragmentFromSelection");

	// Check source model and selection
	if (source == NULL)
	{
		printf("Internal Error : NULL model pointer passed to Aten::addFragmentFromSelection.\n");
		msg.exit("Aten::addFragmentFromSelection");
		return;
	}
	if (source->nSelected() == 0)
	{
		msg.print("Source model '%s' has no selected atoms from which to make a fragment.\n", source->name());
		msg.exit("Aten::addFragmentFromSelection");
		return;
	}

	// Redirect model creation to fragment list
	targetModelList_ = Aten::FragmentLibraryList;

	// Create new fragment model and paste in source model selection
	Clipboard clip;
	clip.copySelection(source);
	Model *m = addModel();
	clip.pasteToModel(m, FALSE);

	// Does the named fragment group already exist? If not, create new one
	FragmentGroup *fg = findFragmentGroup( parentgroup == NULL ? "New Fragments" : parentgroup );
	if (fg == NULL)
	{
		// Add default fragment group...
		fg = fragmentGroups_.add();
		fg->setName(parentgroup);
	}

	// Store the last model on the list.
	Fragment *f = fg->addFragment();
	if (!f->setMasterModel(m)) fg->removeFragment(f);
	else m->regenerateIcon();

	// Return model creation to main list
	targetModelList_ = Aten::MainModelList;

	msg.exit("Aten::addFragmentFromSelection");
}

// Parse fragment directory
bool Aten::parseFragmentDir(const char *path, const char *groupname)
{
	msg.enter("Aten::parseFragmentDir");
	int i;
	Tree *t;
	Fragment *f;
	FragmentGroup *fg;
	QPixmap pixmap;

	// First check - does this directory actually exist
	QDir fragmentdir(path);
	if (!fragmentdir.exists())
	{
		msg.exit("Aten::parseFragmentDir");
		return FALSE;
	}

	// Filter the directory contents - show only files and exclude '.' and '..'
	QStringList fragmentlist = fragmentdir.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<fragmentlist.size(); i++)
	{
		// Construct full filepath
		QString filename(path);
		filename += "/";
		filename += fragmentlist.at(i);
		t = aten.probeFile(qPrintable(filename), FilterData::ModelImport);
		if (t == NULL) continue;
		if (!t->executeRead(qPrintable(filename))) continue;

		// Does the named fragment group already exist? If not, create new one
		fg = findFragmentGroup(groupname);
		if (fg == NULL)
		{
			// Add default fragment group...
			fg = fragmentGroups_.add();
			fg->setName(groupname);
		}

		// Store the last model on the list.
		f = fg->addFragment();
		if (!f->setMasterModel(fragmentModels_.last())) fg->removeFragment(f);
	}

	// Check for other directories
	fragmentlist = fragmentdir.entryList(QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<fragmentlist.size(); i++)
	{
		// Construct full filepath
		QString filename(path);
		filename += "/";
		filename += fragmentlist.at(i);
		parseFragmentDir(qPrintable(filename), qPrintable(fragmentlist.at(i)));
	}

	msg.exit("Aten::parseFragmentDir");
	return TRUE;
}

// Load fragment library
void Aten::openFragments()
{
	msg.enter("Aten::openFragments");
	Dnchar path;
	int nfailed;

	// Redirect model creation to fragment list
	targetModelList_ = Aten::FragmentLibraryList;

	// Default search path should have already been set by openFilters()...
	path.sprintf("%s%cfragments", dataDir_.get(), PATHSEP);
	msg.print(Messenger::Verbose, "Looking for fragments in '%s'...\n", qPrintable(QDir::toNativeSeparators(path.get())));
	nfailed = parseFragmentDir(path, "Ungrouped");

	// Try to load user fragments - we don't mind if the directory doesn't exist...
	path.sprintf("%s%c%s%cfragments%c", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP, PATHSEP);
	msg.print(Messenger::Verbose, "Looking for user fragments in '%s'...\n", path.get());
	nfailed = parseFragmentDir(path, "Ungrouped");

	// Return model creation to main list
	targetModelList_ = Aten::MainModelList;
	aten.setCurrentModel(NULL);

	// Print out info
	int nfragments = 0;
	for (FragmentGroup *fg = fragmentGroups_.first(); fg != NULL; fg = fg->next) nfragments += fg->nFragments();
	msg.print("Loaded %i fragments into library.\n", nfragments);

	msg.exit("Aten::openFragments");
}

// Search for name fragment group
FragmentGroup *Aten::findFragmentGroup(const char *name)
{
	for (FragmentGroup *fg = fragmentGroups_.first(); fg != NULL; fg = fg->next) if (strcmp(name,fg->name()) == 0) return fg;
	return NULL;
}

// Return head of fragments list
FragmentGroup *Aten::fragmentGroups()
{
	return fragmentGroups_.first();
}

