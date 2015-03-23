/*
	*** Aten forcefield functions
	*** src/main/aten.cpp
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
#include "ff/forcefield.h"
#include "base/pattern.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Add Forcefield
Forcefield* Aten::addForcefield(QString name)
{
	current_.ff = forcefields_.add();
	if (name != NULL) current_.ff->setName(name);
	return current_.ff;
}

// Load forcefield
Forcefield* Aten::loadForcefield(QString ffFile)
{
	Messenger::enter("Aten::loadForcefield");

	// Try some different locations to find the supplied forcefield.
	QString filename;
	QFileInfo fileInfo;
	bool result;
	Forcefield* newff = forcefields_.add();

	// First try - actual / absolute path
	fileInfo.setFile(ffFile);
	Messenger::print(Messenger::Verbose, "Looking for forcefield in absolute path (%s)...", qPrintable(ffFile));
	if (fileInfo.exists()) result = newff->load(ffFile);
	else
	{
		// Second try - aten.dataDir/ff
		filename = dataDirectoryFile("ff/"+ffFile);
		fileInfo.setFile(filename);
		Messenger::print(Messenger::Verbose, "Looking for forcefield in installed location (%s)...", qPrintable(filename));
		if (fileInfo.exists()) result = newff->load(filename);
		else
		{
			// Last try - user home datadir/ff
			filename = atenDirectoryFile("ff/"+ffFile);
			fileInfo.setFile(filename);
			Messenger::print(Messenger::Verbose, "Looking for forcefield in user's data directory (%s)...", qPrintable(filename));
			if (fileInfo.exists()) result = newff->load(filename);
			else
			{
				Messenger::print("Can't find forcefield file '%s' in any location.", qPrintable(ffFile));
				result = FALSE;
			}
		}
	}
	if (result)
	{
		current_.ff = newff;
		Messenger::print("Forcefield '%s' is now the default.", qPrintable(newff->name()));
	}
	else
	{
		Messenger::print("Couldn't load forcefield file '%s'.", qPrintable(ffFile));
		forcefields_.remove(newff);
		newff = NULL;
	}
	Messenger::exit("Aten::loadForcefield");
	return newff;
}

// Unload forcefield from the master's list
void Aten::removeForcefield(Forcefield* xff)
{
	Messenger::enter("Aten::removeForcefield");
	Forcefield* newff;
	// If possible, set the active row to the next model. Otherwise, the previous.
	xff->next != NULL ? newff = xff->next : newff = xff->prev;
	current_.ff = newff;
	dereferenceForcefield(xff);
	// Finally, delete the ff
	forcefields_.remove(xff);
	// Set a new default if necessary
	if (current_.ff == xff) current_.ff = forcefields_.first();
	Messenger::exit("Aten::removeForcefield");
}

// Find forcefield by name
Forcefield* Aten::findForcefield(QString name) const
{
	Messenger::enter("Aten::findForcefield");
	Forcefield* ff;
	for (ff = forcefields_.first(); ff != NULL; ff = ff->next) if (name == ff->name()) break;
	if (ff == NULL) Messenger::print("Forcefield '%s' is not loaded.", qPrintable(name));
	Messenger::exit("Aten::findForcefield");
	return ff;
}

// Return the first ff in the list
Forcefield* Aten::forcefields() const
{
	return forcefields_.first();
}

// Return the nth ff in the list
Forcefield* Aten::forcefield(int n)
{
	return forcefields_[n];
}

// Return the number of loaded forcefields
int Aten::nForcefields() const
{
	return forcefields_.nItems();
}

// Set active forcefield
void Aten::setCurrentForcefield(Forcefield* ff)
{
	current_.ff = ff;
	if (current_.ff == NULL) Messenger::print("Default forcefield has been unset.");
	else Messenger::print("Default forcefield is now '%s'.", qPrintable(current_.ff->name()));
}

// Set active forcefield by ID
void Aten::setCurrentForcefield(int id)
{
	setCurrentForcefield(forcefields_[id]);
}

// Return the active forcefield
Forcefield* Aten::currentForcefield() const
{
	return current_.ff;
}

// Return ID of current forcefield
int Aten::currentForcefieldId() const
{
	return forcefields_.indexOf(current_.ff);
}

// Dereference forcefield
void Aten::dereferenceForcefield(Forcefield* xff)
{
	// Remove references to the forcefield in the models
	Messenger::enter("Aten::dereferenceForcefield");
	for (Model* m = models_.first(); m != NULL; m = m->next)
	{
		if (m->forcefield() == xff)
		{
			m->removeTyping();
			m->setForcefield(NULL);
			m->invalidateExpression();
		}
		if (m->patterns() != NULL)
		{
			for (Pattern* p = m->patterns(); p != NULL; p = p->next)
			{
				if (p->forcefield() == xff)
				{
					Atom* i = p->firstAtom();
					for (int n=0; n<p->totalAtoms(); n++)
					{
						i->setType(NULL);
						i = i->next;
					}
					p->setForcefield(NULL);
					m->invalidateExpression();
				}
			}
		}
		else
		{
			int count = 0;
			for (Atom* i = m->atoms(); i != NULL; i = i->next) if (xff->containsType(i->type()))
			{
				++count;
				i->setType(NULL);
			}
			if (count != 0) m->invalidateExpression();
		}
	}
	Messenger::exit("Aten::dereferenceForcefield");
}

// Return combination rules
CombinationRules& Aten::combinationRules()
{
	return combinationRules_;
}