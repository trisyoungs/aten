/*
	*** Model label functions
	*** src/model/labels.cpp
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

#include "model/model.h"
#include "undo/undostate.h"
#include "undo/atom_label.h"

ATEN_USING_NAMESPACE

// Add label to atom
void Model::addLabel(Atom* i, Atom::AtomLabel al)
{
	int oldlabels = i->labels();
	i->addLabel(al);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		AtomLabelEvent* newChange = new AtomLabelEvent;
		newChange->set(i->id(), oldlabels, i->labels());
		recordingState_->addEvent(newChange);
	}
	logChange(Log::Labels);
}

// Remove atom label
void Model::removeLabel(Atom* i, Atom::AtomLabel al)
{
	int oldlabels = i->labels();
	i->removeLabel(al);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		AtomLabelEvent* newChange = new AtomLabelEvent;
		newChange->set(i->id(), oldlabels, i->labels());
		recordingState_->addEvent(newChange);
	}
	logChange(Log::Labels);
}

// Clear labelling from atom
void Model::clearLabels(Atom* i)
{
	int oldlabels = i->labels();
	i->clearLabels();
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		AtomLabelEvent* newChange = new AtomLabelEvent;
		newChange->set(i->id(), oldlabels, 0);
		recordingState_->addEvent(newChange);
	}
	logChange(Log::Labels);
}

// Clear atom labelling
void Model::clearAllLabels()
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) clearLabels(i);
	logChange(Log::Labels);
}

// Clear all labels in selection
void Model::selectionClearLabels()
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) clearLabels(i);
	logChange(Log::Labels);
}

// Remove specific labels in selection
void Model::selectionRemoveLabels(Atom::AtomLabel al)
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) removeLabel(i, al);
	logChange(Log::Labels);
}

// Add atom labels
void Model::selectionAddLabels(Atom::AtomLabel al)
{
	for (Atom* i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) addLabel(i, al);
	logChange(Log::Labels);
}
