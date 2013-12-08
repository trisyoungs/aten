/*
	*** TPrompt Functions
	*** src/gui/tprompt_funcs.cpp
	Copyright T. Youngs 2007-2013

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

#include "base/dnchar.h"
#include "gui/tprompt.uih"
#include <QtGui/QPainter>
#include <QtGui/QLayout>
#include <QtGui/QLineEdit>
#include <QtGui/QListWidget>
#include <QtGui/QKeyEvent>

// Constructor
TPrompt::TPrompt( QWidget *parent ) : QWidget( parent )
{
        QVBoxLayout *layout = new QVBoxLayout( this );
        layout->setMargin(2);
	layout->setSpacing(2);

	// Create layout
        commandList_ = new QListWidget(this);
        commandPrompt_ = new QLineEdit(this);
        layout->addWidget( commandList_ );
        layout->addWidget( commandPrompt_ );
	this->setFocusProxy(commandPrompt_);

	// Connect necessary (local) signals
	QObject::connect(commandPrompt_, SIGNAL(returnPressed()), this, SLOT(promptReturnPressed()));
	QObject::connect(commandPrompt_, SIGNAL(textEdited(QString)), this, SLOT(promptTextEdited(QString)));
	QObject::connect(commandList_, SIGNAL(itemClicked(QListWidgetItem*)), this, SLOT(promptListSingleClicked(QListWidgetItem*)));
	QObject::connect(commandList_, SIGNAL(itemDoubleClicked(QListWidgetItem*)), this, SLOT(promptListDoubleClicked(QListWidgetItem*)));
}

// Return in QLineEdit has been pressed
void TPrompt::promptReturnPressed()
{
	emit returnPressed();
}

// Set list of commands in command tab
void TPrompt::setCommandList(QStringList cmds)
{
	commandList_->clear();
	commandList_->insertItems(0, cmds);
}

// Return list of commands stored in command tab
QStringList TPrompt::commandList()
{
	QStringList items;
	for (int n=0; n<commandList_->count(); ++n) items << commandList_->item(n)->text();
	return items;
}

const char *TPrompt::getText()
{
	// Grab string and add trailing semicolon if required
	static Dnchar text;
	text = qPrintable(commandPrompt_->text());
	commandPrompt_->setText("");
	// If there's nothing 'useful' in the LineEdit, don't update the list...
	if (!text.isEmpty())
	{
		// Add the text we just saved to the list, unless it is a duplicate of an existing entry
		QList<QListWidgetItem*> results = commandList_->findItems(text.get(), Qt::MatchFixedString);
		if (results.size() == 0) commandList_->addItem(text.get());
		commandList_->setCurrentRow(-1);
	}
	return text.get();
}

void TPrompt::keyPressEvent(QKeyEvent *event)
{
	int index;
	// In the case of moving the list selection, if no list item is currently selected store what we currently have in the LineEdit so we may return to it if necessary...
	switch (event->key())
	{
		case (Qt::Key_Up):
			if (commandList_->count() == 0) break;
			index = commandList_->currentRow();
			if (index == -1)
			{
				// Store current text
				lastPromptText_ = qPrintable(commandPrompt_->text());
				index = commandList_->count()-1;
			}
			else index--;
			if (index == -1)
			{
				commandList_->setCurrentRow(-1);
				commandPrompt_->setText( lastPromptText_.get() );
			}
			else
			{
				commandList_->setCurrentRow(index);
				commandPrompt_->setText( commandList_->item(index)->text() );
			}
			break;
		case (Qt::Key_Down):
			if (commandList_->count() == 0) break;
			index = commandList_->currentRow();
			if (index == -1)
			{
				// Store current text
				lastPromptText_ = qPrintable(commandPrompt_->text());
				index = 0;
			}
			else index++;
			if (index == commandList_->count())
			{
				commandList_->setCurrentRow(-1);
				commandPrompt_->setText( lastPromptText_.get() );
			}
			else
			{
				commandList_->setCurrentRow(index);
				commandPrompt_->setText( commandList_->item(index)->text() );
			}
			break;
		default:
			event->ignore();
	}
}

void TPrompt::promptTextEdited(QString text)
{
	// Add the text we just saved to the list, unless it is a duplicate of an existing entry
	QList<QListWidgetItem*> results = commandList_->findItems(text, Qt::MatchStartsWith);
	if (results.size() > 0) commandList_->setCurrentItem(results.first());
}

void TPrompt::promptListSingleClicked(QListWidgetItem *item)
{
	if (item == NULL) return;
	// Store current line edit text
	lastPromptText_ = qPrintable(commandPrompt_->text());
	// Take the text of the item and place it in the line edit
	commandPrompt_->setText( item->text() );
	commandPrompt_->setFocus();
}

void TPrompt::promptListDoubleClicked(QListWidgetItem *item)
{
	if (item == NULL) return;
	// Store current line edit text
	lastPromptText_ = qPrintable(commandPrompt_->text());
	// Take the text of the item and place it in the line edit
	commandPrompt_->setText( item->text() );
	emit returnPressed();
}

