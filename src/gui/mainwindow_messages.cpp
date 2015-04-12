/*
	*** Main Window - Messages Functions
	*** src/gui/mainwindow_messages.cpp
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

#include "gui/mainwindow.h"
#include <QClipboard>
#include <QTextStream>

void AtenWindow::on_MessagesCycleButton_clicked(bool checked)
{
	if (messageDisplay_ == AtenWindow::FullMessages) messageDisplay_ = AtenWindow::MessagesOverScene;
	else if (messageDisplay_ == AtenWindow::MessagesOverScene) messageDisplay_ = AtenWindow::MessagesUnderScene;
	else if (messageDisplay_ == AtenWindow::MessagesUnderScene) messageDisplay_ = AtenWindow::NoMessages;
	else messageDisplay_ = AtenWindow::FullMessages;

	postRedisplay();
}

void AtenWindow::on_MessagesCopyButton_clicked(bool checked)
{
	// Construct new text
	QString text;
	QList<Message>& messages = Messenger::messageBuffer();
	QTextStream stream(&text);
	for (int n=messages.count()-1; n>=0; --n) stream << messages.at(n).text() << endl;

	QClipboard* clipboard = QApplication::clipboard();
	clipboard->setText(text);
}

void AtenWindow::on_MessagesClearButton_clicked(bool checked)
{
	Messenger::clearMessageBuffer();

	postRedisplay();
}

void AtenWindow::on_MessagesScroll_sliderMoved(int position)
{
	postRedisplay();
}

// Return current message display style
AtenWindow::MessageDisplay AtenWindow::messageDisplay()
{
	return messageDisplay_;
}

// Update messages widgets
void AtenWindow::updateMessagesWidgets()
{
	// Calculate current display height
	int maxDisplayLines = ui.MainView->contextHeight() / ui.MainView->fontPixelHeight();
	int currentPosition = ui.MessagesScroll->sliderPosition();
	ui.MessagesScroll->setMaximum( std::max(0, Messenger::nMessagesBuffered()-maxDisplayLines));
}

// Return current position of messages scrollbar
int AtenWindow::messagesScrollPosition()
{
	return ui.MessagesScroll->sliderPosition();
}
