/*
	*** Main Window - Messages Functions
	*** src/gui/mainwindow_messages.cpp
	Copyright T. Youngs 2007-2018

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

	updateWidgets();
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

	updateWidgets();
}

void AtenWindow::on_MessagesFontIncreaseButton_clicked(bool checked)
{
	QFont font = prefs.messagesFont();
	font.setPixelSize( std::min(font.pixelSize()+1, 50) );
	prefs.setMessagesFont(font);

	updateMessagesWidgets();

	updateWidgets();
}

void AtenWindow::on_MessagesFontDecreaseButton_clicked(bool checked)
{
	QFont font = prefs.messagesFont();
	font.setPixelSize( std::max(font.pixelSize()-1, 2) );
	prefs.setMessagesFont(font);

	updateMessagesWidgets();

	updateWidgets();
}

void AtenWindow::on_MessagesScroll_valueChanged(int value)
{
	updateWidgets();
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
	int maxDisplayLines = ui.MainView->contextHeight() / prefs.messagesFont().pixelSize();
	int currentPosition = ui.MessagesScroll->sliderPosition();
	ui.MessagesScroll->setMaximum( std::max(0, Messenger::nMessagesBuffered()-maxDisplayLines));
}

// Return current position of messages scrollbar
int AtenWindow::messagesScrollPosition()
{
	return ui.MessagesScroll->sliderPosition();
}

// Scroll messages by one step
void AtenWindow::scrollMessages(bool up)
{
	if (up) ui.MessagesScroll->setValue(ui.MessagesScroll->value()+ui.MessagesScroll->pageStep());
	else ui.MessagesScroll->setValue(ui.MessagesScroll->value()-ui.MessagesScroll->pageStep());
}