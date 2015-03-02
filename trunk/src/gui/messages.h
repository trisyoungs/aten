/*
	*** Messages Dock Widget
	*** src/gui/messages.h
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

#ifndef ATEN_MESSAGESWIDGET_H
#define ATEN_MESSAGESWIDGET_H

#include "gui/ui_messages.h"

// Forward Declarations (Qt)
class AtenWindow;

// Messages window
class MessagesWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	MessagesWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::MessagesWidget ui;
	// Set controls to reflect program variables
	void setControls();

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	// Window Functions
	*/
	public:
	void showWidget();
	void refresh();

	protected:
	void closeEvent(QCloseEvent *event);

	private slots:
	void on_MessagesBrowser_anchorClicked(const QUrl &link);
};

#endif
