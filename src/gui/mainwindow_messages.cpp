/*
	*** MainWindow - Messages Functions
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

// #include <QtWidgets/QMessageBox>
// #include "main/aten.h"
#include "gui/mainwindow.h"
// #include "gui/prefs.h"
// #include "gui/loadmodel.h"
// #include "gui/trajectory.h"
// #include "gui/ffeditor.h"
// #include "gui/selectpattern.h"
// #include "gui/about.h"
// #include "model/model.h"
// #include "model/clipboard.h"
// #include "model/undostate.h"
// #include "parser/commandnode.h"
// #include <QtWidgets/QFileDialog>
// #include <QKeyEvent>
// #include <QtWidgets/QProgressBar>
// #include "base/sysfunc.h"
// #include "main/version.h"
// #include <iostream>
// #include <fstream>

void AtenWindow::on_MessagesScroll_sliderMoved(int position)
{
	postRedisplay();
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
