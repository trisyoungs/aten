/*
	*** Qt GUI: Typing Window
	*** src/gui/typing.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_TYPINGWINDOW_H
#define ATEN_TYPINGWINDOW_H

#include "gui/ui_typing.h"

// Program preferences window
class AtenTyping : public QWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	private slots:
	void on_TypeModelButton_clicked(bool checked);
	void on_UntypeModelButton_clicked(bool checked);
	void on_ManualTypeSetButton_clicked(bool checked);
	void on_ManualTypeClearButton_clicked(bool checked);
	void on_ManualTypeTestButton_clicked(bool checked);
	void on_ManualTypeEdit_returnPressed();

	/*
	// Local variables
	*/
	private:
	// Element selected in Type filter
	int typelistElement_;

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenTyping(QWidget *parent = 0);
	// Destructor
	~AtenTyping();
	// Main form declaration
	Ui::TypingWidget ui;
	// Finalise widgets (things that couldn't be done in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
