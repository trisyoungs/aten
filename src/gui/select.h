/*
	*** Select Dock Widget
	*** src/gui/select.h
	Copyright T. Youngs 2007-2011

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

#ifndef ATEN_SELECTWIDGET_H
#define ATEN_SELECTWIDGET_H

#include "gui/ui_select.h"

// Selection window
class SelectWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void refresh();
	void showWidget();
	private slots:
	void on_SelectAllButton_clicked(bool on);
	void on_SelectNoneButton_clicked(bool on);
	void on_SelectionExpandButton_clicked(bool on);
	void on_SelectionInvertButton_clicked(bool on);
	void on_SelectButton_clicked(bool on);
	void on_DeselectButton_clicked(bool on);
	void on_TypeSelectElementButton_clicked(bool on);
	void on_SelectTypeButton_clicked(bool on);
	void on_DeselectTypeButton_clicked(bool on);
	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Local variables
	*/
	private:

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	SelectWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::SelectWidget ui;
};

#endif
