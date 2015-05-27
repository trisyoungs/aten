/*
	*** Select Dock Widget
	*** src/gui/select.h
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

#ifndef ATEN_SELECTWIDGET_H
#define ATEN_SELECTWIDGET_H

#include "gui/ui_select.h"

// Forward Declarations (Qt)
class AtenWindow;

// Selection window
class SelectWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	SelectWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::SelectWidget ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Local variables
	 */
	private:
	// Whether window is refreshing
	bool refreshing_;


	/*
	 * Window Functions
	 */
	public:
	void refresh();
	void showWidget();
	void setHistories(QStringList select, QStringList forlist, QStringList netalist);
	private slots:
	void on_SelectAllButton_clicked(bool checked);
	void on_SelectNoneButton_clicked(bool checked);
	void on_SelectionExpandButton_clicked(bool checked);
	void on_SelectionInvertButton_clicked(bool checked);
	void on_SelectCombo_currentIndexChanged(int n);
	void on_SelectButton_clicked(bool checked);
	void on_DeselectButton_clicked(bool checked);
	void on_TypeSelectElementButton_clicked(bool checked);
	void on_SelectTypeButton_clicked(bool checked);
	void on_DeselectTypeButton_clicked(bool checked);
	void on_SelectForButton_clicked(bool checked);
	void on_DeselectForButton_clicked(bool checked);
	protected:
	void closeEvent(QCloseEvent* event);
};

#endif
