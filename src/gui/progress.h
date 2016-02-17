/*
	*** Progress Dialog
	*** src/gui/progress.h
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

#ifndef ATEN_PROGRESSDIALOG_H
#define ATEN_PROGRESSDIALOG_H

#include "gui/ui_progress.h"
#include <QTime>
#include "base/namespace.h"

// Progress Dialog
class AtenProgress : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	AtenProgress(QWidget* parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::AtenProgress ui;


	/*
	 * Local Data
	 */
	private:
	// Point at which task table was last updated
	int taskPoint_;


	/*
	 * Window Functions
	 */
	public:
	// Show and update dialog
	void updateAndShow();
	// Close dialog window
	void terminate();

	private slots:
	void on_CancelButton_clicked(bool checked);
};

#endif
