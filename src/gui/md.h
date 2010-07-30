/*
	*** Qt GUI: MD Window
	*** src/gui/md.h
	Copyright T. Youngs 2007-2009

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

#ifndef ATEN_MDWINDOW_H
#define ATEN_MDWINDOW_H

#include "gui/ui_md.h"

// MD control window
class AtenMD : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	private:
	bool refreshing_;
	public:
	void showWindow();
	void refresh();
	private slots:
	void dialogFinished(int result);

	/*
	// Widget Signals
	*/
	private slots:
	void on_TemperatureSpin_valueChanged(double value);
	void on_PressureSpin_valueChanged(double value);
	void on_NStepsSpin_valueChanged(int value);
	void on_TimeStepMantissaSpin_valueChanged(double value);
	void on_TimeStepExponentSpin_valueChanged(int value);
	void on_RunMDButton_clicked(bool checked);

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenMD(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Destructor
	~AtenMD();
	// Main form declaration
	Ui::MDDialog ui;
	// Finalise widgets (things that couldn't be done in Qt Designer)
	void finaliseUi();
};

#endif
