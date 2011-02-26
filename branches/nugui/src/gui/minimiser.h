/*
	*** Minimiser Dock Window
	*** src/gui/minimiser.h
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

#ifndef ATEN_MINIMISERWIDGET_H
#define ATEN_MINIMISERWIDGET_H

#include "gui/ui_minimiser.h"

// Minimiser window
class MinimiserWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	// Minimisation algorithms
	enum MinimiserMethod { SimpleSteepestMethod, SteepestMethod, ConjugateMethod, MonteCarloMethod, MopacMethod, nMinimiserMethods };
	void showWidget();
	private slots:
	void on_MinimiserMethodCombo_currentIndexChanged(int index);
	void on_MinimiseButton_clicked(bool checked);
	public:
	void doMinimisation();

	/*
	// Local variables
	*/
	private:

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	MinimiserWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~MinimiserWidget();
	// Main form declaration
	Ui::MinimiserWidget ui;
};

#endif
