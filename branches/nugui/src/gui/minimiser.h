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

#ifndef ATEN_FORCEFIELDSWIDGET_H
#define ATEN_FORCEFIELDSWIDGET_H

#include "gui/ui_minimiser.h"

// Forward Declarations
class QFileDialog;

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
	void refresh();
	void refreshTypes();
	void loadForcefield();
	private slots:
	// Forcefields Tab
	void on_ForcefieldCombo_currentIndexChanged(int index);
	void on_LoadForcefieldButton_clicked(bool checked);
	void on_RemoveForcefieldButton_clicked(bool checked);
	void on_EditForcefieldButton_clicked(bool checked);
	void on_TypeModelButton_clicked(bool checked);
	void on_UntypeModelButton_clicked(bool checked);
	void on_AssignFFToCurrentButton_clicked(bool checked);
	void on_AssignFFToAllButton_clicked(bool checked);
	void on_AssignFFToPatternButton_clicked(bool clicked);
	// Energy Tab
	void on_MinimiserMethodCombo_currentIndexChanged(int index);
	void on_CurrentEnergyButton_clicked(bool checked);
	void on_CurrentForcesButton_clicked(bool checked);
	void on_MinimiseButton_clicked(bool checked);
	// Manual Typing Tab
	void on_ManualTypeSetButton_clicked(bool checked);
	void on_ManualTypeClearButton_clicked(bool checked);
	void on_ManualTypeTestButton_clicked(bool checked);
	void on_ManualTypeEdit_returnPressed();
	public:
	void doMinimisation();
	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Local variables
	*/
	private:
	// Whether the widget is currently refreshing
	bool refreshing_;
	// Element selected in Type filter
	int typelistElement_;

	public:
	// File dialogs for forcefields
	QFileDialog *openForcefieldDialog, *saveForcefieldDialog;
	
	
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
