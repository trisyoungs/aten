/*
	*** Qt GUI: Disorder Window
	*** src/gui/disorder.h
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

#ifndef ATEN_DISORDERWINDOW_H
#define ATEN_DISORDERWINDOW_H

#include "gui/ui_disorder.h"
#include "templates/reflist.h"

// Forward declarations
class Model;

// Disordered builder window
class AtenDisorder : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	void refresh();
	private:
	void refreshComponentData();
	void setComponentCentre();
	void setComponentSize();
	private slots:
	void on_ComponentTable_itemSelectionChanged();
	void on_ComponentTable_itemChanged(QTableWidgetItem *item);
	void on_ComponentRegionCombo_currentIndexChanged(int index);
	void on_ShowRegionsCheck_clicked(bool checked);
	void on_DisorderStartButton_clicked(bool checked);
	void on_VDWScaleSpin_valueChanged(double d);
	void on_CentreXSpin_valueChanged(double d);
	void on_CentreYSpin_valueChanged(double d);
	void on_CentreZSpin_valueChanged(double d);
	void on_SizeXSpin_valueChanged(double d);
	void on_SizeYSpin_valueChanged(double d);
	void on_SizeZSpin_valueChanged(double d);
	void on_CentreFracCheck_clicked(bool checked);
	void on_SizeFracCheck_clicked(bool checked);
	void dialogFinished(int result);

	/*
	// Local variables
	*/
	private:
	Reflist<Model, int> componentList;
	bool refreshing_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenDisorder(QWidget *parent = 0);
	~AtenDisorder();
	// Main form declaration
	Ui::DisorderDialog ui;
};

#endif
