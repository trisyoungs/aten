/*
	*** Disorder Window
	*** src/gui/disorder.h
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

#ifndef ATEN_DISORDERWINDOW_H
#define ATEN_DISORDERWINDOW_H

#include "gui/ui_disorder.h"
#include "templates/reflist.h"

// Forward declarations
class Model;

// Disordered builder window
class DisorderWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWidget();
	void refresh();
	private:
	void refreshComponentData(int row);
	void setComponentCentre();
	void setComponentGeometry();
	void setComponentRotation();
	private slots:
	void on_ComponentTable_itemClicked(QTableWidgetItem *widget);
	void on_ComponentTable_itemChanged(QTableWidgetItem *item);
	void on_ComponentRegionCombo_currentIndexChanged(int index);
	void on_ShowRegionsCheck_clicked(bool checked);
	void on_DisorderStartButton_clicked(bool checked);
	void on_VDWScaleSpin_valueChanged(double d);
	void on_CentreXSpin_valueChanged(double d);
	void on_CentreYSpin_valueChanged(double d);
	void on_CentreZSpin_valueChanged(double d);
	void on_CentreFracCheck_clicked(bool checked);
	void on_GeometryXSpin_valueChanged(double d);
	void on_GeometryYSpin_valueChanged(double d);
	void on_GeometryZSpin_valueChanged(double d);
	void on_GeometryFracCheck_clicked(bool checked);
	void on_RotationXSpin_valueChanged(double d);
	void on_RotationYSpin_valueChanged(double d);
	void on_RotationCheck_clicked(bool checked);
	void on_AllowOverlapCheck_clicked(bool checked);
	protected:
	void closeEvent(QCloseEvent *event);

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
	// Constructor
	DisorderWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::DisorderWidget ui;
};

#endif
