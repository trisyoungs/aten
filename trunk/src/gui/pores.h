/*
	*** Pores Dock Widget
	*** src/gui/pores.h
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

#ifndef ATEN_PORESWIDGET_H
#define ATEN_PORESWIDGET_H

#include "gui/ui_pores.h"
#include "methods/partition.h"

// Pores window
class PoresWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Enum for pore shapes
	enum PoreGeometry { CylinderGeometry };

	/*
	// Window Functions
	*/
	public:
	void showWidget();
	private slots:
	// Drill Tab
	void on_PoreSelectButton_clicked(bool checked);
	void on_PoreSelectAndCutButton_clicked(bool checked);
	// Terminate Tab
	// Scheme Tab
	void on_GenerateSchemeButton_clicked(bool checked);
	void on_CopySchemeButton_clicked(bool checked);
	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Local variables
	*/
	private:
	// Partitioning scheme
	PartitioningScheme partitioningScheme_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	PoresWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::PoresWidget ui;
};

#endif
