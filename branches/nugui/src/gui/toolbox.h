/*
	*** Toolbox Dock Widget
	*** src/gui/toolbox.h
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

#ifndef ATEN_TOOLBOXWIDGET_H
#define ATEN_TOOLBOXWIDGET_H

#include "gui/ui_toolbox.h"

class ToolBoxWidget : public QDockWidget
{
	// All Qt declarations must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void updateButtons();
	private slots:
	void on_AtomListButton_clicked(bool checked);
	void on_BuildButton_clicked(bool checked);
	void on_CellDefinitionButton_clicked(bool checked);
	void on_CellTransformButton_clicked(bool checked);
	void on_CommandButton_clicked(bool checked);
	void on_DisorderButton_clicked(bool checked);
	void on_ForcefieldsButton_clicked(bool checked);
	void on_FragmentsButton_clicked(bool checked);
	void on_GeometryButton_clicked(bool checked);
	void on_GlyphsButton_clicked(bool checked);
	void on_GridsButton_clicked(bool checked);
	void on_ModelListButton_clicked(bool checked);
	void on_MDButton_clicked(bool checked);
	void on_MessagesButton_clicked(bool checked);
	void on_PositionButton_clicked(bool checked);
	void on_SelectButton_clicked(bool checked);
	void on_TransformButton_clicked(bool checked);
	void on_VibrationsButton_clicked(bool checked);
	void on_ZMatrixButton_clicked(bool checked);
	void dockWidgetVisibilityChanged(bool visibility);
	void dockWidgetTopLevelChanged(bool topLevel);
	
	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	ToolBoxWidget(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::ToolBoxWidget ui;
};

#endif
