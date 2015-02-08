/*
	*** Build Dock Widget
	*** src/gui/build.h
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

#ifndef ATEN_BUILDWIDGET_H
#define ATEN_BUILDWIDGET_H

#include "gui/ui_build.h"

// Build (draw) window
class BuildWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Enum for extra bond calculation button menu items
	enum RebondMenuItems { ModelNoAugmentItem, SelectionItem, SelectionNoAugmentItem, PatternsItem, PatternsNoAugmentItem };

	/*
	// Window Functions
	*/
	public:
	void showWidget();
	private slots:
	// Edit Tab - Draw
	void on_ElementHButton_clicked(bool checked);
	void on_ElementCButton_clicked(bool checked);
	void on_ElementNButton_clicked(bool checked);
	void on_ElementOButton_clicked(bool checked);
	void on_ElementCustomButton_clicked(bool checked);
	void on_ElementPickButton_clicked(bool checked);
	void on_DrawGrowMenuButton_menuItemClicked(int menuItemId);
	void on_DrawTransmuteMenuButton_menuItemClicked(int menuItemId);
	void on_DrawAddHMenuButton_menuItemClicked(int menuItemId);
	// Edit Tab - Bond
	void on_DrawRebondButton_clicked(bool checked);
	void on_DrawClearBondingButton_clicked(bool checked);
	void on_DrawAugmentButton_clicked(bool checked);
	void on_DrawRebondMenuButton_menuItemClicked(int menuItemId);
	void on_DrawClearBondingMenuButton_menuItemClicked(int menuItemId);
	// Tools Tab - Add Atom
	void on_AddAtomButton_clicked(bool checked);
	// Options Tab
	void on_BondToleranceSlider_valueChanged(int value);
	void on_BondToleranceSpin_valueChanged(double value);
	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Local variables
	*/
	private:
	// Current custom element
	int customElement_;

	/*
	// Dialog
	*/
	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	BuildWidget(AtenWindow& parent1, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::BuildWidget ui;
};

#endif
