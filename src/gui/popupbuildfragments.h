/*
	*** Popup Widget - Build Fragments
	*** src/gui/popupbuildfragments.h
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

#ifndef ATEN_BUILDFRAGMENTSPOPUP_H
#define ATEN_BUILDFRAGMENTSPOPUP_H

#include "gui/ui_popupbuildfragments.h"
#include "gui/tmenubutton.hui"
#include "parser/returnvalue.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class ReturnValue;
class Fragment;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Popup Widget - Grow
class BuildFragmentsPopup : public TMenuButtonPopupWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	BuildFragmentsPopup(AtenWindow& parent, TMenuButton* buttonParent);
	// Main form declaration
	Ui::BuildFragmentsPopup ui;
	// Show popup, updating any controls as necessary beforehand
	void popup();
	// Call named method associated to popup
	bool callMethod(QString methodName, ReturnValue& rv);


	/*
	 * Reimplementations
	 */
	protected:
	void hideEvent(QHideEvent* event) { TMenuButtonPopupWidget::hideEvent(event); }


	/*
	 * Local Variables
	 */
	private:
	// Current drawing fragment
	Fragment* currentFragment_;


	/*
	 * Widget Functions
	 */
	private slots:
	void on_FragmentTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous);
	void on_FragmentTable_currentItemChanged(QTableWidgetItem *current, QTableWidgetItem *previous);
	void on_FilterEdit_textChanged(const QString &text);
	void on_ClearFilterButton_clicked(bool checked);
	void on_ViewAsGridCheck_clicked(bool checked);
};

#endif
