/*
	*** Popup Widget - Transform Multiply
	*** src/gui/popuptransformmultiply.h
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

#ifndef ATEN_TRANSFORMMULTIPLYPOPUP_H
#define ATEN_TRANSFORMMULTIPLYPOPUP_H

#include "gui/ui_popuptransformmultiply.h"
#include "gui/tpopupwidget.hui"
#include "parser/returnvalue.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class ReturnValue;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Popup Widget - Transform Multiply
class TransformMultiplyPopup : public TPopupWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	TransformMultiplyPopup(AtenWindow& parent, TMenuButton* buttonParent);
	// Main form declaration
	Ui::TransformMultiplyPopup ui;
	// Update controls (before show()) (virtual)
	void updateControls();
	// Call named method associated to popup
	bool callMethod(QString methodName, ReturnValue& rv);


	/*
	 * Reimplementations
	 */
	protected:
	void hideEvent(QHideEvent* event) { TPopupWidget::hideEvent(event); }


	/*
	 * Widget Functions
	 */
	private slots:
	void on_PickXButton_clicked(bool checked);
	void on_PickYButton_clicked(bool checked);
	void on_PickZButton_clicked(bool checked);
	void on_NormaliseXButton_clicked(bool checked);
	void on_NormaliseYButton_clicked(bool checked);
	void on_NormaliseZButton_clicked(bool checked);
	void on_OrthogonaliseXButton_clicked(bool checked);
	void on_OrthogonaliseYButton_clicked(bool checked);
	void on_OrthogonaliseZButton_clicked(bool checked);
	void on_GenerateXButton_clicked(bool checked);
	void on_GenerateYButton_clicked(bool checked);
	void on_GenerateZButton_clicked(bool checked);
	void on_OriginCellCentreButton_clicked(bool checked);
	void on_DefineOriginButton_clicked(bool checked);
};

#endif
