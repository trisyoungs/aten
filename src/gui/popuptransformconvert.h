/*
	*** Popup Widget - Transform Convert
	*** src/gui/popuptransformconvert.h
	Copyright T. Youngs 2007-2016

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

#ifndef ATEN_TRANSFORMCONVERTPOPUP_H
#define ATEN_TRANSFORMCONVERTPOPUP_H

#include "gui/ui_popuptransformconvert.h"
#include "gui/tpopupwidget.hui"
#include "parser/returnvalue.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class ReturnValue;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Popup Widget - Transform Reposition
class TransformConvertPopup : public TPopupWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	private:
	// Reference to main window
	AtenWindow& parent_;

	public:
	// Constructor / Destructor
	TransformConvertPopup(AtenWindow& parent, TMenuButton* buttonParent);
	// Main form declaration
	Ui::TransformConvertPopup ui;
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
	void on_SourcePickXButton_clicked(bool checked);
	void on_SourcePickYButton_clicked(bool checked);
	void on_SourcePickZButton_clicked(bool checked);
	void on_SourceNormaliseXButton_clicked(bool checked);
	void on_SourceNormaliseYButton_clicked(bool checked);
	void on_SourceNormaliseZButton_clicked(bool checked);
	void on_SourceOrthogonaliseXButton_clicked(bool checked);
	void on_SourceOrthogonaliseYButton_clicked(bool checked);
	void on_SourceOrthogonaliseZButton_clicked(bool checked);
	void on_SourceGenerateXButton_clicked(bool checked);
	void on_SourceGenerateYButton_clicked(bool checked);
	void on_SourceGenerateZButton_clicked(bool checked);
	void on_OriginCellCentreButton_clicked(bool checked);
	void on_DefineOriginButton_clicked(bool checked);
	void on_TargetPickXButton_clicked(bool checked);
	void on_TargetPickYButton_clicked(bool checked);
	void on_TargetPickZButton_clicked(bool checked);
	void on_TargetNormaliseXButton_clicked(bool checked);
	void on_TargetNormaliseYButton_clicked(bool checked);
	void on_TargetNormaliseZButton_clicked(bool checked);
	void on_TargetOrthogonaliseXButton_clicked(bool checked);
	void on_TargetOrthogonaliseYButton_clicked(bool checked);
	void on_TargetOrthogonaliseZButton_clicked(bool checked);
	void on_TargetGenerateXButton_clicked(bool checked);
	void on_TargetGenerateYButton_clicked(bool checked);
	void on_TargetGenerateZButton_clicked(bool checked);
};

#endif
