/*
	*** Popup Widget - View Style Functions
	*** src/gui/popupviewstyle_funcs.cpp
	Copyright T. Youngs 2007-2018

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

#include "gui/popupviewstyle.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
ViewStylePopup::ViewStylePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Add style tool buttons to their button group
	static int popupCounter = 0;
	ui.LineButton->setGroup("ViewStylePopup"+QString::number(popupCounter));
	ui.TubeButton->setGroup("ViewStylePopup"+QString::number(popupCounter));
	ui.SphereButton->setGroup("ViewStylePopup"+QString::number(popupCounter));
	ui.ScaledButton->setGroup("ViewStylePopup"+QString::number(popupCounter));
	++popupCounter;
}

// Update controls (before show()) (virtual)
void ViewStylePopup::updateControls()
{
	Model* model = parent_.aten().currentModelOrFrame();
	if (!model) return;

	refreshing_ = true;

	switch (model->drawStyle())
	{
		case (Prefs::LineStyle):
			ui.LineButton->setChecked(true);
			break;
		case (Prefs::TubeStyle):
			ui.TubeButton->setChecked(true);
			break;
		case (Prefs::SphereStyle):
			ui.SphereButton->setChecked(true);
			break;
		case (Prefs::ScaledStyle):
			ui.ScaledButton->setChecked(true);
			break;
		default:
			printf("Warning: Prefs has odd render style (%i)\n", model->drawStyle());
			break;
	}

	refreshing_ = false;
}

// Call named method associated to popup
bool ViewStylePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "updateButtonIcon")
	{
		if (!parentMenuButton()) return false;
		Prefs::DrawStyle ds = Prefs::drawStyle(rv.asString());
		switch (ds)
		{
			case (Prefs::LineStyle):
				parentMenuButton()->setIcon(QIcon(":/viewstyle/icons/viewstyle_line.png"));
				break;
			case (Prefs::TubeStyle):
				parentMenuButton()->setIcon(QIcon(":/viewstyle/icons/viewstyle_tube.png"));
				break;
			case (Prefs::SphereStyle):
				parentMenuButton()->setIcon(QIcon(":/viewstyle/icons/viewstyle_sphere.png"));
				break;
			case (Prefs::ScaledStyle):
				parentMenuButton()->setIcon(QIcon(":/viewstyle/icons/viewstyle_scaled.png"));
				break;
			default:
				printf("Warning: Odd render style (%s) passed so can't set parent button's icon.\n", qPrintable(rv.asString()));
				break;
		}
	}
	else if (methodName == "currentStyle")
	{
		if (ui.LineButton->isChecked()) rv = QString(Prefs::drawStyle(Prefs::LineStyle));
		else if (ui.TubeButton->isChecked()) rv = QString(Prefs::drawStyle(Prefs::TubeStyle));
		else if (ui.SphereButton->isChecked()) rv = QString(Prefs::drawStyle(Prefs::SphereStyle));
		else if (ui.ScaledButton->isChecked()) rv = QString(Prefs::drawStyle(Prefs::ScaledStyle));
		return true;
	}
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else
	{
		printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
		result = false;
	}
	return result;
}

/*
 * Widget Functions
 */

void ViewStylePopup::on_LineButton_clicked(bool checked)
{
	if (!checked) return;

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::LineStyle));

	// Hide popup
	done();
}

void ViewStylePopup::on_TubeButton_clicked(bool checked)
{
	if (!checked) return;

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::TubeStyle));

	// Hide popup
	done();
}

void ViewStylePopup::on_SphereButton_clicked(bool checked)
{
	if (!checked) return;

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::SphereStyle));

	// Hide popup
	done();
}

void ViewStylePopup::on_ScaledButton_clicked(bool checked)
{
	if (!checked) return;

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::ScaledStyle));

	// Hide popup
	done();
}
