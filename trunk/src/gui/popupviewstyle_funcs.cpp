/*
	*** Popup Widget - View Style Functions
	*** src/gui/popupviewstyle_funcs.cpp
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

#include "gui/popupviewstyle.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
ViewStylePopup::ViewStylePopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Add style tool buttons to their button group
	ui.LineButton->setGroup("ViewStyles");
	ui.TubeButton->setGroup("ViewStyles");
	ui.SphereButton->setGroup("ViewStyles");
	ui.ScaledButton->setGroup("ViewStyles");
	ui.OwnButton->setGroup("ViewStyles");
}

// Show popup, updating any controls as necessary beforehand
void ViewStylePopup::popup()
{
	refreshing_ = true;

	switch (prefs.renderStyle())
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
		case (Prefs::OwnStyle):
			ui.OwnButton->setChecked(true);
			break;
		default:
			printf("Warning: Prefs has odd render style (%i)\n", prefs.renderStyle());
			break;
	}

	show();

	refreshing_ = false;
}

// Call named method associated to popup
bool ViewStylePopup::callMethod(QString methodName, ReturnValue& rv)
{
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
			case (Prefs::OwnStyle):
				parentMenuButton()->setIcon(QIcon(":/viewstyle/icons/viewstyle_own.png"));
				break;
			default:
				printf("Warning: Odd render style (%s) passed so can't set parent button's icon.\n", qPrintable(rv.asString()));
				break;
		}
	}
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */

void ViewStylePopup::on_LineButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::LineStyle);

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::LineStyle));

	parent_.aten().globalLogChange(Log::Style);

	parent_.updateWidgets(AtenWindow::MainViewTarget);

	// Hide popup
	done();
}

void ViewStylePopup::on_TubeButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::TubeStyle);

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::TubeStyle));

	parent_.aten().globalLogChange(Log::Style);

	parent_.updateWidgets(AtenWindow::MainViewTarget);

	// Hide popup
	done();
}

void ViewStylePopup::on_SphereButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::SphereStyle);

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::SphereStyle));

	parent_.aten().globalLogChange(Log::Style);

	parent_.updateWidgets(AtenWindow::MainViewTarget);

	// Hide popup
	done();
}

void ViewStylePopup::on_ScaledButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::ScaledStyle);

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::ScaledStyle));

	parent_.aten().globalLogChange(Log::Style);

	parent_.updateWidgets(AtenWindow::MainViewTarget);

	// Hide popup
	done();
}

void ViewStylePopup::on_OwnButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::OwnStyle);

	// Update icon
	callMethodSimple("updateButtonIcon", Prefs::drawStyle(Prefs::OwnStyle));

	parent_.aten().globalLogChange(Log::Style);

	parent_.updateWidgets(AtenWindow::MainViewTarget);

	// Hide popup
	done();
}
