/*
	*** Main Window - View Panel Functions
	*** src/gui/mainwindow_panel_view.cpp
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

#include "gui/mainwindow.h"
#include "main/aten.h"

// Static local variables
Matrix storedView;

// Update view panel
void AtenWindow::updateViewPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateViewPanel");

	// Set button status
	TMenuButton::setGroupButtonChecked("Styles", Prefs::drawStyle(prefs.renderStyle()));
	TMenuButton::setGroupButtonChecked("Colourschemes", Prefs::colouringScheme(prefs.colourScheme()));
	ui.ViewControlPerspectiveButton->setChecked(prefs.hasPerspective());
	ui.ViewControlDetectHBondsButton->setChecked(prefs.drawHydrogenBonds());  //ATEN2 TODO Move this from Prefs?
	ui.ViewControlLockViewButton->setChecked(Model::useCommonModelViewMatrix());

	Messenger::exit("AtenWindow::updateViewPanel");
}

/*
 * Control
 */

void AtenWindow::on_ViewControlResetButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->resetView(ui.MainView->contextWidth(), ui.MainView->contextHeight());
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewControlZoomInButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->adjustCamera(0.0,0.0,5.0);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewControlZoomOutButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->adjustCamera(0.0,0.0,-5.0);
	updateWidgets(AtenWindow::MainViewTarget);
}

// Set perspective view
void AtenWindow::on_ViewControlPerspectiveButton_clicked(bool checked)
{
	if (refreshing_) return;

	prefs.setPerspective(checked);
	updateWidgets(AtenWindow::MainViewTarget);
}

// Get current view
void AtenWindow::on_ViewControlGetButton_clicked(bool checked)
{
	// Get view from current model
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;
	storedView = currentModel->modelViewMatrix();
}

// Set current view (from stored view)
void AtenWindow::on_ViewControlSetButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::SetView, "dddddddddddd", storedView[0], storedView[1], storedView[2], storedView[4], storedView[5], storedView[6], storedView[8], storedView[9], storedView[10], storedView[12], storedView[13], storedView[14]);

	updateWidgets(AtenWindow::MainViewTarget);
}

// Toggle detection of H-Bonds
void AtenWindow::on_ViewControlDetectHBondsButton_clicked(bool checked)
{
	if (refreshing_) return;
	
	prefs.setDrawHydrogenBonds(checked);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewControlLockViewButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get view from current model
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;
	currentModel->setCommonViewMatrixFromLocal();

	Model::setUseCommonModelViewMatrix(checked);


	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Style
 */

void AtenWindow::on_ViewStyleLineButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setRenderStyle(Prefs::LineStyle);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewStyleTubeButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setRenderStyle(Prefs::TubeStyle);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewStyleSphereButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setRenderStyle(Prefs::SphereStyle);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewStyleScaledButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setRenderStyle(Prefs::ScaledStyle);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewStyleOwnButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setRenderStyle(Prefs::OwnStyle);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Colour Scheme
 */

void AtenWindow::on_ViewSchemeElementButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ElementScheme);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewSchemeChargeButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ChargeScheme);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewSchemeForceButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ForceScheme);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewSchemeVelocityButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::VelocityScheme);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ViewSchemeOwnButton_clicked(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::OwnScheme);
	aten_.globalLogChange(Log::Style);
	updateWidgets(AtenWindow::MainViewTarget);
}
