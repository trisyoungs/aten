/*
	*** Popup Widget - Grid Style Functions
	*** src/gui/popupgridstyle_funcs.cpp
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

#include "gui/popupgridstyle.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GridStylePopup::GridStylePopup(AtenWindow& parent, TMenuButton* buttonParent, bool primary) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	primary_ = primary;
}

// Update controls (before show()) (virtual)
void GridStylePopup::updateControls()
{
	// Get current grid
	Grid* currentGrid;
	if (!parent_.aten().currentGrid(currentGrid)) return;

	refreshing_ = true;

	Grid::SurfaceStyle style = (primary_ ? currentGrid->primaryStyle() : currentGrid->secondaryStyle());
	switch (style)
	{
		case (Grid::PointSurface):
			ui.PointsButton->setChecked(true);
			break;
		case (Grid::MeshSurface):
			ui.MeshButton->setChecked(true);
			break;
		case (Grid::SolidSurface):
			ui.SolidButton->setChecked(true);
			break;
		default:
			printf("Warning: Grid has odd surface style set (%i)\n", style);
			break;
	}

	refreshing_ = false;
}

// Call named method associated to popup
bool GridStylePopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "updateButtonIcon")
	{
		if (!parentMenuButton()) return false;
		Grid::SurfaceStyle ss = Grid::surfaceStyle(rv.asString());
		switch (ss)
		{
			case (Grid::PointSurface):
				parentMenuButton()->setIcon(QIcon(":/gridstyle/icons/gridstyle_points.png"));
				break;
			case (Grid::MeshSurface):
				parentMenuButton()->setIcon(QIcon(":/gridstyle/icons/gridstyle_mesh.png"));
				break;
			case (Grid::SolidSurface):
				parentMenuButton()->setIcon(QIcon(":/gridstyle/icons/gridstyle_solid.png"));
				break;
			default:
				printf("Warning: Odd render style passed (%s) so can't set parent button's icon.\n", qPrintable(rv.asString()));
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

void GridStylePopup::on_PointsButton_clicked(bool checked)
{
	// Run command
	if (primary_) CommandNode::run(Commands::GridStyle, "c", Grid::surfaceStyle(Grid::PointSurface));
	else CommandNode::run(Commands::GridStyleSecondary, "c", Grid::surfaceStyle(Grid::PointSurface));

	// Update icon
	callMethodSimple("updateButtonIcon", Grid::surfaceStyle(Grid::PointSurface));

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);

	// Hide popup
	done();
}

void GridStylePopup::on_MeshButton_clicked(bool checked)
{
	// Run command
	if (primary_) CommandNode::run(Commands::GridStyle, "c", Grid::surfaceStyle(Grid::MeshSurface));
	else CommandNode::run(Commands::GridStyleSecondary, "c", Grid::surfaceStyle(Grid::MeshSurface));

	// Update icon
	callMethodSimple("updateButtonIcon", Grid::surfaceStyle(Grid::MeshSurface));

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);

	// Hide popup
	done();
}

void GridStylePopup::on_SolidButton_clicked(bool checked)
{
	// Run command
	if (primary_) CommandNode::run(Commands::GridStyle, "c", Grid::surfaceStyle(Grid::SolidSurface));
	else CommandNode::run(Commands::GridStyleSecondary, "c", Grid::surfaceStyle(Grid::SolidSurface));

	// Update icon
	callMethodSimple("updateButtonIcon", Grid::surfaceStyle(Grid::SolidSurface));

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);

	// Hide popup
	done();
}

