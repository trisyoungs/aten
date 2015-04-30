/*
	*** Popup Widget - Grow Functions
	*** src/gui/popupbuildgrow_funcs.cpp
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

#include "gui/popupbuildgrow.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GrowPopup::GrowPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void GrowPopup::popup()
{
	refreshing_ = true;

	switch (parent_.ui.MainView->buildGeometry())
	{
		case (Atom::LinearGeometry):
			ui.GeometryLinearButton->setChecked(true);
			break;
		case (Atom::TShapeGeometry):
			ui.GeometryTShapeButton->setChecked(true);
			break;
		case (Atom::TrigPlanarGeometry):
			ui.GeometryTrigonalButton->setChecked(true);
			break;
		case (Atom::TetrahedralGeometry):
			ui.GeometryTetrahedralButton->setChecked(true);
			break;
		case (Atom::SquarePlanarGeometry):
			ui.GeometrySqPlanarButton->setChecked(true);
			break;
		case (Atom::TrigBipyramidGeometry):
			ui.GeometryTrigBipyramidButton->setChecked(true);
			break;
		case (Atom::OctahedralGeometry):
			ui.GeometryOctahedralButton->setChecked(true);
			break;
		default:
			printf("Warning: Viewer has odd atom geometry set for build (%i)\n", parent_.ui.MainView->buildGeometry());
			break;
	}

	show();

	refreshing_ = false;
}

// Call named method associated to popup
bool GrowPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */

void GrowPopup::on_GeometryLinearButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.ui.MainView->setBuildGeometry(Atom::LinearGeometry);

	// Set icon in Grow buttons
	parent_.ui.BuildDrawGrowButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_linear.png"));
	ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_linear.png"));

	// Hide popup
	done();
}

void GrowPopup::on_GeometryTShapeButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.ui.MainView->setBuildGeometry(Atom::TShapeGeometry);

	// Set icon in Grow buttons
	parent_.ui.BuildDrawGrowButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_tshape.png"));
	ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_tshape.png"));

	// Hide popup
	done();
}

void GrowPopup::on_GeometryTrigonalButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.ui.MainView->setBuildGeometry(Atom::TrigPlanarGeometry);

	// Set icon in Grow buttons
	parent_.ui.BuildDrawGrowButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_trigonal.png"));
	ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_trigonal.png"));

	// Hide popup
	done();
}

void GrowPopup::on_GeometryTetrahedralButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.ui.MainView->setBuildGeometry(Atom::TetrahedralGeometry);

	// Set icon in Grow buttons
	parent_.ui.BuildDrawGrowButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_tetrahedral.png"));
	ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_tetrahedral.png"));

	// Hide popup
	done();
}
void GrowPopup::on_GeometrySqPlanarButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.ui.MainView->setBuildGeometry(Atom::SquarePlanarGeometry);

	// Set icon in Grow buttons
	parent_.ui.BuildDrawGrowButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_sqplanar.png"));
	ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_sqplanar.png"));

	// Hide popup
	done();
}

void GrowPopup::on_GeometryTrigBipyramidButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.ui.MainView->setBuildGeometry(Atom::TrigBipyramidGeometry);

	// Set icon in Grow buttons
	parent_.ui.BuildDrawGrowButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_trigbipy.png"));
	ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_trigbipy.png"));

	// Hide popup
	done();
}

void GrowPopup::on_GeometryOctahedralButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.ui.MainView->setBuildGeometry(Atom::OctahedralGeometry);

	// Set icon in Grow buttons
	parent_.ui.BuildDrawGrowButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_octahedral.png"));
	ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_octahedral.png"));

	// Hide popup
	done();
}

void GrowPopup::on_GrowSelectionButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::SelectionGrowAtom, "ic", parent_.currentBuildElement(), Atom::AtomGeometry(parent_.ui.MainView->buildGeometry()));

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);

	// Hide popup
	done();
}

