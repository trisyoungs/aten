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
GrowPopup::GrowPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	QButtonGroup* buttonGroup = new QButtonGroup(this);
	buttonGroup->addButton(ui.VanDerWaalsRadio);
	buttonGroup->addButton(ui.FixedRadio);
}

// Update controls (before show()) (virtual)
void GrowPopup::updateControls()
{
	refreshing_ = true;

	switch (parent_.buildGeometry())
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
			printf("Warning: Viewer has odd atom geometry set for build (%i)\n", parent_.buildGeometry());
			break;
	}

	refreshing_ = false;
}

// Call named method associated to popup
bool GrowPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "updateButtonIcon")
	{
		if (!parentMenuButton()) return false;
		Atom::AtomGeometry ag = Atom::atomGeometry(rv.asString());
		switch (ag)
		{
			case (Atom::LinearGeometry):
				parentMenuButton()->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_linear.png"));
				ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_linear.png"));
				break;
			case (Atom::TShapeGeometry):
				parentMenuButton()->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_tshape.png"));
				ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_tshape.png"));
				break;
			case (Atom::TrigPlanarGeometry):
				parentMenuButton()->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_trigonal.png"));
				ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_trigonal.png"));
				break;
			case (Atom::TetrahedralGeometry):
				parentMenuButton()->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_tetrahedral.png"));
				ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_tetrahedral.png"));
				break;
			case (Atom::SquarePlanarGeometry):
				parentMenuButton()->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_sqplanar.png"));
				ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_sqplanar.png"));
				break;
			case (Atom::TrigBipyramidGeometry):
				parentMenuButton()->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_trigbipy.png"));
				ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_trigbipy.png"));
				break;
			case (Atom::OctahedralGeometry):
				parentMenuButton()->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_octahedral.png"));
				ui.GrowSelectionButton->setIcon(QIcon(":/atomgeometry/icons/atomgeometry_octahedral.png"));
				break;
			default:
				printf("Warning: Odd atom geometry passed (%s) so can't set parent button's icon.\n", qPrintable(rv.asString()));
				break;
		}
	}
	else if (methodName == "distance")
	{
		if (ui.VanDerWaalsRadio->isChecked()) rv = -1.0;
		else rv = ui.DistanceSpin->value();
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

void GrowPopup::on_GeometryLinearButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.setBuildGeometry(Atom::LinearGeometry);

	// Update icon
	callMethodSimple("updateButtonIcon", Atom::atomGeometry(Atom::LinearGeometry));

	// Hide popup (set button down, and click it)
	done(true, UserAction::DrawGrowAtomsAction);
}

void GrowPopup::on_GeometryTShapeButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.setBuildGeometry(Atom::TShapeGeometry);

	// Update icon
	callMethodSimple("updateButtonIcon", Atom::atomGeometry(Atom::TShapeGeometry));

	// Hide popup (set button down, and click it)
	done(true, UserAction::DrawGrowAtomsAction);
}

void GrowPopup::on_GeometryTrigonalButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.setBuildGeometry(Atom::TrigPlanarGeometry);

	// Update icon
	callMethodSimple("updateButtonIcon", Atom::atomGeometry(Atom::TrigPlanarGeometry));

	// Hide popup (set button down, and click it)
	done(true, UserAction::DrawGrowAtomsAction);
}

void GrowPopup::on_GeometryTetrahedralButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.setBuildGeometry(Atom::TetrahedralGeometry);

	// Update icon
	callMethodSimple("updateButtonIcon", Atom::atomGeometry(Atom::TetrahedralGeometry));

	// Hide popup (set button down, and click it)
	done(true, UserAction::DrawGrowAtomsAction);
}

void GrowPopup::on_GeometrySqPlanarButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.setBuildGeometry(Atom::SquarePlanarGeometry);

	// Update icon
	callMethodSimple("updateButtonIcon", Atom::atomGeometry(Atom::SquarePlanarGeometry));

	// Hide popup (set button down, and click it)
	done(true, UserAction::DrawGrowAtomsAction);
}

void GrowPopup::on_GeometryTrigBipyramidButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.setBuildGeometry(Atom::TrigBipyramidGeometry);

	// Update icon
	callMethodSimple("updateButtonIcon", Atom::atomGeometry(Atom::TrigBipyramidGeometry));

	// Hide popup (set button down, and click it)
	done(true, UserAction::DrawGrowAtomsAction);
}

void GrowPopup::on_GeometryOctahedralButton_clicked(bool checked)
{
	// Set geometry in viewer
	parent_.setBuildGeometry(Atom::OctahedralGeometry);

	// Update icon
	callMethodSimple("updateButtonIcon", Atom::atomGeometry(Atom::OctahedralGeometry));

	// Hide popup (set button down, and click it)
	done(true, UserAction::DrawGrowAtomsAction);
}

void GrowPopup::on_GrowSelectionButton_clicked(bool checked)
{
	// Run command
	if (ui.FixedRadio->isChecked()) CommandNode::run(Commands::SelectionGrowAtom, "icd", parent_.currentBuildElement(), Atom::atomGeometry(parent_.buildGeometry()), ui.DistanceSpin->value());
	else CommandNode::run(Commands::SelectionGrowAtom, "ic", parent_.currentBuildElement(), Atom::atomGeometry(parent_.buildGeometry()));

	// Hide popup
	done(false, UserAction::DrawGrowAtomsAction);
}
