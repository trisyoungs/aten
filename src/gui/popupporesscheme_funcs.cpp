/*
	*** Popup Widget - Pores Scheme
	*** src/gui/popupporesscheme_funcs.cpp
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

#include "gui/popupporesscheme.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "methods/partitioningscheme.h"
#include "methods/partitiondata.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
PoresSchemePopup::PoresSchemePopup(AtenWindow& parent, TMenuButton* buttonParent, PartitioningScheme& scheme) : TMenuButtonPopupWidget(buttonParent), parent_(parent), partitioningScheme_(scheme)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void PoresSchemePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool PoresSchemePopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
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

void PoresSchemePopup::on_GenerateSchemeButton_clicked(bool checked)
{
	// First check - does the current model have a unit cell?
	Model* m = parent_.aten().currentModelOrFrame();
	if (m->cell().type() == UnitCell::NoCell)
	{
		Messenger::print("Can't generate a partitioning scheme for a non-periodic model.");
		return;
	}

	// Grab some values so we are ready to run the command
	QString name;
	name.sprintf("Partitions for model '%s'", qPrintable(m->name()));
	Vec3<int> npoints(ui.PartitionGridXSpin->value(), ui.PartitionGridYSpin->value(), ui.PartitionGridZSpin->value());
	double minSizePcnt = ui.MinimumPartitionSizeSpin->value();
	int atomExtent = ui.AtomExtentSpin->value();

	// Run the command
	CommandNode::run(Commands::CreateScheme, "ciiidii", qPrintable(name), npoints.x, npoints.y, npoints.z, minSizePcnt, atomExtent, 0);
	
	// Update info in window
	double volume = 0.0;
	Matrix volumeElement = m->cell().axes();
	volumeElement.applyScaling(1.0/npoints.x, 1.0/npoints.y, 1.0/npoints.z);
	double elementVolume = volumeElement.determinant();
	for (PartitionData* pd = partitioningScheme_.partitions()->next; pd != NULL; pd = pd->next)
	{
		pd->calculateVolume(elementVolume);
		volume += pd->volume();
	}
	ui.PartitionNumberLabel->setText( QString::number(partitioningScheme_.nPartitions()-1) );
	ui.PartitionVolumeLabel->setText( QString::number(volume, 'f', 2) );
	ui.PartitionVolumePercentLabel->setText( QString::number(100.0*volume/m->cell().volume(), 'f', 2) + "%" );

	// Update the main window
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void PoresSchemePopup::on_CopySchemeButton_clicked(bool checked)
{
	// First check - does the current model have a unit cell?
	Model* m =parent_.aten().currentModelOrFrame();
	if (m->cell().type() == UnitCell::NoCell)
	{
		Messenger::print("Can't generate a partitioning scheme for a non-periodic model.");
		return;
	}

	// Grab some values so we are ready to run the command
	QString name;
	name.sprintf("Partitions for model '%s'", qPrintable(m->name()));
	Vec3<int> npoints(ui.PartitionGridXSpin->value(), ui.PartitionGridYSpin->value(), ui.PartitionGridZSpin->value());
	double minSizePcnt = ui.MinimumPartitionSizeSpin->value();
	int atomExtent = ui.AtomExtentSpin->value();
	CommandNode::run(Commands::CreateScheme, "ciiidii", qPrintable(name), npoints.x, npoints.y, npoints.z, minSizePcnt, atomExtent, 1);
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}
