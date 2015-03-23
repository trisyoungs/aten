/*
	*** Pores Dock Widget
	*** src/gui/pores_funcs.cpp
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

#include <QtGui/QCloseEvent>
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/pores.h"
#include "base/sysfunc.h"

// Static members
// PartitioningScheme PoresWidget::partitioningScheme_;

// Constructor
PoresWidget::PoresWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
	
	// Private variables
	partitioningScheme_.initialiseAbsolute("Generated Scheme", "Scheme generated from model");
}

// Show window
void PoresWidget::showWidget()
{
	show();
}

// Return the widgets partitioning scheme
PartitioningScheme &PoresWidget::partitioningScheme()
{
	return partitioningScheme_;
}

/*
// Drill Tab
*/

void PoresWidget::on_PoreSelectButton_clicked(bool checked)
{
	// First check - does the current model have a unit cell?
	Model* m = parent_.aten().currentModelOrFrame();
	if (m->cell()->type() == UnitCell::NoCell)
	{
		Messenger::print("Can't drill pores in a non-periodic model.");
		return;
	}

	// Grab some values so we are ready to run the command
	int nx = ui.ArrayXSpin->value(), ny = ui.ArrayYSpin->value();
	int face = ui.OriginPlaneCombo->currentIndex()+1;
	Vec3<double> v(ui.PoreVectorX->value(), ui.PoreVectorY->value(), ui.PoreVectorZ->value());
	Dnchar geometry = qPrintable(ui.PoreGeometryCombo->currentText());
	double sizeParam = ui.PoreSizeSpin->value();
	CommandNode::run(Commands::SelectPores, "cdiiiddd", geometry.get(), sizeParam, nx, ny, face, v.x, v.y, v.z);
	parent_.postRedisplay();
}

void PoresWidget::on_PoreSelectAndCutButton_clicked(bool checked)
{
	// First check - does the current model have a unit cell?
	Model* m =parent_.aten().currentModelOrFrame();
	if (m->cell()->type() == UnitCell::NoCell)
	{
		Messenger::print("Can't drill pores in a non-periodic model.");
		return;
	}

	// Grab some values so we are ready to run the command
	int nx = ui.ArrayXSpin->value(), ny = ui.ArrayYSpin->value();
	int face = ui.OriginPlaneCombo->currentIndex()+1;
	Vec3<double> v(ui.PoreVectorX->value(), ui.PoreVectorY->value(), ui.PoreVectorZ->value());
	Dnchar geometry = qPrintable(ui.PoreGeometryCombo->currentText());
	double sizeParam = ui.PoreSizeSpin->value();
	CommandNode::run(Commands::DrillPores, "cdiiiddd", geometry.get(), sizeParam, nx, ny, face, v.x, v.y, v.z);
	parent_.postRedisplay();
}

/*
// Terminate Tab
*/

void PoresWidget::on_TerminateButton_clicked(bool checked)
{
	// First check - are any atoms selected
	Model* m =parent_.aten().currentModelOrFrame();
	if (m->nSelected() == 0)
	{
		Messenger::print("No atoms selected in current model, so nothing to terminate.");
		return;
	}

	// Run the command
	CommandNode::run(Commands::Terminate, "");
	parent_.postRedisplay();
}

/*
// Scheme Tab
*/

void PoresWidget::on_GenerateSchemeButton_clicked(bool checked)
{
	// First check - does the current model have a unit cell?
	Model* m =parent_.aten().currentModelOrFrame();
	if (m->cell()->type() == UnitCell::NoCell)
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
	CommandNode::run(Commands::CreateScheme, "ciiidii", qPrintable(name), npoints.x, npoints.y, npoints.z, minSizePcnt, atomExtent, 0);
	
	// Update info in window
	double volume = 0.0;
	Matrix volumeElement = m->cell()->axes();
	volumeElement.applyScaling(1.0/npoints.x, 1.0/npoints.y, 1.0/npoints.z);
	double elementVolume = volumeElement.determinant();
	for (PartitionData* pd = partitioningScheme_.partitions()->next; pd != NULL; pd = pd->next)
	{
		pd->calculateVolume(elementVolume);
		volume += pd->volume();
	}
	Dnchar s;
	ui.PartitionNumberLabel->setText( QString::number(partitioningScheme_.nPartitions()-1) );
	s.sprintf("%10.2f", volume);
	ui.PartitionVolumeLabel->setText( s.get() );
	s.sprintf("%5.1f %%", 100.0*volume/m->cell()->volume());
	ui.PartitionVolumePercentLabel->setText( s.get() );
	parent_.postRedisplay();
}

void PoresWidget::on_CopySchemeButton_clicked(bool checked)
{
	// First check - does the current model have a unit cell?
	Model* m =parent_.aten().currentModelOrFrame();
	if (m->cell()->type() == UnitCell::NoCell)
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
	parent_.postRedisplay();
}

void PoresWidget::closeEvent(QCloseEvent *event)
{
	event->accept();
}
