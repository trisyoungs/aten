/*
	*** Popup Widget - Grid Set Functions
	*** src/gui/popupgridset_funcs.cpp
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

#include "gui/popupgridset.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GridSetPopup::GridSetPopup(AtenWindow& parent, TMenuButton* buttonParent, bool primary) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	primary_ = primary;
}

// Update controls (before show()) (virtual)
void GridSetPopup::updateControls()
{
	// Get current grid
	Grid* currentGrid;
	if (!parent_.aten().currentGrid(currentGrid)) return;

	refreshing_ = true;

	ui.GridMinimumLabel->setText(QString::number(currentGrid->minimum()));
	ui.GridMaximumLabel->setText(QString::number(currentGrid->maximum()));
	ui.GridNPointsLabel->setText(QString::number(currentGrid->nPoints()));
	ui.GridTotalPositiveSumLabel->setText(QString::number(currentGrid->totalPositiveSum()));
	ui.GridTotalNegativeSumLabel->setText(QString::number(currentGrid->totalNegativeSum()));
	ui.GridTotalAbsoluteLabel->setText(QString::number(currentGrid->totalAbsoluteSum()));
	if (primary_) ui.ViewPercentageLabel->setText(QString::number(100.0*currentGrid->partialPrimarySum()/currentGrid->totalAbsoluteSum()) + "%");
	else ui.ViewPercentageLabel->setText(QString::number(100.0*currentGrid->partialSecondarySum()/currentGrid->totalAbsoluteSum()) + "%");

	refreshing_ = false;
}

// Call named method associated to popup
bool GridSetPopup::callMethod(QString methodName, ReturnValue& rv)
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

void GridSetPopup::on_SetDensityMultipleButton_clicked(bool checked)
{
	// Run command
	if (primary_) CommandNode::run(Commands::GridCutoff, "d", ui.NumberDensitySpin->value()*ui.MultipleSpin->value());
	else CommandNode::run(Commands::GridCutoffSecondary, "d", ui.NumberDensitySpin->value()*ui.MultipleSpin->value());

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
}

void GridSetPopup::on_ViewPercentageSpin_valueChanged(double value)
{
	// Run command
	//if (primary_) CommandNode::run(Commands::GridViewPercentage, "d", ui.NumberDensitySpin->value()*ui.MultipleSpin->value());
	//else CommandNode::run(Commands::GridViewPercentageSecondary, "d", ui.NumberDensitySpin->value()*ui.MultipleSpin->value());

	// Update display
	//parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
	if (refreshing_) return;

	// TEST
	// Get current grid data
	Grid* currentGrid;
	if (!parent_.aten().currentGrid(currentGrid)) return;

	// Get data (assume regular gridded 3D for now)
	double*** data = currentGrid->data3d();
	Vec3<int> nXYZ = currentGrid->nXYZ();

	int nBins = 10, nPasses = 10;
	double targetPercentage = ui.ViewPercentageSpin->value();
	double binDelta, histogramStart;
	Array<double> histogram(nBins), fractions(nBins);
	double point;
	int bin, x, y, z, n, m, lowBin, highBin;

	// Set initial histogram limits
	histogramStart = currentGrid->minimum();
	binDelta = (currentGrid->upperPrimaryCutoff() - currentGrid->minimum()) / nBins;
	
	// Loop over passes
	for (int pass = 0; pass < nPasses; ++pass)
	{
		// Zero histogram
		histogram = 0.0;

		// Bin grid data
		for (x = 0; x < nXYZ.x; ++x)
		{
			for (y = 0; y < nXYZ.y; ++y)
			{
				for (z = 0; z < nXYZ.z; ++z)
				{
					// Grab point and add it to the histogram
					point = data[x][y][z];
					bin = (point - histogramStart) / binDelta;
					if (bin < 0) bin = 0;
					else if (bin >= nBins) bin = nBins-1;
					histogram[bin] += fabs(point);
				}
			}
		}

		// Calculate fractions of data represented by each bin (cutoff)
		fractions = 0.0;
		for (n=0; n<nBins; ++n)
		{
			for (m=0; m<=n; ++m) fractions[m] += histogram[n];
		}
		fractions /= currentGrid->totalAbsoluteSum();

		// TEST print out bin values and sums
// 		for (n=0; n<nBins; ++n) printf("HistoBin %i (x=%f) grid% = %f\n", n, histogramStart+binDelta*n, fractions[n]*100.0);

		// Find new limits for next pass
		for (highBin=0; highBin<nBins; ++highBin) if ((fractions[highBin]*100.0) < targetPercentage) break;
		if (highBin >= nBins)
		{
			// Target cutoff is beyond the current maximum limit of the histogram?
			Messenger::print("Unable to find suitable cutoff for requested percentage (%f)\n", targetPercentage);
			return;
		}
		for (lowBin=highBin-1; lowBin>=0; --lowBin) if ((fractions[lowBin]*100.0) > targetPercentage) break;
		if (lowBin < 0)
		{
			// Target cutoff is beyond the current minimum limit of the histogram?
			Messenger::print("Unable to find suitable cutoff for requested percentage (%f)\n", targetPercentage);
			return;
		}
// 		printf("Low/Hi bins are %i and %i\n", lowBin, highBin);
		histogramStart = histogramStart+binDelta*lowBin;
		binDelta = (binDelta*(highBin-lowBin)) / nBins;
// 		printf("New start and delta are %f and %f\n", histogramStart, binDelta);

		// Done?
		if (binDelta < (currentGrid->maximum()-currentGrid->minimum())*0.001) break;
	}

	// Set new density value
	parent_.ui.GridsPrimaryLowerCutoffSpin->setValue(histogramStart);

	updateControls();
}
