/*
	*** Popup Widget - Transform Multiply Functions
	*** src/gui/popuptransformmultiply_funcs.cpp
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

#include "gui/popuptransformmultiply.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
TransformMultiplyPopup::TransformMultiplyPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformMultiplyPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformMultiplyPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "multiply")
	{
		Model* currentModel = parent_.aten().currentModelOrFrame();
		if (!currentModel) return false;

		// Put values into our matrix...
		Matrix mat;
		mat.setColumn(0, ui.MatrixXXSpin->value(), ui.MatrixXYSpin->value(), ui.MatrixXZSpin->value(), 0.0);
		mat.setColumn(1, ui.MatrixYXSpin->value(), ui.MatrixYYSpin->value(), ui.MatrixYZSpin->value(), 0.0);
		mat.setColumn(2, ui.MatrixZXSpin->value(), ui.MatrixZYSpin->value(), ui.MatrixZZSpin->value(), 0.0);
		// ...and grab coordinate origin
		Vec3<double> v;
		v.set(ui.OriginXSpin->value(), ui.OriginYSpin->value(), ui.OriginZSpin->value());
		CommandNode::run(Commands::MatrixTransform, "dddddddddddd", mat[0], mat[1], mat[2], mat[4], mat[5], mat[6], mat[8], mat[9], mat[10], v.x, v.y, v.z);

		parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
	}
	else if (methodName == "setXVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for X axis.");
			return false;
		}
		ui.MatrixXXSpin->setValue(v.x);
		ui.MatrixXYSpin->setValue(v.y);
		ui.MatrixXZSpin->setValue(v.z);
		return true;
	}
	else if (methodName == "setYVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for Y axis.");
			return false;
		}
		ui.MatrixYXSpin->setValue(v.x);
		ui.MatrixYYSpin->setValue(v.y);
		ui.MatrixYZSpin->setValue(v.z);
		return true;
	}
	else if (methodName == "setZVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for Z axis.");
			return false;
		}
		ui.MatrixZXSpin->setValue(v.x);
		ui.MatrixZYSpin->setValue(v.y);
		ui.MatrixZZSpin->setValue(v.z);
		return true;
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

void TransformMultiplyPopup::on_PickXButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.ui.MainView->setSelectedMode(UserAction::TransformPickAAction);

	done();
}

void TransformMultiplyPopup::on_PickYButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.ui.MainView->setSelectedMode(UserAction::TransformPickBAction);

	done();
}

void TransformMultiplyPopup::on_PickZButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.ui.MainView->setSelectedMode(UserAction::TransformPickCAction);

	done();
}

void TransformMultiplyPopup::on_NormaliseXButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.MatrixXXSpin->value(), ui.MatrixXYSpin->value(), ui.MatrixXZSpin->value());
	v.normalise();
	ui.MatrixXXSpin->setValue(v.x);
	ui.MatrixXYSpin->setValue(v.y);
	ui.MatrixXZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_NormaliseYButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.MatrixYXSpin->value(), ui.MatrixYYSpin->value(), ui.MatrixYZSpin->value());
	v.normalise();
	ui.MatrixYXSpin->setValue(v.x);
	ui.MatrixYYSpin->setValue(v.y);
	ui.MatrixYZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_NormaliseZButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.MatrixZXSpin->value(), ui.MatrixZYSpin->value(), ui.MatrixZZSpin->value());
	v.normalise();
	ui.MatrixZXSpin->setValue(v.x);
	ui.MatrixZYSpin->setValue(v.y);
	ui.MatrixZZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_OrthogonaliseXButton_clicked(bool checked)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.MatrixXXSpin->value(), ui.MatrixXYSpin->value(), ui.MatrixXZSpin->value());
	ref.set(ui.MatrixYXSpin->value(), ui.MatrixYYSpin->value(), ui.MatrixYZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.MatrixXXSpin->setValue(v.x);
	ui.MatrixXYSpin->setValue(v.y);
	ui.MatrixXZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_OrthogonaliseYButton_clicked(bool checked)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.MatrixYXSpin->value(), ui.MatrixYYSpin->value(), ui.MatrixYZSpin->value());
	ref.set(ui.MatrixXXSpin->value(), ui.MatrixXYSpin->value(), ui.MatrixXZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.MatrixYXSpin->setValue(v.x);
	ui.MatrixYYSpin->setValue(v.y);
	ui.MatrixYZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_OrthogonaliseZButton_clicked(bool checked)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.MatrixZXSpin->value(), ui.MatrixZYSpin->value(), ui.MatrixZZSpin->value());
	ref.set(ui.MatrixXXSpin->value(), ui.MatrixXYSpin->value(), ui.MatrixXZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.MatrixZXSpin->setValue(v.x);
	ui.MatrixZYSpin->setValue(v.y);
	ui.MatrixZZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_GenerateXButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.MatrixYXSpin->value(), ui.MatrixYYSpin->value(), ui.MatrixYZSpin->value());
	v2.set(ui.MatrixZXSpin->value(), ui.MatrixZYSpin->value(), ui.MatrixZZSpin->value());
	v = v1 * v2;
	v.normalise();
	ui.MatrixXXSpin->setValue(v.x);
	ui.MatrixXYSpin->setValue(v.y);
	ui.MatrixXZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_GenerateYButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.MatrixXXSpin->value(), ui.MatrixXYSpin->value(), ui.MatrixXZSpin->value());
	v2.set(ui.MatrixZXSpin->value(), ui.MatrixZYSpin->value(), ui.MatrixZZSpin->value());
	v = v1 * v2;
	ui.MatrixYXSpin->setValue(v.x);
	ui.MatrixYYSpin->setValue(v.y);
	ui.MatrixYZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_GenerateZButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.MatrixXXSpin->value(), ui.MatrixXYSpin->value(), ui.MatrixXZSpin->value());
	v2.set(ui.MatrixYXSpin->value(), ui.MatrixYYSpin->value(), ui.MatrixYZSpin->value());
	v = v1 * v2;
	v.normalise();
	ui.MatrixZXSpin->setValue(v.x);
	ui.MatrixZYSpin->setValue(v.y);
	ui.MatrixZZSpin->setValue(v.z);
}

void TransformMultiplyPopup::on_OriginCellCentreButton_clicked(bool checked)
{
	Vec3<double> o;
	if (parent_.aten().currentModelOrFrame()->cell().type() == UnitCell::NoCell) o.set(0.0,0.0,0.0);
	else o = parent_.aten().currentModelOrFrame()->cell().centre();
	ui.OriginXSpin->setValue(o.x);
	ui.OriginYSpin->setValue(o.y);
	ui.OriginZSpin->setValue(o.z);
}

void TransformMultiplyPopup::on_DefineOriginButton_clicked(bool checked)
{
	// Get geometric centre of selection
	Vec3<double> v = parent_.aten().currentModelOrFrame()->selectionCentreOfGeometry();
	// Set widgets
	ui.OriginXSpin->setValue(v.x);
	ui.OriginYSpin->setValue(v.y);
	ui.OriginZSpin->setValue(v.z);
}
