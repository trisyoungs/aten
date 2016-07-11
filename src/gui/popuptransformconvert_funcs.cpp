/*
	*** Popup Widget - Transform Convert Functions
	*** src/gui/popuptransformconvert_funcs.cpp
	Copyright T. Youngs 2007-2016

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

#include "gui/popuptransformconvert.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
TransformConvertPopup::TransformConvertPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformConvertPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformConvertPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "convert")
	{
		// Put values into our matrices...
		Matrix source, target, rotmat;
		source.setColumn(0, ui.SourceMatrixXXSpin->value(), ui.SourceMatrixXYSpin->value(), ui.SourceMatrixXZSpin->value(), 0.0);
		source.setColumn(1, ui.SourceMatrixYXSpin->value(), ui.SourceMatrixYYSpin->value(), ui.SourceMatrixYZSpin->value(), 0.0);
		source.setColumn(2, ui.SourceMatrixZXSpin->value(), ui.SourceMatrixZYSpin->value(), ui.SourceMatrixZZSpin->value(), 0.0);
		target.setColumn(0, ui.TargetMatrixXXSpin->value(), ui.TargetMatrixXYSpin->value(), ui.TargetMatrixXZSpin->value(), 0.0);
		target.setColumn(1, ui.TargetMatrixYXSpin->value(), ui.TargetMatrixYYSpin->value(), ui.TargetMatrixYZSpin->value(), 0.0);
		target.setColumn(2, ui.TargetMatrixZXSpin->value(), ui.TargetMatrixZYSpin->value(), ui.TargetMatrixZZSpin->value(), 0.0);
		// ...and grab coordinate origin
		Vec3<double> v;
		v.set(ui.OriginXSpin->value(), ui.OriginYSpin->value(), ui.OriginZSpin->value());

		CommandNode::run(Commands::MatrixConvert, "ddddddddddddddddddddd", source[0], source[1], source[2], source[4], source[5], source[6], source[8], source[9], source[10], target[0], target[1], target[2], target[4], target[5], target[6], target[8], target[9], target[10], v.x, v.y, v.z);

		parent_.updateWidgets(AtenWindow::AtomsTableTarget);
	}
	else if (methodName == "setSourceXVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for source X axis.");
			return false;
		}
		ui.SourceMatrixXXSpin->setValue(v.x);
		ui.SourceMatrixXYSpin->setValue(v.y);
		ui.SourceMatrixXZSpin->setValue(v.z);
		return true;
	}
	else if (methodName == "setSourceYVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for source Y axis.");
			return false;
		}
		ui.SourceMatrixYXSpin->setValue(v.x);
		ui.SourceMatrixYYSpin->setValue(v.y);
		ui.SourceMatrixYZSpin->setValue(v.z);
		return true;
	}
	else if (methodName == "setSourceZVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for source Z axis.");
			return false;
		}
		ui.SourceMatrixZXSpin->setValue(v.x);
		ui.SourceMatrixZYSpin->setValue(v.y);
		ui.SourceMatrixZZSpin->setValue(v.z);
		return true;
	}
	else if (methodName == "setTargetXVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for target X axis.");
			return false;
		}
		ui.TargetMatrixXXSpin->setValue(v.x);
		ui.TargetMatrixXYSpin->setValue(v.y);
		ui.TargetMatrixXZSpin->setValue(v.z);
		return true;
	}
	else if (methodName == "setTargetYVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for target Y axis.");
			return false;
		}
		ui.TargetMatrixYXSpin->setValue(v.x);
		ui.TargetMatrixYYSpin->setValue(v.y);
		ui.TargetMatrixYZSpin->setValue(v.z);
		return true;
	}
	else if (methodName == "setTargetZVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for target Z axis.");
			return false;
		}
		ui.TargetMatrixZXSpin->setValue(v.x);
		ui.TargetMatrixZYSpin->setValue(v.y);
		ui.TargetMatrixZZSpin->setValue(v.z);
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

void TransformConvertPopup::on_SourcePickXButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.setSelectedMode(UserAction::ConvertSourcePickAAction);

	done();
}

void TransformConvertPopup::on_SourcePickYButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.setSelectedMode(UserAction::ConvertSourcePickBAction);

	done();
}

void TransformConvertPopup::on_SourcePickZButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.setSelectedMode(UserAction::ConvertSourcePickCAction);

	done();
}

void TransformConvertPopup::on_SourceNormaliseXButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.SourceMatrixXXSpin->value(), ui.SourceMatrixXYSpin->value(), ui.SourceMatrixXZSpin->value());
	v.normalise();
	ui.SourceMatrixXXSpin->setValue(v.x);
	ui.SourceMatrixXYSpin->setValue(v.y);
	ui.SourceMatrixXZSpin->setValue(v.z);
}

void TransformConvertPopup::on_SourceNormaliseYButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.SourceMatrixYXSpin->value(), ui.SourceMatrixYYSpin->value(), ui.SourceMatrixYZSpin->value());
	v.normalise();
	ui.SourceMatrixYXSpin->setValue(v.x);
	ui.SourceMatrixYYSpin->setValue(v.y);
	ui.SourceMatrixYZSpin->setValue(v.z);
}

void TransformConvertPopup::on_SourceNormaliseZButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.SourceMatrixZXSpin->value(), ui.SourceMatrixZYSpin->value(), ui.SourceMatrixZZSpin->value());
	v.normalise();
	ui.SourceMatrixZXSpin->setValue(v.x);
	ui.SourceMatrixZYSpin->setValue(v.y);
	ui.SourceMatrixZZSpin->setValue(v.z);
}

void TransformConvertPopup::on_SourceOrthogonaliseXButton_clicked(bool checked)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.SourceMatrixXXSpin->value(), ui.SourceMatrixXYSpin->value(), ui.SourceMatrixXZSpin->value());
	ref.set(ui.SourceMatrixYXSpin->value(), ui.SourceMatrixYYSpin->value(), ui.SourceMatrixYZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.SourceMatrixXXSpin->setValue(v.x);
	ui.SourceMatrixXYSpin->setValue(v.y);
	ui.SourceMatrixXZSpin->setValue(v.z);
}

void TransformConvertPopup::on_SourceOrthogonaliseYButton_clicked(bool checked)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.SourceMatrixYXSpin->value(), ui.SourceMatrixYYSpin->value(), ui.SourceMatrixYZSpin->value());
	ref.set(ui.SourceMatrixXXSpin->value(), ui.SourceMatrixXYSpin->value(), ui.SourceMatrixXZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.SourceMatrixYXSpin->setValue(v.x);
	ui.SourceMatrixYYSpin->setValue(v.y);
	ui.SourceMatrixYZSpin->setValue(v.z);
}

void TransformConvertPopup::on_SourceOrthogonaliseZButton_clicked(bool checked)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.SourceMatrixZXSpin->value(), ui.SourceMatrixZYSpin->value(), ui.SourceMatrixZZSpin->value());
	ref.set(ui.SourceMatrixXXSpin->value(), ui.SourceMatrixXYSpin->value(), ui.SourceMatrixXZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.SourceMatrixZXSpin->setValue(v.x);
	ui.SourceMatrixZYSpin->setValue(v.y);
	ui.SourceMatrixZZSpin->setValue(v.z);
}

void TransformConvertPopup::on_SourceGenerateXButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.SourceMatrixYXSpin->value(), ui.SourceMatrixYYSpin->value(), ui.SourceMatrixYZSpin->value());
	v2.set(ui.SourceMatrixZXSpin->value(), ui.SourceMatrixZYSpin->value(), ui.SourceMatrixZZSpin->value());
	v = v1 * v2;
	v.normalise();
	ui.SourceMatrixXXSpin->setValue(v.x);
	ui.SourceMatrixXYSpin->setValue(v.y);
	ui.SourceMatrixXZSpin->setValue(v.z);
}

void TransformConvertPopup::on_SourceGenerateYButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.SourceMatrixXXSpin->value(), ui.SourceMatrixXYSpin->value(), ui.SourceMatrixXZSpin->value());
	v2.set(ui.SourceMatrixZXSpin->value(), ui.SourceMatrixZYSpin->value(), ui.SourceMatrixZZSpin->value());
	v = v1 * v2;
	ui.SourceMatrixYXSpin->setValue(v.x);
	ui.SourceMatrixYYSpin->setValue(v.y);
	ui.SourceMatrixYZSpin->setValue(v.z);
}

void TransformConvertPopup::on_SourceGenerateZButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.SourceMatrixXXSpin->value(), ui.SourceMatrixXYSpin->value(), ui.SourceMatrixXZSpin->value());
	v2.set(ui.SourceMatrixYXSpin->value(), ui.SourceMatrixYYSpin->value(), ui.SourceMatrixYZSpin->value());
	v = v1 * v2;
	v.normalise();
	ui.SourceMatrixZXSpin->setValue(v.x);
	ui.SourceMatrixZYSpin->setValue(v.y);
	ui.SourceMatrixZZSpin->setValue(v.z);
}

void TransformConvertPopup::on_OriginCellCentreButton_clicked(bool checked)
{
	Vec3<double> o;
	if (parent_.aten().currentModelOrFrame()->cell().type() == UnitCell::NoCell) o.set(0.0,0.0,0.0);
	else o = parent_.aten().currentModelOrFrame()->cell().centre();
	ui.OriginXSpin->setValue(o.x);
	ui.OriginYSpin->setValue(o.y);
	ui.OriginZSpin->setValue(o.z);
}

void TransformConvertPopup::on_DefineOriginButton_clicked(bool checked)
{
	// Get geometric centre of selection
	Vec3<double> v = parent_.aten().currentModelOrFrame()->selectionCentreOfGeometry();
	// Set widgets
	ui.OriginXSpin->setValue(v.x);
	ui.OriginYSpin->setValue(v.y);
	ui.OriginZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetPickXButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.setSelectedMode(UserAction::ConvertTargetPickAAction);

	done();
}

void TransformConvertPopup::on_TargetPickYButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.setSelectedMode(UserAction::ConvertTargetPickBAction);

	done();
}

void TransformConvertPopup::on_TargetPickZButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.setSelectedMode(UserAction::ConvertTargetPickCAction);

	done();
}

void TransformConvertPopup::on_TargetNormaliseXButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TargetMatrixXXSpin->value(), ui.TargetMatrixXYSpin->value(), ui.TargetMatrixXZSpin->value());
	v.normalise();
	ui.TargetMatrixXXSpin->setValue(v.x);
	ui.TargetMatrixXYSpin->setValue(v.y);
	ui.TargetMatrixXZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetNormaliseYButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TargetMatrixYXSpin->value(), ui.TargetMatrixYYSpin->value(), ui.TargetMatrixYZSpin->value());
	v.normalise();
	ui.TargetMatrixYXSpin->setValue(v.x);
	ui.TargetMatrixYYSpin->setValue(v.y);
	ui.TargetMatrixYZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetNormaliseZButton_clicked(bool checked)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TargetMatrixZXSpin->value(), ui.TargetMatrixZYSpin->value(), ui.TargetMatrixZZSpin->value());
	v.normalise();
	ui.TargetMatrixZXSpin->setValue(v.x);
	ui.TargetMatrixZYSpin->setValue(v.y);
	ui.TargetMatrixZZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetOrthogonaliseXButton_clicked(bool checked)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.TargetMatrixXXSpin->value(), ui.TargetMatrixXYSpin->value(), ui.TargetMatrixXZSpin->value());
	ref.set(ui.TargetMatrixYXSpin->value(), ui.TargetMatrixYYSpin->value(), ui.TargetMatrixYZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.TargetMatrixXXSpin->setValue(v.x);
	ui.TargetMatrixXYSpin->setValue(v.y);
	ui.TargetMatrixXZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetOrthogonaliseYButton_clicked(bool checked)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.TargetMatrixYXSpin->value(), ui.TargetMatrixYYSpin->value(), ui.TargetMatrixYZSpin->value());
	ref.set(ui.TargetMatrixXXSpin->value(), ui.TargetMatrixXYSpin->value(), ui.TargetMatrixXZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.TargetMatrixYXSpin->setValue(v.x);
	ui.TargetMatrixYYSpin->setValue(v.y);
	ui.TargetMatrixYZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetOrthogonaliseZButton_clicked(bool checked)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.TargetMatrixZXSpin->value(), ui.TargetMatrixZYSpin->value(), ui.TargetMatrixZZSpin->value());
	ref.set(ui.TargetMatrixXXSpin->value(), ui.TargetMatrixXYSpin->value(), ui.TargetMatrixXZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	ui.TargetMatrixZXSpin->setValue(v.x);
	ui.TargetMatrixZYSpin->setValue(v.y);
	ui.TargetMatrixZZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetGenerateXButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TargetMatrixYXSpin->value(), ui.TargetMatrixYYSpin->value(), ui.TargetMatrixYZSpin->value());
	v2.set(ui.TargetMatrixZXSpin->value(), ui.TargetMatrixZYSpin->value(), ui.TargetMatrixZZSpin->value());
	v = v1 * v2;
	v.normalise();
	ui.TargetMatrixXXSpin->setValue(v.x);
	ui.TargetMatrixXYSpin->setValue(v.y);
	ui.TargetMatrixXZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetGenerateYButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TargetMatrixXXSpin->value(), ui.TargetMatrixXYSpin->value(), ui.TargetMatrixXZSpin->value());
	v2.set(ui.TargetMatrixZXSpin->value(), ui.TargetMatrixZYSpin->value(), ui.TargetMatrixZZSpin->value());
	v = v1 * v2;
	ui.TargetMatrixYXSpin->setValue(v.x);
	ui.TargetMatrixYYSpin->setValue(v.y);
	ui.TargetMatrixYZSpin->setValue(v.z);
}

void TransformConvertPopup::on_TargetGenerateZButton_clicked(bool checked)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TargetMatrixXXSpin->value(), ui.TargetMatrixXYSpin->value(), ui.TargetMatrixXZSpin->value());
	v2.set(ui.TargetMatrixYXSpin->value(), ui.TargetMatrixYYSpin->value(), ui.TargetMatrixYZSpin->value());
	v = v1 * v2;
	v.normalise();
	ui.TargetMatrixZXSpin->setValue(v.x);
	ui.TargetMatrixZYSpin->setValue(v.y);
	ui.TargetMatrixZZSpin->setValue(v.z);
}
