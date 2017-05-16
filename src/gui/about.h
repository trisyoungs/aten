/*
	*** About Dialog
	*** src/gui/about.h
	Copyright T. Youngs 2013-2017

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

#ifndef ATEN_ATENABOUT_H
#define ATEN_ATENABOUT_H

#include "gui/ui_about.h"
#include "base/namespace.h"
#include <QDialog>

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class PluginStore;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Forward Declarations (Qt)
class AtenWindow;

class AtenAbout : public QDialog
{
	// All Qt declarations must include this macro
	Q_OBJECT

	private:
	// Main form declaration
	Ui::AboutDialog ui;
	// Reference to main window
	AtenWindow& atenWindow_;
	// Reference to plugin store
	const PluginStore& pluginStore_;

	public:
	// Constructor / Destructor
	AtenAbout(AtenWindow& parent);
	~AtenAbout();


	/*
	 * Slots
	 */
	private slots:
	void updateTree(int type);
	void on_CloseButton_clicked(bool checked);
};

#endif
