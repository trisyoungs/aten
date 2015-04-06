/*
	*** Glyphs Dock Widget
	*** src/gui/glyphs.h
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

#ifndef ATEN_GLYPHSWINDOW_H
#define ATEN_GLYPHSWINDOW_H

#include "gui/ui_glyphs.h"
#include "math/constants.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Glyph;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Glyphs dock widget
class GlyphsWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	GlyphsWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::GlyphsWidget ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Local variables and functions
	 */
	private:
	// Local widget pointers
	QRadioButton *dataAtomRadio[MAXGLYPHDATA], *dataValueRadio[MAXGLYPHDATA];
	QSpinBox *dataAtomIdSpin[MAXGLYPHDATA];
	QComboBox* dataAtomDataCombo[MAXGLYPHDATA];
	QDoubleSpinBox* dataValueXSpin[MAXGLYPHDATA], *dataValueYSpin[MAXGLYPHDATA], *dataValueZSpin[MAXGLYPHDATA];
	TColourFrame *dataColourFrame[MAXGLYPHDATA];
	QWidget *dataTabWidget[MAXGLYPHDATA];
	QWidget *dataAtomWidget[MAXGLYPHDATA], *dataValueWidget[MAXGLYPHDATA];
	// Whether the widget should refresh when it is next shown
	bool shouldRefresh_;
	// Whether widget is refreshing
	bool refreshing_;
	// Update current glyph data
	void updateData(Glyph* g);
	// Update controls (i.e. enable/disable widgets in data groups)
	void updateControls(Glyph* g);


	/*
	 * Window Functions
	 */
	public:
	// Show the widget
	void showWidget();
	// Update widget
	void refresh();

	private slots:
	void on_GlyphList_currentRowChanged(int row);
	void on_GlyphList_itemSelectionChanged();
	void on_GlyphTypeCombo_currentIndexChanged(int row);
	void on_GlyphLineEdit_returnPressed();
	void on_GlyphVisibleCheck_clicked(bool checked);
	void on_GlyphAddButton_clicked(bool checked);
	void on_GlyphDeleteSelectedButton_clicked(bool checked);
	void on_GlyphSelectAllButton_clicked(bool checked);
	void on_GlyphSelectNoneButton_clicked(bool checked);
	void on_GlyphInvertSelectionButton_clicked(bool checked);
	void on_GlyphHideAllButton_clicked(bool checked);
	void on_GlyphHideNoneButton_clicked(bool checked);
	void on_GlyphHideSelectedButton_clicked(bool checked);
	void on_Data1AtomIdSpin_valueChanged(int i);
	void on_Data2AtomIdSpin_valueChanged(int i);
	void on_Data3AtomIdSpin_valueChanged(int i);
	void on_Data4AtomIdSpin_valueChanged(int i);
	void on_Data1AtomRadio_clicked(bool checked);
	void on_Data2AtomRadio_clicked(bool checked);
	void on_Data3AtomRadio_clicked(bool checked);
	void on_Data4AtomRadio_clicked(bool checked);
	void on_Data1ValueRadio_clicked(bool checked);
	void on_Data2ValueRadio_clicked(bool checked);
	void on_Data3ValueRadio_clicked(bool checked);
	void on_Data4ValueRadio_clicked(bool checked);
	void on_Data1ValueXSpin_valueChanged(double d);
	void on_Data2ValueXSpin_valueChanged(double d);
	void on_Data3ValueXSpin_valueChanged(double d);
	void on_Data4ValueXSpin_valueChanged(double d);
	void on_Data1ValueYSpin_valueChanged(double d);
	void on_Data2ValueYSpin_valueChanged(double d);
	void on_Data3ValueYSpin_valueChanged(double d);
	void on_Data4ValueYSpin_valueChanged(double d);
	void on_Data1ValueZSpin_valueChanged(double d);
	void on_Data2ValueZSpin_valueChanged(double d);
	void on_Data3ValueZSpin_valueChanged(double d);
	void on_Data4ValueZSpin_valueChanged(double d);
	void on_Data1ColourButton_clicked(bool checked);
	void on_Data2ColourButton_clicked(bool checked);
	void on_Data3ColourButton_clicked(bool checked);
	void on_Data4ColourButton_clicked(bool checked);

	private:
	// Add item to list
	void addItemToList(Glyph* g);
	void dataAtomIdChanged(int id, int value);
	void dataValueChanged(int id, int component, double value);
	void dataValueChanged(int id, double x, double y, double z);
	void dataColourChanged(int id);

	protected:
	void closeEvent(QCloseEvent* event);
};

#endif
