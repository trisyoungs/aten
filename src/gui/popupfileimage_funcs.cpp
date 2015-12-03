/*
	*** Popup Widget - File Image
	*** src/gui/popupfileimage_funcs.cpp
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

#include "gui/popupfileimage.h"
// #include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include <QClipboard>

ATEN_USING_NAMESPACE

// Constructor
FileImagePopup::FileImagePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void FileImagePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool FileImagePopup::callMethod(QString methodName, ReturnValue& rv)
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

void FileImagePopup::on_CopyToClipboardButton_clicked(bool checked)
{
	// Generate image of current view
	QPixmap pixmap = parent_.ui.MainView->generateImage();

	// Copy image to clipboard
        QClipboard* clipboard = QApplication::clipboard();
        clipboard->setImage(pixmap.toImage());

	// Hide popup
	done();
}

