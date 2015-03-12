/*
	*** Font Instance
	*** src/render/fontinstance.cpp
	Copyright T. Youngs 2013-2015

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

#include "render/fontinstance.h"
#include "base/messenger.h"

ATEN_USING_NAMESPACE

// Static Members
QString FontInstance::fontFile_ = "";
FTFont* FontInstance::font_ = NULL;
double FontInstance::fontBaseHeight_ = 0.0;
double FontInstance::fontFullHeight_ = 0.0;
double FontInstance::dotWidth_ = 0.0;

// Setup font specified
bool FontInstance::setupFont(QString fontName)
{
	// If the current font is valid, and matches the name of the new font supplied, do nothing
	if (font_ && (fontFile_ == fontName)) return true;

	if (font_) delete font_;
	font_ = NULL;
	fontFile_ = fontName;

	FTPolygonFont* newFont = new FTPolygonFont(qPrintable(fontName));
	if (newFont->Error())
	{
		printf("Error generating font.\n");
		delete newFont;
		fontBaseHeight_ = 1.0;
		return false;
	}
	else
	{
		font_ = newFont;

		// Request unicode character mapping...
		if (!font_->CharMap(ft_encoding_unicode)) Messenger::print("Failed to set unicode character mapping for font - special characters may not render correctly.");

// 		font_->Depth(3.0);
// 		font_->Outset(-.5, 1.5);
		font_->FaceSize(1);
		FTBBox boundingBox = font_->BBox("0123456789");
		fontBaseHeight_ = boundingBox.Upper().Y() - boundingBox.Lower().Y();
		boundingBox = font_->BBox("ABCDEfghijKLMNOpqrstUVWXYz");
		fontFullHeight_ = boundingBox.Upper().Y() - boundingBox.Lower().Y();
		boundingBox = font_->BBox("..");
		dotWidth_ = boundingBox.Upper().X() - boundingBox.Lower().X();
	}

	return (font_ != NULL);
}

// Return whether font exists and is ready for use
bool FontInstance::fontOK()
{
	return (font_ != NULL);
}

// Return current font
FTFont* FontInstance::font()
{
	return font_;
}

// Return base height of font
double FontInstance::fontBaseHeight()
{
	return fontBaseHeight_;
}

// Return full height of font
double FontInstance::fontFullHeight()
{
	return fontFullHeight_;
}

// Return bounding box for specified string
FTBBox FontInstance::boundingBox(QString text)
{
	if (!font_) return FTBBox();

	// Need to be a little careful here - we will put a '.' either side of the text so we get the full width of strings with trailing spaces..
	FTBBox box = font_->BBox(qPrintable("." + text + "."));
// 	double newWidth = box.Upper().X() - dotWidth_;
// 	box.Upper().X(newWidth);
	return FTBBox(box.Lower(), FTPoint(box.Upper().X()-dotWidth_, box.Upper().Y()));
}

// Calculate bounding box for specified string
void FontInstance::boundingBox(QString text, Vec3<double>& lowerLeft, Vec3<double>& upperRight)
{
	FTBBox box = boundingBox(text);
	lowerLeft.set(box.Lower().X(), box.Lower().Y(), box.Lower().Z());
	upperRight.set(box.Upper().X(), box.Upper().Y(), box.Upper().Z());
}

// Calculate bounding box width for specified string
double FontInstance::boundingBoxWidth(QString text)
{
	FTBBox box = boundingBox(text);
	return (box.Upper().X() - box.Lower().X());
}

// Calculate bounding box height for specified string
double FontInstance::boundingBoxHeight(QString text)
{
	FTBBox box = boundingBox(text);
	return (box.Upper().Y() - box.Lower().Y());
}
