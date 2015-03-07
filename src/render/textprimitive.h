/*
	*** Text Primitive
	*** src/render/textprimitive.h
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

#ifndef ATEN_TEXTPRIMITIVE_H
#define ATEN_TEXTPRIMITIVE_H

#include "render/textfragment.h"
#include "render/textformat.h"
#include "math/matrix.h"
#include "templates/vector3.h"
#include "templates/list.h"
#include <QtCore/QString>
#include "base/namespace.h"


// External Declarations
extern int TextPrimitiveParser_parse();

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

// Text Primitive
class TextPrimitive : public ListItem<TextPrimitive>
{
	public:
	// Constructor / Destructor
	TextPrimitive();
	~TextPrimitive();
	// Text Anchors enum
	enum TextAnchor { TopLeftAnchor, TopMiddleAnchor, TopRightAnchor, MiddleLeftAnchor, CentralAnchor, MiddleRightAnchor, BottomLeftAnchor, BottomMiddleAnchor, BottomRightAnchor, nTextAnchors };
	// Convert text string to TextAnchor
	static TextAnchor textAnchor(QString s);
	// Convert TextAnchor to text string
	static const char* textAnchor(TextAnchor anchor);
	// Escape Sequence enum
	enum EscapeSequence { BoldEscape, ItalicEscape, NewLineEscape, SubScriptEscape, SuperScriptEscape, nEscapeSequences };
	// Convert text string to EscapeSequence
	static EscapeSequence escapeSequence(QString s);


	/*
	 * Definition
	 */
	private:
	// General text scaling factor
	static double textSizeScale_;
	// Coordinates of anchorpoint of text
	Vec3<double> anchorPoint_;
	// Location of anchorpoint on text bounding box
	TextAnchor anchorPosition_;
	// Vector by which to adjust position of text
	Vec3<double> adjustmentVector_;
	// Local transform matrix for the text
	Matrix localRotation_;
	// Text size
	double textSize_;
	// Text fragments to render
	List<TextFragment> fragments_;

	public:
	// Set text scaling factor
	static void setTextSizeScale(double textSizeScale);
	// Set data
	void set(QString text, Vec3<double> anchorPoint, TextAnchor anchorPosition, Vec3<double> adjustmentVector, Matrix rotation, double textSize);
	// Return transformation matrix to use when rendering (including fragment scale/translation if one is specified)
	Matrix transformationMatrix(double baseFontSize, TextFragment* fragment = NULL);
	// Calculate bounding box of primitive
	void boundingBox(Vec3<double>& lowerLeft, Vec3<double>& upperRight);
	// Render primitive
	void render(Matrix viewMatrix, bool correctOrientation, double baseFontSize);


	/*
	 * Generation
	 */
	private:
	// Character string source
	static QString stringSource_;
	// Integer position in stringSource, total length of string, and starting position of current token/function
	static int stringPos_, stringLength_;
	// Get next character from current input stream
	static QChar getChar();
	// Peek next character from current input stream
	static QChar peekChar();
	// 'Replace' last character read from current input stream
	static void unGetChar();
	// Current target for generation
	static TextPrimitive* target_;
	// Format stack, used when generating primitive
	static List<TextFormat> formatStack_;
	// Current horizontal position, used when generating primitive
	static double horizontalPosition_;

	public:
	// Parser lexer, called by yylex()
	static int lex();
	// Generate TextFragment data for specified TextPrimitive from supplied string
	static bool generateFragments(TextPrimitive* target, QString inputString);
	// Return current target
	static TextPrimitive* target();
	// Add text fragment
	bool addFragment(QString text);
	// Add escape marker
	static bool addEscape(TextPrimitive::EscapeSequence escSeq);
	// Remove escape marker
	static void removeEscape();
};

ATEN_END_NAMESPACE

#endif
