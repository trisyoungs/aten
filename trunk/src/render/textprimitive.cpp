/*
	*** Text Primitive
	*** src/render/textprimitive.cpp
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

#include "render/textprimitive.h"
#include "render/textprimitive_grammar.hh"
#include "render/fontinstance.h"

ATEN_USING_NAMESPACE

// Static members
TextPrimitive* TextPrimitive::target_ = NULL;
QString TextPrimitive::stringSource_;
int TextPrimitive::stringPos_, TextPrimitive::stringLength_;
List<TextFormat> TextPrimitive::formatStack_;
double TextPrimitive::horizontalPosition_;
double TextPrimitive::textSizeScale_ = 1.0;

// Constructor
TextPrimitive::TextPrimitive() : ListItem<TextPrimitive>()
{
}

// Destructor
TextPrimitive::~TextPrimitive()
{
}

// Text Anchor Keywords
const char* TextAnchorKeywords[] = { "TopLeft", "TopMiddle", "TopRight", "MiddleLeft", "Central", "MiddleRight", "BottomLeft", "BottomMiddle", "BottomRight" };

// Convert text string to TextAnchor
TextPrimitive::TextAnchor TextPrimitive::textAnchor(QString s)
{
	for (int n=0; n<TextPrimitive::nTextAnchors; ++n) if (s == TextAnchorKeywords[n]) return (TextPrimitive::TextAnchor) n;
	return TextPrimitive::nTextAnchors;
}

// Convert TextAnchor to text string
const char* TextPrimitive::textAnchor(TextPrimitive::TextAnchor anchor)
{
	return TextAnchorKeywords[anchor];
}

// Escape Sequence Keywords
const char* EscapeSequenceKeywords[] = { "b", "it", "n", "sub", "sup" };

// Convert text string to EscapeSequence
TextPrimitive::EscapeSequence TextPrimitive::escapeSequence(QString s)
{
	for (int n=0; n<TextPrimitive::nEscapeSequences; ++n) if (s == EscapeSequenceKeywords[n]) return (TextPrimitive::EscapeSequence) n;
	return TextPrimitive::nEscapeSequences;
}

// Set text scaling facotor
void TextPrimitive::setTextSizeScale(double textSizeScale)
{
	textSizeScale_ = textSizeScale;
}

// Set data
void TextPrimitive::set(QString text, Vec3< double > anchorPoint, TextPrimitive::TextAnchor anchorPosition, Vec3< double > adjustmentVector, Matrix rotation, double textSize)
{
	// Call the parser
	generateFragments(this, text);

	anchorPoint_ = anchorPoint;
	anchorPosition_ = anchorPosition;
	adjustmentVector_ = adjustmentVector;
	localRotation_ = rotation;
	textSize_ = textSize;
}

// Return transformation matrix to use when rendering the text
Matrix TextPrimitive::transformationMatrix(double baseFontSize, TextFragment* fragment)
{
	Matrix textMatrix, A;
	Vec3<double> lowerLeft, upperRight, anchorPos, anchorPosRotated, textCentre;

	// Calculate scaling factor for font
	double scale = FontInstance::fontBaseHeight() * textSizeScale_ * textSize_ / baseFontSize;
	
	// Calculate bounding box and anchor position on it
	boundingBox(lowerLeft, upperRight);
	switch (anchorPosition_)
	{
		case (TextPrimitive::TopLeftAnchor):
			anchorPos.set(lowerLeft.x, upperRight.y, 0.0);
			break;
		case (TextPrimitive::TopMiddleAnchor):
			anchorPos.set((lowerLeft.x+upperRight.x)*0.5, upperRight.y, 0.0);
			break;
		case (TextPrimitive::TopRightAnchor):
			anchorPos = upperRight;
			break;
		case (TextPrimitive::MiddleLeftAnchor):
			anchorPos.set(lowerLeft.x, (lowerLeft.y+upperRight.y)*0.5, 0.0);
			break;
		case (TextPrimitive::CentralAnchor):
			anchorPos.set((lowerLeft.x+upperRight.x)*0.5, (lowerLeft.y+upperRight.y)*0.5, 0.0);
			break;
		case (TextPrimitive::MiddleRightAnchor):
			anchorPos.set(upperRight.x, (lowerLeft.y+upperRight.y)*0.5, 0.0);
			break;
		case (TextPrimitive::BottomLeftAnchor):
			anchorPos = lowerLeft;
			break;
		case (TextPrimitive::BottomMiddleAnchor):
			anchorPos.set((lowerLeft.x+upperRight.x)*0.5, lowerLeft.y, 0.0);
			break;
		case (TextPrimitive::BottomRightAnchor):
			anchorPos.set(upperRight.x, lowerLeft.y, 0.0);
			break;
		default:
			break;
	}

	// Rotate anchor position with local rotation matrix
	textCentre = (lowerLeft + upperRight) * 0.5;
	anchorPosRotated = localRotation_ * (anchorPos - textCentre) * scale;

	// Construct matrix
	// -- Translate to centre of text bounding box (not rotated) accounting for fragment translation if one was specified
	if (fragment) textCentre -= fragment->translation();
	textMatrix.createTranslation(-textCentre);
	// -- Apply scaled local rotation matrix
	A = localRotation_;
	A.applyScaling(scale, scale, scale);
	textMatrix *= A;
	// -- Apply translation to text anchor point
	textMatrix.applyTranslation(-anchorPosRotated+anchorPoint_+adjustmentVector_*scale);
	// -- Apply fragment specific operations
	if (fragment)
	{
		// -- Apply local scaling to text (if fragment was provided)
		textMatrix.applyScaling(fragment->scale());
		// -- Apply local shear to text (if fragment is italic)
		if (fragment->italic()) textMatrix.applyShearX(0.2);
	}

	return textMatrix;
}

// Calculate bounding box of primitive
void TextPrimitive::boundingBox(Vec3<double>& lowerLeft, Vec3<double>& upperRight)
{
	// Set initial lowerLeft and upperRight from the first primitive in the list
	if (fragments_.first()) FontInstance::boundingBox(fragments_.first()->text(), lowerLeft, upperRight);
	else
	{
		// No fragments in list!
		lowerLeft.zero();
		upperRight.zero();
		return;
	}
	
	// Loop over remaining fragments, keeping track of the total width of the primitive and the max/min y values
	Vec3<double> ll, ur;
// 	double width = upperRight.x - lowerLeft.x;
	for (TextFragment* fragment = fragments_.first()->next; fragment != NULL; fragment = fragment->next)
	{
		// Get bounding box for this fragment
		FontInstance::boundingBox(fragment->text(), ll, ur);

		// Scale the box by the current scaling factor...
		ur.x = ll.x + (ur.x - ll.x)*fragment->scale();
		ur.y = ll.y + (ur.y - ll.y)*fragment->scale();

		// Translate the box by the defined amount
		ll += fragment->translation();
		ur += fragment->translation();

		// Update lowerLeft and upperRight values
		if (ll.y < lowerLeft.y) lowerLeft.y = ll.y;
		if (ur.y > upperRight.y) upperRight.y = ur.y;
		if (ur.x > upperRight.x) upperRight.x = ur.x;
	}
}

// Render primitive
void TextPrimitive::render(Matrix viewMatrix, bool correctOrientation, double baseFontSize)
{
	Matrix textMatrix;

	// Loop over fragments
	for (TextFragment* fragment = fragments_.first(); fragment != NULL; fragment = fragment->next)
	{
		textMatrix = transformationMatrix(baseFontSize, fragment) * viewMatrix;
		glLoadMatrixd(textMatrix.matrix());

		// Draw bounding boxes around each fragment
		if (false)
		{
			glDisable(GL_LINE_STIPPLE);
			glLineWidth(1.0);
			Vec3<double> ll, ur;
			FontInstance::boundingBox(fragment->text(), ll, ur);
			glBegin(GL_LINE_LOOP);
			glVertex3d(ll.x, ll.y, 0.0);
			glVertex3d(ur.x, ll.y, 0.0);
			glVertex3d(ur.x, ur.y, 0.0);
			glVertex3d(ll.x, ur.y, 0.0);
			glEnd();
		}

		if (fragment->bold())
		{
			// Render the text twice - once with lines, and once with polygon fill
			glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
			FontInstance::font()->Render(qPrintable(fragment->text()));
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			FontInstance::font()->Render(qPrintable(fragment->text()));
		}
		else FontInstance::font()->Render(qPrintable(fragment->text()));
	}
}

/*
 * Generation
 */

// Bison-generated ExpressionParser_lex()
int TextPrimitiveParser_lex()
{
	if (!TextPrimitive::target()) return 0;
	return TextPrimitive::target()->lex();
}

// Get next character from current input stream
QChar TextPrimitive::getChar()
{
	// Are we at the end of the current string?
	if (stringPos_ == stringLength_) return 0;

	// Return current char and increment index
	return stringSource_.at(stringPos_++);
}

// Peek next character from current input stream
QChar TextPrimitive::peekChar()
{
	return (stringPos_ == stringLength_ ? 0 : stringSource_.at(stringPos_));
}

// 'Replace' last character read from current input stream
void TextPrimitive::unGetChar()
{
	--stringPos_;
}

// Parser lexer, called by yylex()
int TextPrimitive::lex()
{
	bool done, isEscape;
	static QString token;
	QChar c;

	// Reset some variables
	token.clear();
	isEscape = false;
	done = false;

	do
	{
		c = getChar();
		if (c == QChar(0))
		{
			if (token.length() == 0) return 0;
			done = true;
			break;
		}
		else if (c == QChar('\\'))
		{
			if (token.length() == 0) isEscape = true;
			else
			{
				unGetChar();
				done = true;
			}
		}
		else if (c == QChar('{'))
		{
			if (token.length() == 0) return '{';
			unGetChar();
			done = true;
		}
		else if (c == QChar('}'))
		{
			if (token.length() == 0) return '}';
			unGetChar();
			done = true;
		}
		else token += c;

		// Break out if we are finished
		if (done) break;
	} while (c != 0);

	// Did we find an escape sequence, or just normal text?
	if (isEscape)
	{
		// Is the text a recognised escape?
		TextPrimitive::EscapeSequence es = TextPrimitive::escapeSequence(qPrintable(token));
		if (es == TextPrimitive::nEscapeSequences)
		{
			Messenger::print(Messenger::Verbose, "Error: String '%s' is not a valid escape sequence.", qPrintable(token));
			return UCR_TP_FAIL;
		}
		TextPrimitiveParser_lval.escSeq = es;
		return UCR_TP_ESCAPE;
	}
	else
	{
		TextPrimitiveParser_lval.text = &token;
		return UCR_TP_TEXT;
	}

	return 0;
}

// Generate TextFragment data for specified TextPrimitive from supplied string
bool TextPrimitive::generateFragments(TextPrimitive* target, QString inputString)
{
	// Set / reset variables
	target_ = target;
	stringPos_ = 0;
	stringSource_ = inputString;
	stringLength_ = stringSource_.length();

	// Clear the format stack and create a basic format
	formatStack_.clear();
	formatStack_.add();
	horizontalPosition_ = 0.0;

	return (TextPrimitiveParser_parse() == 0);
}

// Return current target
TextPrimitive* TextPrimitive::target()
{
	return target_;
}

// Add text fragment
bool TextPrimitive::addFragment(QString text)
{
	TextFragment* fragment = fragments_.add();
	if (formatStack_.nItems() == 0)
	{
		Messenger::print("Internal Error: No TextFormat on stack in TextPrimitive::addFragment().");
		fragment->set(text);
		return false;
	}

	// Get topmost TextFormat
	TextFormat* format = formatStack_.last();

	// Set fragment info
	Vec3<double> translation(horizontalPosition_, format->y(), 0.0); 
	fragment->set(text, format->scale(), translation, format->italic(), format->bold());
	
	// We have just added some text, so update the horizontal position
	horizontalPosition_ += FontInstance::boundingBoxWidth(text) * format->scale();

	return true;
}

// Add escape code
bool TextPrimitive::addEscape(TextPrimitive::EscapeSequence escSeq)
{
	// Copy topmost formatting node first, since we retain any previously-set (i.e. nested) formats
	TextFormat* topMostFormat = formatStack_.last();
	TextFormat* newFormat = formatStack_.add();
	if (topMostFormat) (*newFormat) = (*topMostFormat);
	else Messenger::print("Internal Error: No topmost TextFormat to copy from in TextPrimitive::addEscape().");

	// Deal with the escape sequence
	switch (escSeq)
	{
		// Add bold level
		case (TextPrimitive::BoldEscape):
			newFormat->setBold(true);
			break;
		// Add italic level
		case (TextPrimitive::ItalicEscape):
			newFormat->setItalic(true);
			break;
		// Newline
		case (TextPrimitive::NewLineEscape):
// 			newFormat->		TODO
			break;
		// Add subscript level - adjust baseline position and scale of current format
		case (TextPrimitive::SubScriptEscape):
			newFormat->adjustY( -FontInstance::fontBaseHeight() * newFormat->scale() * (1.0/3.0) );
			newFormat->setScale( 0.583 * newFormat->scale() );
			break;
		// Add superscript level - adjust baseline position and scale of current format
		case (TextPrimitive::SuperScriptEscape):
			newFormat->adjustY( FontInstance::fontBaseHeight() * newFormat->scale() * (2.0/3.0) );
			newFormat->setScale( 0.583 * newFormat->scale() );
			break;
		default:
			Messenger::print("Escape %i not handled in TextPrimitive::addEscape().", escSeq);
			return false;
			break;
	}

	return true;
}


// Remove escape code
void TextPrimitive::removeEscape()
{
	formatStack_.removeLast();
}
