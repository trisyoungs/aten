/*
	*** Line Parsing Routines
	*** src/base/lineparser.cpp
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

#include "base/lineparser.h"
#include "base/sysfunc.h"
#include "math/mathfunc.h"
#include "math/constants.h"
#include "base/messenger.h"
#include <string.h>
#include <stdarg.h>

ATEN_USING_NAMESPACE

// Parse options
const char* ParseOptionKeywords[LineParser::nParseOptions] = { "stripcomments", "usequotes", "skipblanks", "stripbrackets", "noescapes", "usecurlies", "normalcommas" };
LineParser::ParseOption LineParser::parseOption(QString s)
{
	return (LineParser::ParseOption) (1 << enumSearch("line parser option", LineParser::nParseOptions, ParseOptionKeywords, s));
}

// Constructors
LineParser::LineParser()
{
	reset();
}

// Destructor
LineParser::~LineParser()
{
	if (inputFile_ != NULL) delete inputFile_;
	if (outputFile_ != NULL) delete outputFile_;
	if (cachedFile_ != NULL) delete cachedFile_;
}

// Reset data
void LineParser::reset()
{
	inputFilename_.clear();
	outputFilename_.clear();
	endOfLine_ = FALSE;
	lineLength_ = 0;
	linePos_ = 0;
	lastLineNo_ = 0;
	inputFile_ = NULL;
	outputFile_ = NULL;
	cachedFile_ = NULL;
	directOutput_ = FALSE;
}

/*
// Source line/file and read options
*/

// Return filename of current input file (if any)
QString LineParser::inputFilename() const
{
	return inputFilename_;
}

// Return filename of current output file (if any)
QString LineParser::outputFilename() const
{
	return outputFilename_;
}

// Return pointer to current line
QString LineParser::line() const
{
	return line_;
}

// Set line target
void LineParser::setLine(QString s)
{
	line_ = s;
	lineLength_ = line_.length();
	linePos_ = 0;
}

// Return integer line number of last read line
int LineParser::lastLineNo() const
{
	return lastLineNo_;
}

// Open new file for reading
bool LineParser::openInput(QString filename)
{
	Messenger::enter("LineParser::openInput");

	// Check existing input file
	if (inputFile_ != NULL)
	{
		printf("Warning - LineParser already appears to have an open file...\n");
		inputFile_->close();
		delete inputFile_;
		inputFile_ = NULL;
	}

	// Open new file
	inputFile_ = new std::ifstream(qPrintable(filename), std::ios::in | std::ios::binary);
	if (!inputFile_->is_open())
	{
		closeFiles();
		Messenger::print("Error: Failed to open file '%s' for reading.", qPrintable(filename));
		Messenger::exit("LineParser::openInput");
		return FALSE;
	}

	// Reset variables
	lastLineNo_ = 0;
	inputFilename_ = filename;

	Messenger::exit("LineParser::openInput");
	return TRUE;
}

// Open new stream for writing
bool LineParser::openOutput(QString filename, bool directOutput)
{
	Messenger::enter("LineParser::openOutput");

	// Check existing input file
	if ((outputFile_ != NULL) || (cachedFile_ != NULL))
	{
		printf("Warning - LineParser already appears to have an open file/cache...\n");
		if (outputFile_ != NULL)
		{
			outputFile_->close();
			delete outputFile_;
			outputFile_ = NULL;
		}
		if (cachedFile_ != NULL)
		{
			delete cachedFile_;
			cachedFile_ = NULL;
		}
	}

	// Open new file
	directOutput_ = directOutput;
	if (directOutput_)
	{
		outputFile_ = new std::ofstream(qPrintable(filename), std::ios::out);
		if (!outputFile_->is_open())
		{
			closeFiles();
			Messenger::print("Error: Failed to open file '%s' for writing.", qPrintable(filename));
			Messenger::exit("LineParser::openOutput");
			return FALSE;
		}
	}
	else cachedFile_ = new std::stringstream;
	outputFilename_ = filename;

	Messenger::exit("LineParser::openOutput");
	return TRUE;
}

// Close file 
void LineParser::closeFiles()
{
	if (inputFile_ != NULL)
	{
		inputFile_->close();
		delete inputFile_;
	}
	if (outputFile_ != NULL)
	{
		outputFile_->close();
		delete outputFile_;
	}
	reset();
}

// Return whether current file source is good for reading
bool LineParser::isFileGoodForReading() const
{
	if (inputFile_ == NULL) return FALSE;
	else if (!inputFile_->is_open()) return FALSE;
	return TRUE;
}

// Return whether current file source is good for writing
bool LineParser::isFileGoodForWriting() const
{
	if (directOutput_)
	{
		if (outputFile_ == NULL) return FALSE;
		else if (!outputFile_->is_open()) return FALSE;
		return TRUE;
	}
	else return TRUE;
}

// Peek next character in input stream
char LineParser::peek() const
{
	if (inputFile_ == NULL) return '\0';
	return inputFile_->peek();
}

// Tell current position of input stream
std::streampos LineParser::tellg() const
{
	std::streampos result = 0;
	if (inputFile_ != NULL) result = inputFile_->tellg();
	else printf("Warning: LineParser tried to tellg() on a non-existent input file.\n");
	return result;
}

// Seek position in input stream
void LineParser::seekg(std::streampos pos)
{
	if (inputFile_ != NULL)
	{
		if (inputFile_->eof()) inputFile_->clear();
		inputFile_->seekg(pos);
	}
	else printf("Warning: LineParser tried to seekg() on a non-existent input file.\n");
}

// Seek n bytes in specified direction in input stream
void LineParser::seekg(std::streamoff off, std::ios_base::seekdir dir)
{
	if (inputFile_ != NULL) inputFile_->seekg(off, dir);
	else printf("Warning: LineParser tried to seekg() on a non-existent input file.\n");
}

// Rewind input stream to start
void LineParser::rewind()
{
	if (inputFile_ != NULL) inputFile_->seekg(0, std::ios::beg);
	else Messenger::print("No file currently open to rewind.");
}

// Return whether the end of the input stream has been reached (or only whitespace remains)
bool LineParser::eofOrBlank() const
{
	if (inputFile_ == NULL) return TRUE;
	// Simple check first - is this the end of the file?
	if (inputFile_->eof()) return TRUE;
	// Otherwise, store the current file position and search for a non-whitespace character (or end of file)
	std::streampos pos = inputFile_->tellg();
	
	// Skip through whitespace, searching for 'hard' character
	char c;
	bool result = TRUE;
	do
	{
		inputFile_->get(c);
		if (inputFile_->eof()) break;
		// If a whitespace character then skip it....
		if ((c == ' ') || (c == '\r') || ( c == '\n') || (c == '\t') || (c == '\0'))
		{
			if (inputFile_->eof()) break;
			else continue;
		}
		result = FALSE;
		break;
	} while (1);
	inputFile_->seekg(pos);
	
	return result;
}

/*
// Read/Write Routines
*/

// Read single line from internal file source
int LineParser::readNextLine(int optionMask)
{
	Messenger::enter("LineParser::readNextLine");

	// Reset line contents
	lineLength_ = 0;
	line_.clear();

	// Returns : 0=ok, 1=error, -1=eof
	if (inputFile_ == NULL)
	{
		printf("Error: No input file open for LineParser::readNextLine.\n");
		Messenger::exit("LineParser::readNextLine");
		return 1;
	}
	if (inputFile_->eof())
	{
		Messenger::exit("LineParser::readNextLine");
		return -1;
	}
	
	// Loop until we get 'suitable' line from file
	int nSpaces, result = 0;
	do
	{
		char chr;
		while(inputFile_->get(chr).good())
		{
			if (chr == '\r')
			{
				if (inputFile_->peek() == '\n') inputFile_->ignore();
				break;
			}
			else if (chr == '\n') break;
			line_ += chr;
			++lineLength_;

			// Check here for overfilling the line_ buffer - perhaps it's a binary file?
			if (lineLength_ == MAXLINELENGTH)
			{
				Messenger::exit("LineParser::readNextLine");
				return -1;
			}
		}
		Messenger::print(Messenger::Parse, "Line from file is: [%s]", qPrintable(line_));

		// Remove comments from line
		if (optionMask&LineParser::StripComments) removeComments(line_);
		
		// If we are skipping blank lines, check for a blank line here
		if (optionMask&LineParser::SkipBlanks)
		{
			// Now, see if our line contains only blanks
			nSpaces = 0;
			for (int n=0; n<line_.length(); ++n) if (line_.at(n).isSpace()) ++nSpaces;
			if (line_.length() == nSpaces) result = -1;
			else result = 0;
		}
		else result = 0;
		
		// If result is 0, everything went okay, but if not we got a blank line. EOF or failed perhaps?
		if (result == -1)
		{
			if (inputFile_->eof())
			{
				Messenger::exit("LineParser::readLine");
				return -1;
			}
			if (inputFile_->fail())
			{
				Messenger::exit("LineParser::readLine");
				return 1;
			}
		}
		lineLength_ = line_.length();
		linePos_ = 0;
		++lastLineNo_;
	} while (result != 0);
// 	printf("LineParser Returned line = [%s], length = %i",line_,lineLength_);
	Messenger::exit("LineParser::readNextLine");
	return result;
}

// Gets next delimited arg from internal line
bool LineParser::getNextArg(int optionMask, QString& destArg)
{
	Messenger::enter("LineParser::getNextArg");
	bool done = false, hadQuotes = false, failed = false;
	char c, quoteChar = '\0';
	endOfLine_ = FALSE;

	// Clear destination argument
	destArg.clear();

	if (endOfLine_)
	{
// 		printf("Lineparser is at end of line - returning...\n");
		return TRUE;
	}

	while (linePos_ < lineLength_)
	{
		c = line_.at(linePos_).toAscii();
		switch (c)
		{
			// End of line markers
			case (10):	// Line feed (\n)
			case (13):	// Carriage Return
				done = TRUE;
				endOfLine_ = TRUE;
				break;
			// Delimiters
			// If we encounter one and arg length != 0 this signals the end of the argument.
			case (','):	// Comma
				if (optionMask&LineParser::NormalCommas)
				{
					destArg += c;
					break;
				}
			case (9):	// Horizontal Tab
			case (' '):	// Space
				if (quoteChar != '\0') destArg += c;
				else if (!destArg.isEmpty()) done = TRUE;
				break;
			// Quote marks
			// If LineParser::UseQuotes, keep delimiters and other quote marks inside the quoted text.
			case (34):	// Double quotes
			case (39):	// Single quotes
				if (!(optionMask&LineParser::UseQuotes)) break;
				if (quoteChar == '\0') quoteChar = c;
				else if (quoteChar == c)
				{
					quoteChar = '\0';
					hadQuotes = TRUE;
					done = TRUE;
				}
				else destArg += c;
				break;
			// Curly brackets - treat in the same way as quotes
			case ('{'):
			case ('}'):
				// If explicitly not useing braces, add as normal character
				if (!(optionMask&LineParser::UseCurlies)) destArg += c;
				else
				{
					// If the quoteChar is a left brace and we have a right brace, stop quoting
					if ((quoteChar == '{') && (c == '}'))
					{
						quoteChar = '\0';
						break;
					}
					// If we are already quoting by some other means, add character and exit
					if (quoteChar != '\0') destArg += c;
					// No previous quoting, so begin quoting if '{'
					if (c == '{') quoteChar = '{';
				}
				break;
			// Parentheses
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optionMask&LineParser::StripBrackets) break;
				destArg += c;
				break;
			// Comment markers
			case ('#'):	// "#" Rest/all of line is a comment
				endOfLine_ = TRUE;
				done = TRUE;
				break;
			// Normal character
			default: 
				destArg += c;
				break;
		}
		// Increment line position
		++linePos_;
		if (done || failed) break;
	}

	// Check for end of the input line
	if (linePos_ == lineLength_) endOfLine_ = TRUE;

	Messenger::exit("LineParser::getNextArg");
	if (failed) return FALSE;
	return (destArg.isEmpty() ? (hadQuotes ? TRUE : FALSE) : TRUE);
}

// Rip next n characters
bool LineParser::getNextN(int optionMask, int length, QString& destArg)
{
	Messenger::enter("LineParser::getNextN");

	// Clear destination argument
	destArg.clear();

	// Put the next 'length' characters from line_ into temparg (and put into supplied arg if supplied)
	// A negative length may be supplied, which we interpret as 'strip trailing spaces'
	if (lineLength_ == 0)
	{
		Messenger::exit("LineParser::getNextN");
		return FALSE;
	}

	int n, charsLeft = lineLength_ - linePos_;
	bool stripTrailing = (length < 0);
	length = abs(length);
	if (length > charsLeft) length = charsLeft;
	//if (length > lineLength_) length = lineLength_;
	for (n=0; n<length; ++n)
	{
		switch (line_.at(n).toAscii())
		{
			// Brackets
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optionMask&LineParser::StripBrackets) break;
				destArg += line_.at(n);
				break;
			default:
				destArg += line_.at(n);
				break;
		}
		++linePos_;
	}

	// Strip trailing spaces from destination argument
	if (stripTrailing) for (n = destArg.length()-1; (destArg.at(n) == ' ') || (destArg.at(n) == '\t'); --n) destArg.chop(1); 

	Messenger::exit("LineParser::getNextN");
	return TRUE;
}

// Get all arguments (delimited) from LineParser::line_
void LineParser::getAllArgsDelim(int optionMask)
{
	// Parse the string in 'line_' into arguments in 'args'
	Messenger::enter("LineParser::getAllArgsDelim");
	arguments_.clear();
	endOfLine_ = FALSE;
	QString arg;
	while (!endOfLine_)
	{
		// Clear our argument
		arg.clear();

		// We must pass on the current optionMask, else it will be reset by the default value in getNextArg()
		if (getNextArg(optionMask, arg))
		{
			Messenger::print(Messenger::Parse,"getAllArgsDelim arg=%i [%s]", arguments_.count(), qPrintable(arg));
			arguments_ << arg;
		}
	}
	Messenger::exit("LineParser::getAllArgsDelim");
}

/*
// Delimited Parsing Routines
*/

// Parse delimited (from file)
int LineParser::getArgsDelim(int optionMask)
{
	Messenger::enter("LineParser::getArgsDelim[ifstream]");
	bool done = FALSE;
	int result;
	// Returns : 0=ok, 1=error, -1=eof
	do
	{
		// Read line from file and parse it
		result = readNextLine(optionMask);
		if (result != 0)
		{
			Messenger::exit("LineParser::getArgsDelim[ifstream]");
			return result;
		}
		// Assume that we will finish after parsing the line we just read in
		done = TRUE;
		// To check for blank lines, do the parsing and then check nargs()
		getAllArgsDelim(optionMask);
		if ((optionMask&LineParser::SkipBlanks) && (nArgs() == 0)) done = FALSE;
	} while (!done);
	Messenger::exit("LineParser::getArgsDelim[ifstream]");
	return 0;
}

// Get rest of current line starting at next delimited part (and put into destination argument if supplied)
bool LineParser::getRestDelim(QString& destArg)
{
	Messenger::enter("LineParser::getRestDelim");

	int n, length;
	char c;
	if (lineLength_ == 0)
	{
		Messenger::exit("LineParser::getRestDelim");
		return FALSE;
	}
	length = lineLength_ - linePos_;
	for (n=0; n<length; n++)
	{
		c = line_.at(linePos_).toAscii();
		switch (c)
		{
			// Ignore whitespace occuring before first proper character
			case (' '):
			case ('\0'):
				if (!destArg.isEmpty()) destArg += c;
				break;
			default:
				destArg += c;
				break;
		}
		linePos_ ++;
	}

	// Strip whitespace from start and end of destArg if there is any...
	destArg = destArg.trimmed();

	Messenger::exit("LineParser::getRestDelim");
	return TRUE;
}

// Get next argument (delimited) from file stream
bool LineParser::getArgDelim(int optionMask, QString& destArg)
{
	Messenger::enter("LineParser::getArgDelim");

	bool result = getNextArg(optionMask, destArg);
	Messenger::print(Messenger::Parse,"getArgDelim = %s [%s]", result ? "TRUE" : "FALSE", qPrintable(destArg));

	Messenger::exit("LineParser::getArgDelim");
	return result;
}

// Parse all arguments (delimited) from string
void LineParser::getArgsDelim(int optionMask, QString line)
{
	line_ = line;
	lineLength_ = line_.length();
	linePos_ = 0;
	getAllArgsDelim(optionMask);
}

// Get next delimited chunk from input stream (not line)
bool LineParser::getCharsDelim(QString& destArg)
{
	bool result = TRUE;
	char c;
	destArg.clear();
	while (!inputFile_->eof())
	{
		inputFile_->get(c);
		if ((c == '\n') || (c == '\t') || (c == '\r') || (c == ' '))
		{
			// Eat DOS-style line terminator
			if ((c == '\r') && (inputFile_->peek() == '\n')) inputFile_->get(c);
			if (!destArg.isEmpty()) break;
			else continue;
		}
		if (c == '\0')
		{
			if (destArg.isEmpty()) result = FALSE;
			break;
		}
		destArg += c;
	}
	return result;
}

// Get next delimited chunk from string, removing grabbed part
bool LineParser::getCharsDelim(int optionMask, QString& line, QString& destArg)
{
	Messenger::enter("LineParser::getCharsDelim(int,QString,QString)");

	int pos = 0, length = line.length();
	bool done = false, hadQuotes = false, failed = false;
	char c, quoteChar = '\0';
	destArg.clear();

	while (pos < length)
	{
		c = line.at(pos).toAscii();
		switch (c)
		{
			// End of line markers
			case (10):	// Line feed (\n)
			case (13):	// Carriage Return
				done = TRUE;
				break;
			// Delimiters
			// If we encounter one and arg length != 0 this signals the end of the argument.
			case (9):	// Horizontal Tab
			case (' '):	// Space
			case (','):	// Comma
				if (quoteChar != '\0') destArg += c;
				else if (!destArg.isEmpty()) done = TRUE;
				break;
			// Quote marks
			// If LineParser::UseQuotes, keep delimiters and other quote marks inside the quoted text.
			case (34):	// Double quotes
			case (39):	// Single quotes
				if (!(optionMask&LineParser::UseQuotes)) break;
				if (quoteChar == '\0') quoteChar = c;
				else if (quoteChar == c)
				{
					quoteChar = '\0';
					hadQuotes = TRUE;
					done = TRUE;
				}
				else destArg += c;
				break;
			// Curly brackets - treat in the same way as quotes
			case ('{'):
			case ('}'):
				// If not using curlies, just add as a normal character
				if (!(optionMask&LineParser::UseCurlies)) destArg += c;
				else
				{
					// If the quoteChar is a left brace and we have a right brace, stop quoting
					if ((quoteChar == '{') && (c == '}'))
					{
						quoteChar = '\0';
						break;
					}

					// If we are already quoting by some other means, add character and exit
					if (quoteChar != '\0') destArg += c;

					// No previous quoting, so begin quoting if '{'
					if (c == '{') quoteChar = '{';
				}
				break;
			// Parentheses
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optionMask&LineParser::StripBrackets) break;
				destArg += c;
				break;
			// Comment markers
			case ('#'):	// "#" Rest/all of line is a comment
				endOfLine_ = TRUE;
				done = TRUE;
				break;
			// Normal character
			default: 
				destArg += c;
				break;
		}
		// Increment line position
		pos ++;
		if (done || failed) break;
	}

	// Trim characters from source string
	line.remove(0, pos);

	Messenger::exit("LineParser::getCharsDelim(int,QString,QString)");
	if (failed) return FALSE;
	return (destArg.isEmpty() ? (hadQuotes ? TRUE : FALSE) : TRUE);
}

// Return a number of characters from the input stream
QString LineParser::getChars(int nChars, bool skipeol)
{
	QString result;
	char c;
	// Check number of characters requested
	int i = 0;
	if (nChars == 0) return NULL;
	else if (nChars > MAXLINELENGTH)
	{
		Messenger::print("Error: The maximum number of characters read at once from a file is currently %i.", MAXLINELENGTH);
		return QString();
	}
	else if (nChars < 0)
	{
		for (int i= nChars; i<0; i++) inputFile_->unget();
	}
	else for (i=0; i < nChars; ++i)
	{
		inputFile_->get(c);
		if (skipeol) while ((c == '\n') || (c == '\r'))
		{
			if (inputFile_->eof()) break;
			inputFile_->get(c);
		}
		result += c;
		if (inputFile_->eof()) break;
	}

	if (inputFile_->eof())
	{
// 		closeFile();
		return NULL;
	}
	if (inputFile_->fail())
	{
// 		closeFile();
		return NULL;
	}
	
	return result;
}

// Skip a number of characters from the input stream
void LineParser::skipChars(int nchars)
{
	if (nchars == 0) return;
	inputFile_->ignore(nchars);
// 	if (file_->eof() || file_->fail()) closeFile();
}

// Return an integer value from reading 'n' chars of an (unformatted) input stream
int LineParser::getInteger(int nbytes)
{
	// Use default size if none specified
	if (nbytes == 0)
	{
		int readi;
		inputFile_->read((char*) &readi, sizeof(int));
		return readi;
	}
	// Try and select a suitable type for the read, based on nbytes
	if (sizeof(short int) == nbytes)
	{
		short int readi;
		inputFile_->read((char*) &readi, nbytes);
		return (int) readi;
	}
	else if (sizeof(int) == nbytes)
	{
		int readi;
		inputFile_->read((char*) &readi, nbytes);
		return readi;
	}
	else if (sizeof(long int) == nbytes)
	{
		long int readi;
		inputFile_->read((char*) &readi, nbytes);
		return (int) readi;
	}
	else Messenger::print("Error: Integer of size %i bytes does not correspond to any internal type.", nbytes);
	return 0;
}

// Read an array of integer values from an (unformatted) input file
int LineParser::getIntegerArray(int *array, int count)
{
	inputFile_->read((char*) array, count*sizeof(int));
	if (inputFile_->eof())
	{
// 		closeFile();
		return -1;
	}
	if (inputFile_->fail())
	{
// 		closeFile();
		return 1;
	}
	return 0;
}

// Return a double value from reading 'n' chars of an (unformatted) input file
double LineParser::getDouble(int nbytes)
{
	// Use default size if none specified
	if (nbytes == 0)
	{
		double readd;
		inputFile_->read((char*) &readd, sizeof(double));
		return readd;
	}
	// Try and select a suitable type for the read, based on nbytes
	if (sizeof(double) == nbytes)
	{
		double readd;
		inputFile_->read((char*) &readd, nbytes);
		return readd;
	}
	else if (sizeof(long double) == nbytes)
	{
		long double readd;
		inputFile_->read((char*) &readd, nbytes);
		return (double) readd;
	}
	else Messenger::print("Error: Double of size %i bytes does not correspond to any internal type.", nbytes);
	return 0.0;
}

// Read an array of double values from an (unformatted) input file
int LineParser::getDoubleArray(double* array, int count)
{
	inputFile_->read((char*) array, count*sizeof(double));
	if (inputFile_->eof())
	{
// 		closeFile();
		return -1;
	}
	if (inputFile_->fail())
	{
// 		closeFile();
		return 1;
	}
	return 0;
}

// Write line to file
bool LineParser::writeLine(QString s)
{
	Messenger::enter("LineParser::writeLine");
	if (!directOutput_)
	{
		if (cachedFile_ == NULL)
		{
			Messenger::print("Unable to delayed-writeLine - destination cache is not open.");
			Messenger::exit("LineParser::writeLine");
			return FALSE;
		}
		else *cachedFile_ << qPrintable(s);
	}
	else if (outputFile_ == NULL)
	{
		Messenger::print("Unable to direct-writeLine - destination file is not open.");
		Messenger::exit("LineParser::writeLine");
		return FALSE;
	}
	else *outputFile_ << qPrintable(s);
	Messenger::exit("LineParser::writeLine");
	return TRUE;
}

// Write formatted line to file
bool LineParser::writeLineF(const char* fmt, ...)
{
	Messenger::enter("LineParser::writeLine");
	if (!directOutput_)
	{
		if (cachedFile_ == NULL)
		{
			Messenger::print("Unable to delayed-writeLineF - destination cache is not open.");
			Messenger::exit("LineParser::writeLineF");
			return FALSE;
		}
	}
	else if (outputFile_ == NULL)
	{
		Messenger::print("Unable to direct-writeLineF - destination file is not open.");
		Messenger::exit("LineParser::writeLineF");
		return FALSE;
	}
	
	// Construct line
	va_list arguments;
	static char s[8096];
	s[0] = '\0';
	// Parse the argument list (...) and internally write the output string into s[]
	va_start(arguments,fmt);
	vsprintf(s,fmt,arguments);
	va_end(arguments);
	if (directOutput_) *outputFile_ << s;
	else *cachedFile_ << s;
	Messenger::exit("LineParser::writeLineF");
	return TRUE;
}

// Commit cached output stream to actual output file
bool LineParser::commitCache()
{
	// Were we using cached writing?
	if (directOutput_)
	{
		printf("Internal Error: Tried to commit cached writes when direct output was enabled.\n");
		return FALSE;
	}
	std::ofstream outputFile(qPrintable(outputFilename_));
	if (outputFile.is_open())
	{
		outputFile << cachedFile_->str();
		outputFile.close();
	}
	else
	{
		Messenger::print("Error: Couldn't open output file '%s' for writing.", qPrintable(outputFilename_));
		return FALSE;
	}
	return TRUE;
}

// Skip lines from file
int LineParser::skipLines(int nlines)
{
	Messenger::enter("LineParser::skipLines");
	int result;
	for (int n=0; n<nlines; n++)
	{
		result = readNextLine(0);
		if (result != 0)
		{
			Messenger::exit("LineParser::skipLines");
			return result;
		}
	}
	Messenger::exit("LineParser::skipLines");
	return 0;
}

/*
// Argument Data
*/

// Returns number of arguments grabbed from last parse
int LineParser::nArgs() const
{
	return arguments_.count();
}

// Returns the specified argument as a character string
QString LineParser::argc(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning \"NULL\"...\n", i);
		return "NULL";
	}
	return arguments_.at(i);
}

// Returns the specified argument as an integer
int LineParser::argi(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning 0...\n", i);
		return 0;
	}
	return arguments_.at(i).toInt();
}

// Returns the specified argument as a double
double LineParser::argd(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning 0.0...\n", i);
		return 0.0;
	}
	return arguments_.at(i).toDouble();
}

// Returns the specified argument as a bool
bool LineParser::argb(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning FALSE...\n", i);
		return FALSE;
	}
	QString lower = arguments_.at(i).toLower();
	if (lower == "off") return false;
	else if (lower == "on") return true;
	else if (lower == "no") return false;
	else if (lower == "yes") return true;
	else if (lower == "false") return false;
	else if (lower == "true") return true;
	return false;
}

// Returns the specified argument as a float
float LineParser::argf(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning 0.0f...\n", i);
		return 0.0f;
	}
	return (float) argd(i);
}

// Returns whether the specified argument exists (has been provided)
bool LineParser::hasArg(int i) const
{
	if ((i < 0) || (i >= nArgs())) return FALSE;
	return TRUE;
}
