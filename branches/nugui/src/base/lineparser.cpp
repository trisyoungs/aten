/*
	*** Line Parsing Routines
	*** src/base/lineparser.cpp
	Copyright T. Youngs 2007-2011

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
#include "base/mathfunc.h"
#include "base/constants.h"
#include "base/messenger.h"
#include <string.h>
#include <stdarg.h>

// Parse options
const char *ParseOptionKeywords[LineParser::nParseOptions] = { "defaults", "usequotes", "skipblanks", "stripbrackets", "noescapes", "usecurlies" };
LineParser::ParseOption LineParser::parseOption(const char *s)
{
	return (LineParser::ParseOption) (1 << enumSearch("line parser option", LineParser::nParseOptions, ParseOptionKeywords, s));
}

// Constructors
LineParser::LineParser()
{
	// Private variables
	endOfLine_ = FALSE;
	lineLength_ = 0;
	readOnly_ = FALSE;
	linePos_ = 0;
	lastLineNo_ = 0;
	file_ = NULL;
}
LineParser::LineParser(const char *filename, bool outputstream)
{
	// Private variables
	reset();
	// Open new file for reading or writing
	openFile(filename, outputstream);
}

// Destructor
LineParser::~LineParser()
{
	if (file_ != NULL) delete file_;
}

// Reset data
void LineParser::reset()
{
	filename_.clear();
	endOfLine_ = FALSE;
	lineLength_ = 0;
	linePos_ = 0;
	lastLineNo_ = 0;
	file_ = NULL;
	readOnly_ = FALSE;
}

/*
// Source line/file and read options
*/

// Return filename of opened (or recently closed) file
const char *LineParser::filename() const
{
	return filename_.get();
}

// Return pointer to current line
const char *LineParser::line() const
{
	return line_;
}

// Set line target
void LineParser::setLine(const char *s)
{
	strncpy(line_, s, MAXLINELENGTH);
	lineLength_ = strlen(line_);
	linePos_ = 0;
}

// Return integer line number of last read line
int LineParser::lastLineNo() const
{
	return lastLineNo_;
}

// Return read-only status of file
bool LineParser::isFileReadOnly() const
{
	return readOnly_;
}

// Open file for parsing
bool LineParser::openFile(const char *filename, bool outputstream)
{
	msg.enter("LineParser::openFile");
	// Check existing input file
	if (file_ != NULL)
	{
		printf("Warning - LineParser already appears to have an open file...\n");
		file_->close();
		delete file_;
		file_ = NULL;
	}
	// Open new file
	if (outputstream) file_ = new fstream(filename, ios::out);
	else file_ = new fstream(filename, ios::in | ios::binary);
	if (!file_->is_open())
	{
		closeFile();
		msg.print("Error: Failed to open file '%s' for %s.\n", filename, outputstream ? "writing" : "reading");
		msg.exit("LineParser::openFile");
		return FALSE;
	}
	// Reset variables
	readOnly_ = outputstream ? FALSE : TRUE;
	lastLineNo_ = 0;
	filename_ = filename;
	msg.exit("LineParser::openFile");
	return TRUE;
}

// Close file 
void LineParser::closeFile()
{
	if (file_ != NULL)
	{
		file_->close();
		delete file_;
	}
	file_ = NULL;
	lastLineNo_ = 0;
}

// Return whether current file source is good for reading/writing
bool LineParser::isFileGood() const
{
	if (file_ == NULL) return FALSE;
	return file_->is_open();
}

// Return whether current file source is good for reading
bool LineParser::isFileGoodForReading() const
{
	if (file_ == NULL) return FALSE;
	if (!file_->is_open()) return FALSE;
	return readOnly_;
}

// Return whether current file source is good for writing
bool LineParser::isFileGoodForWriting() const
{
	if (file_ == NULL) return FALSE;
	if (!file_->is_open()) return FALSE;
	return (!readOnly_);
}

// Peek next character in file
char LineParser::peek() const
{
	if (file_ == NULL) return '\0';
	return file_->peek();
}

// Tell current position of file stream
streampos LineParser::tellg() const
{
	streampos result = 0;
	if (file_ != NULL) result = file_->tellg();
	else printf("Warning: LineParser tried to tellg() on a non-existent file.\n");
	return result;
}

// Seek position in file
void LineParser::seekg(streampos pos)
{
	if (file_ != NULL)
	{
		if (file_->eof()) file_->clear();
		file_->seekg(pos);
	}
	else printf("Warning: LineParser tried to seekg() on a non-existent file.\n");
}

// Seek n bytes in specified direction
void LineParser::seekg(streamoff off, ios_base::seekdir dir)
{
	if (file_ != NULL) file_->seekg(off, dir);
	else printf("Warning: LineParser tried to seekg() on a non-existent file.\n");
}

// Rewind file to start
void LineParser::rewind()
{
	if (file_ != NULL) file_->seekg(0, ios::beg);
	else msg.print("No file currently open to rewind.\n");
}

// Return whether the end of the file has been reached (or only whitespace remains)
bool LineParser::eofOrBlank() const
{
	if (file_ == NULL) return TRUE;
	// Simple check first - is this the end of the file?
	if (file_->eof()) return TRUE;
	// Otherwise, store the current file position and search for a non-whitespace character (or end of file)
	streampos pos = file_->tellg();
	
	// Skip through whitespace, searching for 'hard' character
	char c;
	bool result = TRUE;
	do
	{
		file_->get(c);
		if (file_->eof()) break;
		// If a whitespace character then skip it....
		if ((c == ' ') || (c == '\r') || ( c == '\n') || (c == '\t') || (c == '\0'))
		{
			if (file_->eof()) break;
			else continue;
		}
		result = FALSE;
		break;
	} while (1);
	file_->seekg(pos);
	
	return result;
}

/*
// Read/Write Routines
*/

// Read single line from internal file source
int LineParser::readNextLine(int optionMask)
{
	msg.enter("LineParser::readNextLine");
	// Returns : 0=ok, 1=error, -1=eof
	if (file_ == NULL)
	{
		printf("Attempted to readLine from a NULL file in LineParser.\n");
		msg.exit("LineParser::readNextLine");
		return 1;
	}
	if (file_->eof())
	{
		msg.exit("LineParser::readLine");
		return -1;
	}
	// Loop until we get 'suitable' line from file
	int nchars, nspaces, result = 0;
	do
	{
		char chr;
		lineLength_ = 0;
		while(file_->get(chr).good())
		{
			if (chr == '\r')
			{
				if (file_->peek() == '\n') file_->ignore();
				break;
			}
			else if (chr == '\n') break;
			line_[lineLength_++] = chr;
		}
		line_[lineLength_] = '\0';
		msg.print(Messenger::Parse, "Line from file is: [%s]\n", line_);

		// Remove comments from line
		removeComments(line_);
		
		// If we are skipping blank lines, check for a blank line here
		if (optionMask&LineParser::SkipBlanks)
		{
			// Now, see if our line contains only blanks
			nchars = 0;
			nspaces = 0;
			for (char *c = line_; *c != '\0'; c++)
			{
				nchars++;
				if (isspace(*c)) nspaces++;
			}
			if (nchars == nspaces) result = -1;
			else result = 0;
		}
		else result = 0;
		
		// If result is 0, everything went okay, but if not we got a blank line. EOF or failed perhaps?
		if (result == -1)
		{
			if (file_->eof())
			{
				msg.exit("LineParser::readLine");
				return -1;
			}
			if (file_->fail())
			{
				msg.exit("LineParser::readLine");
				return 1;
			}
		}
		lineLength_ = strlen(line_);
		linePos_ = 0;
		lastLineNo_ ++;
	} while (result != 0);
// 	printf("LineParser Returned line = [%s], length = %i\n",line_,lineLength_);
	msg.exit("LineParser::readNextLine");
	return result;
}

// Gets next delimited arg from internal line
bool LineParser::getNextArg(int optionMask, Dnchar* destarg)
{
	// Get the next input chunk from the internal string and put into argument specified.
	msg.enter("LineParser::getNextArg");
	int arglen;
	bool done, hadquotes, failed;
	char c, quotechar;
	failed = FALSE;
	done = FALSE;
	hadquotes = FALSE;
	quotechar = '\0';
	endOfLine_ = FALSE;
	arglen = 0;
	if (endOfLine_)
	{
		destarg->clear();
		printf("Lineparser is at end of line - returning...\n");
		return TRUE;
	}
	while (linePos_ < lineLength_)
	{
		c = line_[linePos_];
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
			case (9):	// Horizontal Tab
			case (' '):	// Space
			case (','):	// Comma
				if (quotechar != '\0')
				{
					tempArg_[arglen] = c;
					arglen ++;
				}
				else if (arglen != 0) done = TRUE;
				break;
			// Quote marks
			// If LineParser::UseQuotes, keep delimiters and other quote marks inside the quoted text.
			case (34):	// Double quotes
			case (39):	// Single quotes
				if (!(optionMask&LineParser::UseQuotes)) break;
				if (quotechar == '\0') quotechar = c;
				else if (quotechar == c)
				{
					quotechar = '\0';
					hadquotes = TRUE;
					done = TRUE;
				}
				else
				{
					tempArg_[arglen] = c;
					arglen ++;
				}
				break;
			// Curly brackets - treat in the same way as quotes
			case ('{'):
			case ('}'):
				if (!(optionMask&LineParser::UseBraces))
				{
					// Just add as normal character
					tempArg_[arglen] = c;
					arglen ++;
				}
				else
				{
					// If the quotechar is a left brace and we have a right brace, stop quoting
					if ((quotechar == '{') && (c == '}'))
					{
						quotechar = '\0';
						break;
					}
					// If we are already quoting by some other means, add character and exit
					if (quotechar != '\0')
					{
						tempArg_[arglen] = c;
						arglen ++;
					}
					// No previous quoting, so begin quoting if '{'
					if (c == '{') quotechar = '{';
				}
				break;
			// Parentheses
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optionMask&LineParser::StripBrackets) break;
				tempArg_[arglen] = c;
				arglen ++;
				break;
			// Comment markers
			case ('#'):	// "#" Rest/all of line is a comment
				endOfLine_ = TRUE;
				done = TRUE;
				break;
			// Normal character
			default: 
				tempArg_[arglen] = c;
				arglen ++;
				break;
		}
		// Increment line position
		linePos_++;
		if (done || failed) break;
	}
	// Finalise argument
	tempArg_[arglen] = '\0';
	if (linePos_ == lineLength_) endOfLine_ = TRUE;
	// Store the result in the desired destination
	if (destarg != NULL) *destarg = tempArg_;
	msg.exit("LineParser::getNextArg");
	if (failed) return FALSE;
	return (arglen == 0 ? (hadquotes ? TRUE : FALSE) : TRUE);
}

// Rip next n characters
bool LineParser::getNextN(int optionMask, int length, Dnchar* destarg)
{
	// Put the next 'length' characters from line_ into temparg (and put into supplied arg if supplied)
	// A negative length may be supplied, which we interpret as 'strip trailing spaces'
	msg.enter("LineParser::getNextN");
	int arglen = 0;
	char c;
	if (lineLength_ == 0)
	{
		msg.exit("LineParser::getNextN");
		return FALSE;
	}
	int n, charsleft = lineLength_ - linePos_;
	bool striptrailing = (length < 0);
	length = abs(length);
	if (length > charsleft) length = charsleft;
	//if (length > lineLength_) length = lineLength_;
	for (n=0; n<length; n++)
	{
		c = line_[linePos_];
		switch (c)
		{
			// Brackets
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optionMask&LineParser::StripBrackets) break;
				tempArg_[arglen] = c;
				arglen ++;
				break;
			default:
				tempArg_[arglen] = c;
				arglen ++;
				break;
		}
		linePos_ ++;
	}
	// Add terminating character to temparg
	tempArg_[arglen] = '\0';
	if (striptrailing) for (n = arglen-1; (tempArg_[n] == ' ') || (tempArg_[n] == '\t'); --n) tempArg_[n] = '\0'; 
	if (destarg != NULL) destarg->set(tempArg_);
	//printf("getNextN found [%s], length = %i\n", tempArg_, arglen);
	//line_.eraseStart(length);
	msg.exit("LineParser::getNextN");
	return TRUE;
}

// Get all arguments (delimited) from LineParser::line_
void LineParser::getAllArgsDelim(int optionMask)
{
	// Parse the string in 'line_' into arguments in 'args'
	msg.enter("LineParser::getAllArgsDelim");
	arguments_.clear();
	endOfLine_ = FALSE;
	Dnchar *arg;
	while (!endOfLine_)
	{
		// Create new, empty dnchar
		arg = new Dnchar;
		// We must pass on the current optionMask, else it will be reset by the default value in getNextArg()
		if (getNextArg(optionMask, arg))
		{
			msg.print(Messenger::Parse,"getAllArgsDelim arg=%i [%s]\n", arguments_.nItems(), arg->get());
			// Add this char to the list
			arguments_.own(arg);
		}
		else delete arg;
	}
	msg.exit("LineParser::getAllArgsDelim");
}

/*
// Delimited Parsing Routines
*/

// Parse delimited (from file)
int LineParser::getArgsDelim(int optionMask)
{
	msg.enter("LineParser::getArgsDelim[ifstream]");
	bool done = FALSE;
	int result;
	// Returns : 0=ok, 1=error, -1=eof
	do
	{
		// Read line from file and parse it
		result = readNextLine(optionMask);
		if (result != 0)
		{
			msg.exit("LineParser::getArgsDelim[ifstream]");
			return result;
		}
		// Assume that we will finish after parsing the line we just read in
		done = TRUE;
		// To check for blank lines, do the parsing and then check nargs()
		getAllArgsDelim(optionMask);
		if ((optionMask&LineParser::SkipBlanks) && (nArgs() == 0)) done = FALSE;
	} while (!done);
	msg.exit("LineParser::getArgsDelim[ifstream]");
	return 0;
}

// Get rest of current line starting at next delimited part (and put into supplied arg if supplied)
bool LineParser::getRestDelim(int optionMask, Dnchar* destarg)
{
	msg.enter("LineParser::getRestDelim");
	int arglen = 0, n, length;
	char c;
	if (lineLength_ == 0)
	{
		msg.exit("LineParser::getRestDelim");
		return FALSE;
	}
	length = lineLength_ - linePos_;
	for (n=0; n<length; n++)
	{
		c = line_[linePos_];
		switch (c)
		{
			// Ignore whitespace occuring before first proper character
			case (' '):
			case ('\0'):
				if (arglen != 0) tempArg_[arglen++] = c;
				break;
			default:
				tempArg_[arglen++] = c;
				break;
		}
		linePos_ ++;
	}
	// Add terminating character to temparg - strip whitespace at end if there is any...
	tempArg_[arglen] = '\0';
	for (n=arglen-1; n>0; --n)
	{
		if ((tempArg_[n] != ' ') && (tempArg_[n] != '\t')) break;
		tempArg_[n] = '\0';
	}
	if (destarg != NULL) destarg->set(tempArg_);
	msg.exit("LineParser::getRestDelim");
	return TRUE;
}

// Get next argument (delimited) from file stream
bool LineParser::getArgDelim(int optionMask, Dnchar* destarg)
{
	msg.enter("LineParser::getArgDelim");
	bool result = getNextArg(optionMask, destarg);
	msg.print(Messenger::Parse,"getArgDelim = %s [%s]\n", result ? "TRUE" : "FALSE", destarg->get());
	msg.exit("LineParser::getArgDelim");
	return result;
}

// Parse all arguments (delimited) from string
void LineParser::getArgsDelim(int optionMask, const char* s)
{
	strcpy(line_,s);
	lineLength_ = strlen(line_);
	linePos_ = 0;
	getAllArgsDelim(optionMask);
}

// Get next delimited chunk from file (not line)
bool LineParser::getCharsDelim(int optionMask, Dnchar *destarg)
{
	int length = 0;
	bool result = TRUE;
	char c;
	while (!file_->eof())
	{
		file_->get(c);
		if ((c == '\n') || (c == '\t') || (c == '\r') || (c == ' '))
		{
			if (length != 0) break;
			else continue;
		}
		if (c == '\0')
		{
			if (length == 0) result = FALSE;
			break;
		}
		tempArg_[length] = c;
		++length;
	}
	tempArg_[length] = '\0';
	if (destarg != NULL) destarg->set(tempArg_);
	return result;
}

// Get next delimited chunk from string, removing grabbed part
bool LineParser::getCharsDelim(int optionMask, Dnchar *source, Dnchar *destarg)
{
	// Get the next input chunk from the internal string and put into argument specified.
	msg.enter("LineParser::getCharsDelim(int,Dnchar,Dnchar)");
	int arglen, pos = 0, length = source->length();
	bool done, hadquotes, failed;
	char c, quotechar;
	failed = FALSE;
	done = FALSE;
	hadquotes = FALSE;
	quotechar = '\0';
	arglen = 0;
	while (pos < length)
	{
		c = (*source)[pos];
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
				if (quotechar != '\0')
				{
					tempArg_[arglen] = c;
					arglen ++;
				}
				else if (arglen != 0) done = TRUE;
				break;
			// Quote marks
			// If LineParser::UseQuotes, keep delimiters and other quote marks inside the quoted text.
			case (34):	// Double quotes
			case (39):	// Single quotes
				if (!(optionMask&LineParser::UseQuotes)) break;
				if (quotechar == '\0') quotechar = c;
				else if (quotechar == c)
				{
					quotechar = '\0';
					hadquotes = TRUE;
					done = TRUE;
				}
				else
				{
					tempArg_[arglen] = c;
					arglen ++;
				}
				break;
			// Curly brackets - treat in the same way as quotes
			case ('{'):
			case ('}'):
				if (!(optionMask&LineParser::UseBraces))
				{
					// Just add as normal character
					tempArg_[arglen] = c;
					arglen ++;
				}
				else
				{
					// If the quotechar is a left brace and we have a right brace, stop quoting
					if ((quotechar == '{') && (c == '}'))
					{
						quotechar = '\0';
						break;
					}
					// If we are already quoting by some other means, add character and exit
					if (quotechar != '\0')
					{
						tempArg_[arglen] = c;
						arglen ++;
					}
					// No previous quoting, so begin quoting if '{'
					if (c == '{') quotechar = '{';
				}
				break;
			// Parentheses
			case ('('):	// Left parenthesis
			case (')'):	// Right parenthesis
				if (optionMask&LineParser::StripBrackets) break;
				tempArg_[arglen] = c;
				arglen ++;
				break;
			// Comment markers
			case ('#'):	// "#" Rest/all of line is a comment
				endOfLine_ = TRUE;
				done = TRUE;
				break;
			// Normal character
			default: 
				tempArg_[arglen] = c;
				arglen ++;
				break;
		}
		// Increment line position
		pos ++;
		if (done || failed) break;
	}
	// Finalise argument
	tempArg_[arglen] = '\0';
	// Store the result in the desired destination
	if (destarg != NULL) *destarg = tempArg_;
	// Trim characters from source string
	source->eraseStart(pos);
	msg.exit("LineParser::getCharsDelim(int,Dnchar,Dnchar)");
	if (failed) return FALSE;
	return (arglen == 0 ? (hadquotes ? TRUE : FALSE) : TRUE);
}

// Return a number of characters from the input stream
const char *LineParser::getChars(int nchars, bool skipeol)
{
	char c;
	// Check number of characters requested
	int i=0;
	if (nchars == 0) return NULL;
	else if (nchars > MAXLINELENGTH)
	{
		msg.print("Error: The maximum number of characters read at once from a file is currently %i.\n", MAXLINELENGTH);
		return NULL;
	}
	else if (nchars < 0)
	{
		tempArg_[0] = '\0';
		for (int i=nchars; i<0; i++) file_->unget();
	}
	else for (i=0; i < nchars; ++i)
	{
		file_->get(c);
		if (skipeol) while ((c == '\n') || (c == '\r')) { if (file_->eof()) break; file_->get(c); }
		tempArg_[i] = c;
		if (file_->eof()) break;
	}
	tempArg_[i] = '\0';
	if (file_->eof())
	{
// 		closeFile();
		return NULL;
	}
	if (file_->fail())
	{
// 		closeFile();
		return NULL;
	}
	return tempArg_;
}

// Skip a number of characters from the input stream
void LineParser::skipChars(int nchars)
{
	if (nchars == 0) return;
	file_->ignore(nchars);
// 	if (file_->eof() || file_->fail()) closeFile();
}

// Return an integer value from reading 'n' chars of an (unformatted) input file
int LineParser::getInteger(int nbytes)
{
	// Use default size if none specified
	if (nbytes == 0)
	{
		int readi;
		file_->read((char*) &readi, sizeof(int));
		return readi;
	}
	// Try and select a suitable type for the read, based on nbytes
	if (sizeof(short int) == nbytes)
	{
		short int readi;
		file_->read((char*) &readi, nbytes);
		return (int) readi;
	}
	else if (sizeof(int) == nbytes)
	{
		int readi;
		file_->read((char*) &readi, nbytes);
		return readi;
	}
	else if (sizeof(long int) == nbytes)
	{
		long int readi;
		file_->read((char*) &readi, nbytes);
		return (int) readi;
	}
	else msg.print("Error: Integer of size %i bytes does not correspond to any internal type.\n", nbytes);
	return 0;
}

// Read an array of integer values from an (unformatted) input file
int LineParser::getIntegerArray(int *array, int count)
{
	file_->read((char*) array, count*sizeof(int));
	if (file_->eof())
	{
// 		closeFile();
		return -1;
	}
	if (file_->fail())
	{
// 		closeFile();
		return 0;
	}
	return 1;
}

// Return a double value from reading 'n' chars of an (unformatted) input file
double LineParser::getDouble(int nbytes)
{
	// Use default size if none specified
	if (nbytes == 0)
	{
		double readd;
		file_->read((char*) &readd, sizeof(double));
		return readd;
	}
	// Try and select a suitable type for the read, based on nbytes
	if (sizeof(double) == nbytes)
	{
		double readd;
		file_->read((char*) &readd, nbytes);
		return readd;
	}
	else if (sizeof(long double) == nbytes)
	{
		long double readd;
		file_->read((char*) &readd, nbytes);
		return (double) readd;
	}
	else msg.print("Error: Double of size %i bytes does not correspond to any internal type.\n", nbytes);
	return 0.0;
}

// Read an array of double values from an (unformatted) input file
int LineParser::getDoubleArray(double *array, int count)
{
	file_->read((char*) array, count*sizeof(double));
	if (file_->eof())
	{
// 		closeFile();
		return -1;
	}
	if (file_->fail())
	{
// 		closeFile();
		return 0;
	}
	return 1;
}

// Write line to file
bool LineParser::writeLine(const char *s)
{
	msg.enter("LineParser::writeLine");
	if (readOnly_)
	{
		msg.print("Unable to write line - destination file was opened read-only!\n");
		msg.exit("LineParser::writeLine");
		return FALSE;
	}
	*file_ << s;
	msg.exit("LineParser::writeLine");
	return TRUE;
}

// Write formatter line to file
bool LineParser::writeLineF(const char *fmt ...)
{
	msg.enter("LineParser::writeLine");
	if (readOnly_)
	{
		msg.print("Unable to write formatted line - destination file was opened read-only!\n");
		msg.exit("LineParser::writeLineF");
		return FALSE;
	}
	
	va_list arguments;
	static char s[8096];
	s[0] = '\0';
	// Parse the argument list (...) and internally write the output string into s[]
	va_start(arguments,fmt);
	vsprintf(s,fmt,arguments);
	va_end(arguments);
	*file_ << s;
	msg.exit("LineParser::writeLineF");
	return TRUE;
}

// Skip lines from file
int LineParser::skipLines(int nlines)
{
	msg.enter("LineParser::skipLines");
	int result;
	for (int n=0; n<nlines; n++)
	{
		result = readNextLine(LineParser::Defaults);
		if (result != 0)
		{
			msg.exit("LineParser::skipLines");
			return result;
		}
	}
	msg.exit("LineParser::skipLines");
	return 0;
}

/*
// Argument Data
*/

// Returns number of arguments grabbed from last parse
int LineParser::nArgs() const
{
	return arguments_.nItems();
}

// Returns the specified argument as a character string
const char *LineParser::argc(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning \"NULL\"...\n", i);
		return "NULL";
	}
	return arguments_[i]->get();
}

// Returns the specified argument as an integer
int LineParser::argi(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning 0...\n", i);
		return 0;
	}
	return arguments_[i]->asInteger();
}

// Returns the specified argument as a double
double LineParser::argd(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning 0.0...\n", i);
		return 0.0;
	}
	return arguments_[i]->asDouble();
}

// Returns the specified argument as a bool
bool LineParser::argb(int i)
{
	if ((i < 0) || (i >= nArgs()))
	{
		printf("Warning: Argument %i is out of range - returning FALSE...\n", i);
		return FALSE;
	}
	return arguments_[i]->asBool();
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

/*
// Atom type definition functions
*/

const char *LineParser::parseNetaString(Dnchar &source)
{
	// Cut the next atomtype command from the supplied string. Put in 'dest', along with any bracketed
	// argument part. Use brackets a bit like quotes are used above, except we don't toggle the flag.
	// Ignore spaces and horizontal tabs. Commas separate commands.
	msg.enter("LineParser::parseNetaString");
	int n, nchars, bracketlevel;
	bool done, el_list;
	static Dnchar typecmd;
	nchars = 0;
	bracketlevel = 0;
	el_list = FALSE;
	done = FALSE;
	typecmd.createEmpty(source);
// 	source.print();
	for (n=0; n<source.length(); n++)
	{
		switch (source[n])
		{
			case (32):	// Space
				break;
			case (10):	// Line feed (\n) - Signals end of the keyword.
				done = TRUE;
				break;
			case (13):	// Carriage Return - Signals end of the keyword.
				done = TRUE;
				break;
			case (44):	// Comma - end of keyword or separator in element list
				if (bracketlevel == 0 && !el_list) done = TRUE;
				else typecmd += ',';
				break;
			case (9):	// Horizontal Tab
				break;
			case (40):	// "(" - Signals beginning of options list
				bracketlevel ++;
				typecmd += '(';
				break;
			case (41):	// ")" - Signals end of options list
				bracketlevel --;
				typecmd += ')';
				break;
			case (91):	// "[" - Signals beginning of element list
				el_list = TRUE;
				// If we're inside brackets, *don't* remove it
				if (bracketlevel != 0) typecmd += source[n];
				break;
			case (93):	// "]" - Signals end of element list
				el_list = FALSE;
				// If we're inside brackets, *don't* remove it
				if (bracketlevel != 0) typecmd += source[n];
				break;
			default: 
				typecmd += source[n];
				break;
		}
		if (done) break;
	}
	// Strip off the keyword characters from the string.
	source.eraseStart(n+1);
// 	printf("Result = ");
// 	typecmd.print();
	msg.exit("LineParser::parseNetaString");
	return typecmd.get();
}

const char *LineParser::trimNetaKeyword(Dnchar &source)
{
	// Remove the keyword part of the command and put in 'dest', leaving the options (minus brackets)
	// in the original string. Remove '[' and ']' from keyword since this is only used to keep a list of elements together.
	msg.enter("LineParser::trimNetaKeyword");
	bool done, equals;
	static Dnchar keywd;
	done = FALSE;
	equals = FALSE;
// 	printf("String given to trimAtomKeyword = '%s'\n",source.get());
	keywd.createEmpty(source);
	int n;
	for (n=0; n<source.length(); n++)
	{
		switch (source[n])
		{
			case (40):	// "(" - if present signals start of options
				done = TRUE;
				break;
			case (10):	// Line feed (\n) - Signals end of the keyword.
				done = TRUE;
				break;
			case (13):	// Carriage Return - Signals end of the keyword.
				done = TRUE;
				break;
			case (61):	// "=" - Signals keyword/single option delimiter (unless first character)
				if (n == 0) keywd += source[n];
				else
				{
					done = TRUE;
					equals = TRUE;
				}
				break;
			case (91):	// '['
			case (93):	// ']'
				break;
			default: 
				keywd += source[n]; 
				break;
		}
		if (done) break;
	}
	// Trim off the keyword part
	if (equals) source.eraseStart(n+1);
	else source.eraseStart(n);
	//printf("RESULT in trim atkeywd = '%s'\n",dest.get());
	// Remove brackets if they're there
	if (source.length() != 0) 
		if (source[0] == '(')
		{
			source.eraseStart(1);
			source.eraseEnd(1);
		}
	msg.exit("LineParser::trimNetaKeyword");
	return keywd.get();
}
