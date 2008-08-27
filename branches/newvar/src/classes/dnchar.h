/*
	*** Dynamic character array
	*** src/classes/dnchar.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_DNCHAR_H
#define ATEN_DNCHAR_H

// Dynamic character array
class Dnchar
{
	public:
	// Constructor / Destructor
	Dnchar();
	~Dnchar();
	// Copy constructor
	Dnchar(const Dnchar&);
	// List pointers
	Dnchar *prev, *next;

	private:
	// Current size
	int size_;
	// Position of '\0' character
	int endPosition_;
	// Pointer to character array
	char *data_;

	public:
	// Print string info
	void print() const;
	// Clear string but don't free
	void clear();
	// Set value
	void set(const char *s);
	// Get value
	const char *get() const;
	// Resize data
	void createEmpty(int);
	// Resize data (to be same length as source Dnchar)
	void createEmpty(Dnchar&);
	// Returns the length of the current string
	int length() const;
	// Returns TRUE if current length is 1 or less.
	bool empty() const;
	// Erase range of characters from the string
	void erase(int, int);
	// Erase 'n' characters from start of string
	void eraseStart(int);
	// Erase 'n' characters from end of string
	void eraseEnd(int);
	// Find position of first occurrence of character 'c'
	int find(char);
	// Cut n characters from start of string and place in other
	void cutStart(int, Dnchar&);
	// Concatenate supplied string on to end
	void cat(const char*);
	// Assignment operator
	void operator=(const char*);
	// Assignment operator
	void operator=(const Dnchar&);
	// Equality operator (const char*)
	bool operator==(const char*) const;
	// Equality operator
	bool operator==(const Dnchar&) const;
	// Inequality operator
	bool operator!=(const Dnchar&) const;
	// Array subscript operator
	char operator[](int) const;
	// Character addition operator
	void operator+=(char);
	// Returns contents as double
	double asDouble() const;
	// Returns contents as integer
	int asInteger() const;
	// Returns contents as bool
	bool asBool() const;
	// Returns true if the string contains a number
	bool isNumeric() const;
};

#endif
