/*
	*** Combination Rules
	*** src/ff/combine.cpp
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

#include "ff/combine.h"
#include "base/messenger.h"
#include <math.h>

ATEN_USING_NAMESPACE

// Combination rules
const char* CombinationRuleKeywords[CombinationRules::nCombinationRules] = { "arithmetic", "geometric" };
const char* CombinationRuleNames[CombinationRules::nCombinationRules] = { "Arithmetic Mean [(a+b)/2]", "Geometric Mean [sqrt(a*b)]" };
const char* CombinationRules::combinationRule(CombinationRule cr)
{
	return CombinationRuleKeywords[cr];
}
const char* CombinationRules::combinationRuleName(CombinationRule cr)
{
	return CombinationRuleNames[cr];
}

// Execute combination rule with parameters specified
double CombinationRules::combine(CombinationRules::CombinationRule cr, double a, double b)
{
	switch (cr)
	{
		case (CombinationRules::ArithmeticRule):
			return (a+b)*0.5;
			break;
		case (CombinationRules::GeometricRule):
			return sqrt(a*b);
			break;
	}
	return 0.0;
}
