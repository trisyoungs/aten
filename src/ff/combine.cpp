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
#include "base/prefs.h"
#include "base/sysfunc.h"
#include "base/messenger.h"

ATEN_USING_NAMESPACE

// Combination rules
const char* CombinationRuleKeywords[CombinationRules::nCombinationRules] = { "arithmetic", "geometric", "custom1", "custom2", "custom3" };
const char* CombinationRuleNames[CombinationRules::nCombinationRules] = { "Arithmetic Mean [(a+b)/2]", "Geometric Mean [sqrt(a*b)]", "Custom Rule 1", "Custom Rule 2", "Custom Rule 3" };
CombinationRules::CombinationRule CombinationRules::combinationRule(QString s, bool reportError)
{
	CombinationRules::CombinationRule cr = (CombinationRules::CombinationRule) enumSearch("combination rule",CombinationRules::nCombinationRules,CombinationRuleKeywords,s);
	if ((cr == CombinationRules::nCombinationRules) && reportError) enumPrintValid(CombinationRules::nCombinationRules,CombinationRuleKeywords);
	return cr;
}
const char* CombinationRules::combinationRule(CombinationRule cr)
{
	return CombinationRuleKeywords[cr];
}
const char* CombinationRules::combinationRuleName(CombinationRule cr)
{
	return CombinationRuleNames[cr];
}

// Regenerate combination rule function trees
bool CombinationRules::regenerateEquations()
{
	Messenger::enter("CombinationRules::regenerateEquations");
	CombinationRules::CombinationRule cr;
	QStringList equations;
	for (int n=0; n<CombinationRules::nCombinationRules; ++n)
	{
		cr = (CombinationRules::CombinationRule) n;
		QString equation;
		equation.sprintf("double %s(double a, double b) { double c = 0.0; %s; return c; }", CombinationRules::combinationRule(cr), qPrintable(prefs.combinationRule(cr)));
		equations << equation;
	}
	bool success = combinationRules_.generateFromStringList(equations, "CombinationRules", "Combination Rule", false);
	Messenger::exit("CombinationRules::regenerateEquations");
	return success;
}

// Execute combination rule with parameters specified
double CombinationRules::combine(CombinationRules::CombinationRule cr, double a, double b)
{
	Messenger::enter("CombinationRules::combine");
	ReturnValue rv;
	if (!combinationRules_.executeFunction(CombinationRules::combinationRule(cr), rv, "dd", a, b))
	{
		printf("Internal Error: Couldn't find function corresponding to combination rule.\n");
		Messenger::exit("CombinationRules::combine");
		return 0.0;
	}
	Messenger::exit("CombinationRules::combine");
	return rv.asDouble();
}
