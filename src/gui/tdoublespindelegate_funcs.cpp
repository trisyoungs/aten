/*
	*** TDoubleSpinDelegate Functions
	*** src/gui/tdoublespindelegate_funcs.cpp
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

#include "gui/tdoublespindelegate.hui"

TDoubleSpinDelegate::TDoubleSpinDelegate(QObject *parent, double vmin, double vmax, double vstep, double nDecimals) : QItemDelegate(parent)
{
	// Private variables
	min_ = vmin;
	max_ = vmax;
	step_ = vstep;
	nDecimals_ = nDecimals;
}

// Create editor
QWidget* TDoubleSpinDelegate::createEditor(QWidget* parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	// Create editor widget (in this case a double spin box) and set some properties
	QDoubleSpinBox* editor = new QDoubleSpinBox(parent);
	editor->setMinimum(min_);
	editor->setMaximum(max_);
	editor->setSingleStep(step_);
	editor->setDecimals(nDecimals_);

	return editor;
}

// Set initial value in editor
void TDoubleSpinDelegate::setEditorData(QWidget* editor, const QModelIndex &index) const
{
	double value = index.model()->data(index, Qt::EditRole).toDouble();

	QDoubleSpinBox* spinBox = static_cast<QDoubleSpinBox*>(editor);
	spinBox->setValue(value);
}

// Get value from editing widget, and set back in model
void TDoubleSpinDelegate::setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex &index) const
{
	QDoubleSpinBox* spinBox = static_cast<QDoubleSpinBox*>(editor);
	spinBox->interpretText();
	double value = spinBox->value();

	model->setData(index, value, Qt::EditRole);
}

// Update widget geometry
void TDoubleSpinDelegate::updateEditorGeometry(QWidget* editor, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	editor->setGeometry(option.rect);
}
