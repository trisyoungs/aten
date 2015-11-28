/*
	*** Progress Indicator Functions
	*** src/gui/progress_funcs.cpp
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

#include "gui/progress.h"
#include "base/messenger.h"
#include "templates/variantpointer.h"

ATEN_USING_NAMESPACE

// Constructor
AtenProgress::AtenProgress(QWidget* parent, Qt::WindowFlags flags) : QDialog(parent, flags)
{
	ui.setupUi(this);
	taskPoint_ = -1;
}

void AtenProgress::on_CancelButton_clicked(bool checked)
{
	Messenger::cancelAllTasks();

	hide();
}

// Show dialog, setting all widget values and titles
void AtenProgress::updateAndShow()
{
	QTableWidgetItem* item;
	QProgressBar* bar;
	int row = 0;

	// Clear and update task table if necessary
	if (taskPoint_ != Messenger::taskPoint())
	{
		ui.TaskTable->clear();
		ui.TaskTable->setColumnCount(3);
		ui.TaskTable->setRowCount(Messenger::nTasks());
		for (Task* task = Messenger::tasks(); task != NULL; task = task->next)
		{
			bar = new QProgressBar(this);
			bar->setRange(0, task->nSteps());
			ui.TaskTable->setCellWidget(row, 0, bar);

			item = new QTableWidgetItem(task->title());
			item->setData(Qt::UserRole, VariantPointer<Task>(task));
			ui.TaskTable->setItem(row, 1, item);

			item = new QTableWidgetItem(task->etaText());
			ui.TaskTable->setItem(row, 2, item);
		}
		ui.TaskTable->setColumnWidth(0, 64);
		ui.TaskTable->resizeColumnsToContents();
		taskPoint_ = Messenger::taskPoint();
	}

	// Update item information
	for (int row=0; row < ui.TaskTable->rowCount(); ++row)
	{
		// Get title item (which contains Task pointer)
		item = ui.TaskTable->item(row, 1);
		if (!item) return;
		Task* task = (Task*) VariantPointer<Task>(item->data(Qt::UserRole));
		if (!task) return;
		item->setText(task->title());

		// Get second item (progress bar)
		bar = (QProgressBar*) ui.TaskTable->cellWidget(row, 0);
		if (bar) bar->setValue(task->currentStep());

		// Get third item
		item = ui.TaskTable->item(row, 2);
		if (item) item->setText(task->etaText());
	}

	// Update main ETA information (from first task)
	Task* task = Messenger::tasks();
	if (task)
	{
		ui.TitleLabel->setText(task->title());
		ui.ProgressBar->setRange(0, task->nSteps());
		ui.ProgressBar->setValue(task->currentStep());
		ui.TimeRemainingLabel->setText(task->etaText());
	}
	else
	{
		ui.TitleLabel->setText("No Task");
		ui.ProgressBar->setRange(0, 1);
		ui.ProgressBar->setValue(0);
		ui.TimeRemainingLabel->setText("N/A");
	}

	QApplication::processEvents();
	
	// If there are no tasks, close the window
	if (Messenger::nTasks() == 0) hide();
	else show();
}

// Close dialog window
void AtenProgress::terminate()
{
	hide();
}
