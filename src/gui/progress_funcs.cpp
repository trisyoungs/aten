/*
	*** Progress Indicator Functions
	*** src/gui/progress_funcs.cpp
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

#include "gui/progress.h"
#include "gui/mainwindow.h"
#include "base/messenger.h"
#include "templates/variantpointer.h"

ATEN_USING_NAMESPACE

// Constructor
AtenProgress::AtenProgress(AtenWindow& parent, Qt::WindowFlags flags) : QDialog(&parent, flags), parent_(parent)
{
	ui.setupUi(this);
	taskPoint_ = -1;
	lastUpdatePercentage_ = 0;
}

void AtenProgress::on_UpdateFrequencySlider_valueChanged(int value)
{
	lastUpdatePercentage_ = 0;
	if (value == 0) ui.UpdateFrequencyLabel->setText("Off");
	else if (value == 1) ui.UpdateFrequencyLabel->setText("Occasionally");
	else if (value == 2) ui.UpdateFrequencyLabel->setText("Sometimes");
	else if (value == 3) ui.UpdateFrequencyLabel->setText("Frequently");
	else if (value == 4) ui.UpdateFrequencyLabel->setText("Always");
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
		for (RefListItem<Task,int>* ri = Messenger::tasks(); ri != NULL; ri = ri->next)
		{
			Task *task = ri->item;

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
	RefListItem<Task,int>* ri = Messenger::tasks();
	int mainCompletion = 0;
	Task* task = ri->item;
	if (task)
	{
		ui.TitleLabel->setText(task->title());
		ui.ProgressBar->setRange(0, task->nSteps());
		ui.ProgressBar->setValue(task->currentStep());
		ui.TimeRemainingLabel->setText(task->etaText());
		mainCompletion = ((double)task->currentStep() / (double) task->nSteps()) * 100;
	}
	else
	{
		ui.TitleLabel->setText("No Task");
		ui.ProgressBar->setRange(0, 1);
		ui.ProgressBar->setValue(0);
		ui.TimeRemainingLabel->setText("N/A");
	}

	// Process application events
	QApplication::processEvents();

	// If the Update slider is enabled, see if we need to do an update
	if (ui.UpdateFrequencySlider > 0)
	{
		int delta = mainCompletion - lastUpdatePercentage_;
		int divisor = ui.UpdateFrequencySlider->value() == 4 ? 100 : ui.UpdateFrequencySlider->value()*5;
		if (delta >= (100/divisor))
		{
			lastUpdatePercentage_ = mainCompletion;
			parent_.updateWidgets(AtenWindow::MainViewTarget);
		}
	}
	
	// If there are no tasks, close the window
	if (Messenger::nTasks() == 0) hide();
	else show();
}

// Close dialog window
void AtenProgress::terminate()
{
	hide();
}
