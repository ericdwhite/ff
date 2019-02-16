#include "DateComponent.hxx"
#include "FFI/Cxx.hxx"
#include "LinkButton.hxx"
#include "TaskActionsButton.hxx"
#include "TaskWidget.hxx"


QDate qDate(Date d) {
    return QDate(d.year, d.month, d.day);
}


TaskWidget::TaskWidget(QWidget * parent, StorageHandle storage, Note task):
    super(parent), label(new QLabel(QString::fromStdString(task.text)))
{
    auto box = new QVBoxLayout(this);
    box->addWidget(label);
    {
        auto row = new QHBoxLayout;
        row->addLayout(new DateComponent(
            "Start:", qDate(task.start), not task.isTracking
        ));
        row->addLayout(new DateComponent(
            "Deadline:", qDate(task.end), not task.isTracking
        ));
        row->addWidget(new TaskActionsButton(storage, task.id));
        row->addStretch();
        box->addLayout(row);
    }
    if (task.isTracking) {
        box->addWidget(new LinkButton(
            QString::fromStdString(
                task.track.provider + ": " + task.track.source + " #"
                + task.track.externalId
            ),
            QString::fromStdString(task.track.url)
        ));
    }
}
