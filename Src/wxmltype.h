#ifndef WXMLTYPE_H
#define WXMLTYPE_H
#include <QtGlobal>
#include <QString>
#include <QStringList>
#include <QDebug>

struct QPoint3F;
QPoint3F Str2Point3F(QString _value);
QString Point3F2Str(QPoint3F _value);
QVector<QPoint3F> Str2Point3Fs(QString _value);
QString Point3Fs2Str(QVector<QPoint3F> _value);

struct QPoint3F
{
    qreal x;
    qreal y;
    qreal z;
    QPoint3F(qreal _x,qreal _y,qreal _z): x(_x), y(_y), z(_z) {}
};

#endif // WXMLTYPE_H
