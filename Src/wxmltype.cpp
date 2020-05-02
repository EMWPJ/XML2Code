#include "wxmltype.h"

QPoint3F Str2Point3F(QString _value)
{
    QStringList tmp = _value.split(',');
    return QPoint3F(tmp.at(0).toDouble(),tmp.at(1).toDouble(),tmp.at(2).toDouble());
}

QString Point3F2Str(QPoint3F _value)
{
    QString tmp = QString::number(_value.x,'g',16) + "," + QString::number(_value.y,'g',16) + "," + QString::number(_value.z,'g',16);
    return tmp;
}

QVector<QPoint3F> Str2Point3Fs(QString _value)
{
    QStringList tmp = _value.split(QRegExp("[ \r\n]"),QString::SkipEmptyParts);
    QVector<QPoint3F> result;
    for(int i=0; i<tmp.count(); i++)
    {
        result.append(Str2Point3F(tmp.at(i)));
    }
    return result;
}

QString Point3Fs2Str(QVector<QPoint3F> _value)
{
    QString tmp;
    for(int i=0; i<_value.count(); i++)
    {
        tmp += Point3F2Str(_value.at(i))  + " ";
    }
    tmp.chop(1);
    return tmp;
}
