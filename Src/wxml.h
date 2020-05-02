#ifndef WXML_H
#define WXML_H

#include <QObject>
#include <QtXml>
#include <QDebug>

class WXML;
class WXML : public QObject
{
    Q_OBJECT    
private:
public:
    explicit WXML(QObject *parent = nullptr);
     ~WXML();

    void Save(QString fileName);
    void Load(QString fileName);
    virtual void FromXML(QDomNode node)=0;
    virtual QDomNode ToXML(QDomNode par)=0;

signals:

};

#endif // WXML_H
