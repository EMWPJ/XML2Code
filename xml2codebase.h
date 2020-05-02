#ifndef XML2CODEBASE_H
#define XML2CODEBASE_H

#include <QObject>
#include <QtXml>
#include <QVector>
#include <QList>

#include "../Src/xmlconvert.h"
#include "../Src/wxml.h"

#include <QDebug>

class WXML2CodeBase;
class WXML2CodeClassBase;
class WXML2CodeChildBase;

class WXML2Code;
class WXML2CodeClass;
class WXML2CodeChild;

class WXML2CodeBase : public WXML
{
    Q_OBJECT
private:
    QString Name;
    QVector <WXML2CodeClass*> XMLClasss;
public:
    explicit WXML2CodeBase(QObject *parent = nullptr);
    ~WXML2CodeBase();
    WXML2CodeClass* AddXMLClass();
    void RemoveXMLClass(WXML2CodeClass* _value);
    void DeleteXMLClass(qint32 index);
    WXML2CodeClass* XMLClass(qint32 index);
    qint32 XMLClassCount();
    void FromXML(QDomNode node);
    QDomNode ToXML(QDomNode par);
    QString GetName();
    void SetName(QString _value);
};

class WXML2CodeClassBase : public WXML
{
    Q_OBJECT
private:
    QString Name;
    QString DataType;
    bool Root;
    QVector <WXML2CodeChild*> Childs;
public:
    explicit WXML2CodeClassBase(QObject *parent = nullptr);
    ~WXML2CodeClassBase();
    WXML2CodeChild* Child(qint32 index);
    qint32 ChildCount();
    void FromXML(QDomNode node);
    QDomNode ToXML(QDomNode par);
    QString GetDataType();
    void SetDataType(QString _value);
    QString GetName();
    void SetName(QString _value);
};

class WXML2CodeChildBase : public WXML
{
    Q_OBJECT
private:
    bool Leaf;
    bool Visual;
    QString Name;
    QString DataType;
    QString Path;
    qint32 Number;
public:
    explicit WXML2CodeChildBase(QObject *parent = nullptr);
    ~WXML2CodeChildBase();
    void FromXML(QDomNode node);
    QDomNode ToXML(QDomNode par);
    bool GetLeaf();
    void SetLeaf(bool _value);
    bool GetVisual();
    void SetVisual(bool _value);
    QString GetName();
    void SetName(QString _value);
    QString GetDataType();
    void SetDataType(QString _value);
    QString GetPath();
    void SetPath(QString _value);
    qint32 GetNumber();
    void SetNumber(qint32 _value);
};

#endif // XML2CODEBASE_H
