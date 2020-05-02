#ifndef XML2CODE_H
#define XML2CODE_H

#include <QtXml>
#include <QVector>
#include <QList>

#include "../Src/wxml.h"
#include "xml2codebase.h"
#include <QDebug>

class WXML2Code : public WXML2CodeBase
{
    Q_OBJECT
private:
    QString BaseInclude();
    QString BaseTypes();
    QString BaseImplements();
    QString BaseStatements();
    QString Include();
    QString Types();
    QString Implements();
    QString Statements();
public:
    explicit WXML2Code(QObject *parent = nullptr);
    ~WXML2Code();
    void ExportBaseCode(QString filePath);
    void ExportCode(QString filePath);
};

class WXML2CodeClass : public WXML2CodeClassBase
{
    Q_OBJECT
private:
public:
    QString BaseImplements();
    QString BaseStatements();
    QString BaseProtectedStatements();
    QString BasePublicStatements();
    QString BaseConstructorStatements();
    QString BaseConstructorImplements();
    QString BaseDestructorStatements();
    QString BaseDestructorImplements();
    QString ProtectedStatements();
    QString PublicStatements();
    QString Implements();
    QString Statements();
    QString ConstructorStatements();
    QString ConstructorImplements();
    QString DestructorStatements();
    QString DestructorImplements();

    QString FromXMLImplement();
    QString FromXMLStatement();
    QString ToXMLImplement();
    QString ToXMLStatement();
public:
    explicit WXML2CodeClass(QObject *parent = nullptr);
    ~WXML2CodeClass();
};

class WXML2CodeChild : public WXML2CodeChildBase
{
    Q_OBJECT
private:
public:
    QString ChildFieldStatement();
    QString ChildAddImplement();
    QString ChildAddStatement();
    QString ChildCountImplement();
    QString ChildCountStatement();
    QString ChildGetImplement();
    QString ChildGetStatement();
    QString ChildSetImplement();
    QString ChildSetStatement();
    QString ChildRemoveImplement();
    QString ChildRemoveStatement();
    QString ChildClearImplement();
    QString ChildClearStatement();

    QString ChildPublicStatements();
    QString ChildProtectedStatements();
    QString ChildBaseImplements();

    QString LeafFieldStatement();
    QString LeafAddImplement();
    QString LeafAddStatement();
    QString LeafClearImplement();
    QString LeafClearStatement();
    QString LeafCountImplement();
    QString LeafCountStatement();
    QString LeafGetImplement();
    QString LeafGetStatement();
    QString LeafSetImplement();
    QString LeafSetStatement();
    QString LeafRemoveImplement();
    QString LeafRemoveStatement();

    QString LeafBaseImplements();
    QString LeafProtectedStatements();
    QString LeafPublicStatements();

    QString LeafType();
    WXML2CodeClass *ParentClass();
    QString ConvertStr();
    QString ConvertXML();
    XMLNodeType NodeType();

    QString ReadAttribute();
    QString ReadChild();

    QString WriteAttribute();
    QString WriteChild();
public:
    explicit WXML2CodeChild(QObject *parent = nullptr);
    ~WXML2CodeChild();
};

#endif // XML2CODE_H
