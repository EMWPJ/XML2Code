#include "xml2code.h"

WXML2Code::WXML2Code(QObject *parent) : WXML2CodeBase(parent)
{

}

WXML2Code::~WXML2Code()
{

}

QString WXML2Code::BaseInclude()
{
    QString str;
    str = "#include <QObject>\n";
    str += "#include <QtXml>\n";
    str += "#include <QVector>\n";
    str += "#include <QDebug>\n";
    str += "#include \"../Src/xmlconvert.h\"\n";
    str += "#include \"../Src/wxmltype.h\"\n";
    str += "#include \"../Src/wxml.h\"\n";
    return str;
}

QString WXML2Code::BaseTypes()
{
    QString str = "";
    for(int i=0; i<XMLClassCount(); i++)
    {
        str += "class " + XMLClass(i)->GetDataType() + "Base;\n";
    }
    str += "\n";
    return str;
}

QString WXML2Code::BaseImplements()
{
    QString str = "";
    for(int i=0; i<XMLClassCount(); i++)
    {
        str += XMLClass(i)->BaseImplements() + "\n";
    }
    return str;
}

QString WXML2Code:: BaseStatements()
{
    QString str = "";
    for(int i=0; i<XMLClassCount(); i++)
    {
        str += XMLClass(i)->BaseStatements() + "\n";
    }
    return str;
}

QString WXML2Code::Include()
{
    QString str;
    str = "#include <QObject>\n";
    str += "#include <QtXml>\n";
    str += "#include <QVector>\n";
    str += "#include \"../Src/xmlconvert.h\"\n";
    str += "#include \"../Src/wxmltype.h\"\n";
    str += "#include \"../Src/wxml.h\"\n";
    str += "#include \"" + GetName() + "Base.h\"\n";
    return str;
}

QString WXML2Code::Types()
{
    QString str = "";
    for(int i=0; i<XMLClassCount(); i++)
    {
        str += "class " + XMLClass(i)->GetDataType() + ";\n";
    }
    str += "\n";
    return str;
}

QString WXML2Code::Implements()
{
    QString str = "";
    for(int i=0; i<XMLClassCount(); i++)
    {
        str += XMLClass(i)->Implements() + ";\n";
    }
    str += "\n";
    return str;
}

QString WXML2Code::Statements()
{
    QString str = "";
    for(int i=0; i<XMLClassCount(); i++)
    {
        str += XMLClass(i)->Statements() + ";\n";
    }
    str += "\n";
    return str;
}

void WXML2Code::ExportBaseCode(QString filePath)
{
    QFile fileh(filePath + "\\" + GetName() + "Base.h");
    if(!fileh.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        return;
    }
    QString str = "#ifndef " + GetName().toUpper() + "BASE_H\n";
    str +=  "#define " + GetName().toUpper() + "BASE_H\n";
    fileh.write(str.toUtf8());
    str = BaseInclude();
    fileh.write(str.toUtf8());
    str = BaseTypes();
    fileh.write(str.toUtf8());
    str = Types();
    fileh.write(str.toUtf8());
    str = BaseStatements();
    fileh.write(str.toUtf8());
    str = "#endif // " + GetName().toUpper() + "BASE_H";
    fileh.write(str.toUtf8());
    fileh.close();

    QFile filecpp(filePath + "\\" + GetName() + "Base.cpp");
    if(!filecpp.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        return;
    }
    str = "#include \"" + GetName() + "Base.h\"\n";
    str += "#include \"" + GetName() + ".h\"\n\n";
    filecpp.write(str.toUtf8());
    str = BaseImplements();
    filecpp.write(str.toUtf8());
    filecpp.close();
}

void WXML2Code::ExportCode(QString filePath)
{
    QFile fileh(filePath  + "\\" + GetName() + ".h");
    if(!fileh.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        return;
    }
    QString str = "#ifndef " + GetName().toUpper() + "_H\n";
    str +=  "#define " + GetName().toUpper() + "_H\n";
    fileh.write(str.toUtf8());
    str = Include();
    fileh.write(str.toUtf8());
    str = Statements();
    fileh.write(str.toUtf8());
    str = "#endif // " + GetName().toUpper() + "_H";
    fileh.write(str.toUtf8());
    fileh.close();

    QFile filecpp(filePath + "\\" + GetName() + ".cpp");
    if(!filecpp.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        return;
    }
    str = "#include \"" + GetName() + ".h\"\n\n";
    filecpp.write(str.toUtf8());
    str = Implements();
    filecpp.write(str.toUtf8());
    filecpp.close();
}


WXML2CodeClass::WXML2CodeClass(QObject *parent) : WXML2CodeClassBase(parent)
{

}

WXML2CodeClass::~WXML2CodeClass()
{

}

QString WXML2CodeClass::BaseImplements()
{
    QString str = "";
    str += "//" + GetName() + "\n";
    str += BaseConstructorImplements() + BaseDestructorImplements();
    str += FromXMLImplement();
    str += ToXMLImplement();
    for(int i=0; i<ChildCount(); i++)
    {
        if(Child(i)->GetLeaf())
        {
            str += Child(i)->LeafBaseImplements();
        }
        else
        {
            str += Child(i)->ChildBaseImplements();
        }
    }
    str += "\n";
    return str;
}

QString WXML2CodeClass::BaseStatements()
{
    QString str = "class " + GetDataType() + "Base : public " + "WXML\n{\n    Q_OBJECT\n";
    str += BaseProtectedStatements() + BasePublicStatements();
    str += "};\n\n";
    return str;
}

QString WXML2CodeClass::BaseProtectedStatements()
{
    QString str = "protected:\n";
    for(int i=0; i<ChildCount(); i++)
    {
        if(Child(i)->GetLeaf())
        {
            str += Child(i)->LeafProtectedStatements();
        }
        else
        {
            str += Child(i)->ChildProtectedStatements();
        }
    }
    str += "\n";
    return str;
}

QString WXML2CodeClass::BasePublicStatements()
{
    QString str = "public:\n";
    str += BaseConstructorStatements();
    str += BaseDestructorStatements();
    str += FromXMLStatement();
    str += ToXMLStatement();
    for(int i=0; i<ChildCount(); i++)
    {
        if(Child(i)->GetLeaf())
        {
            str += Child(i)->LeafPublicStatements();
        }
        else
        {
            str += Child(i)->ChildPublicStatements();
        }
    }
    str += "\n";
    return str;
}

QString WXML2CodeClass::ProtectedStatements()
{
    QString str = "protected:\n";
    str += "\n";
    return str;
}

QString WXML2CodeClass::PublicStatements()
{
    QString str = "public:";
    str += ConstructorStatements();
    str += DestructorStatements();
    str += "\n";
    return str;
}

QString WXML2CodeClass::Implements()
{
    QString str = "//    " +  GetDataType() + "\n";
    str += ConstructorImplements();
    str += DestructorImplements();
    return str;
}

QString WXML2CodeClass::Statements()
{
    QString str = "class " + GetDataType() + " : public " + GetDataType() + "Base\n{\n    Q_OBJECT\n";
    str += "public:\n";
    str += ConstructorStatements() + DestructorStatements();
    str += "};\n\n";
    return str;
}

QString WXML2CodeClass::BaseConstructorStatements()
{
    QString str = "    explicit " + GetDataType() + "Base(QObject *parent = nullptr);\n";
    return str;
}

QString WXML2CodeClass::BaseConstructorImplements()
{
    QString str =  GetDataType() + "Base::" + GetDataType() + "Base(QObject *parent) : WXML(parent)\n{\n";
    for(int i=0; i<ChildCount(); i++)
    {
        if(Child(i)->GetLeaf())
        {
            switch (Child(i)->GetNumber())
            {
            case 0:
            {
                str += "    " + Child(i)->GetName() + "Exsit = false;\n";
                break;
            }
            case 1:
            {
                break;
            }
            case 2:
            {
                break;
            }
            }
        }
        else
        {
            switch (Child(i)->GetNumber())
            {
            case 0:
            {
                str += "    " + Child(i)->GetName() + "Exsit = false;\n";
                break;
            }
            case 1:
            {
                str += "    " + Child(i)->GetName() + " = new " + Child(i)->GetDataType() + "(this);\n";
                break;
            }
            case 2:
            {
                break;
            }
            }
        }
    }
    str += "}\n\n";
    return str;
}

QString WXML2CodeClass::BaseDestructorStatements()
{
    QString str = "    ~" + GetDataType() + "Base();\n";
    return str;
}

QString WXML2CodeClass::BaseDestructorImplements()
{
    QString str = GetDataType() + "Base::~" + GetDataType() + "Base()\n{\n";
    for(int i=0; i<ChildCount(); i++)
    {
        if(!Child(i)->GetLeaf())
        {
            switch (Child(i)->GetNumber())
            {
            case 0:
            {
                str += "    if(" + Child(i)->GetName() + "Exsit)\n    {\n        delete " + Child(i)->GetName() + ";\n    }\n";
                break;
            }
            case 1:
            {
                str += "    delete " + Child(i)->GetName() + ";\n";
                break;
            }
            case 2:
            {
                str += "    " + Child(i)->GetName() + "Clear();\n";
                break;
            }
            }
        }
    }
    str += "}\n\n";
    return str;
}

QString WXML2CodeClass::ConstructorStatements()
{
    QString str = "    explicit " + GetDataType() + "(QObject *parent = nullptr);\n";
    return str;
}

QString WXML2CodeClass::ConstructorImplements()
{
    QString str =  GetDataType() + "::" + GetDataType() + "(QObject *parent) : "
            + GetDataType() + "Base(parent)\n{\n";
    str += "\n}\n\n";
    return str;
}

QString WXML2CodeClass::DestructorStatements()
{
    QString str = "    ~" + GetDataType() + "();\n";
    return str;
}

QString WXML2CodeClass::DestructorImplements()
{
    QString str = GetDataType() + "::~" + GetDataType() + "()\n{\n\n}\n";
    return str;
}

QString WXML2CodeClass::FromXMLImplement()
{
    QString str = "void " + GetDataType() + "Base::FromXML(QDomNode node)\n{\n";
    str += "    for(int i=0; i<node.attributes().count(); i++)\n    {\n";
    str += "        QDomNode tmp = node.attributes().item(i);\n";
    QString stratt = "";
    for(int i=0; i<ChildCount(); i++)
    {
        stratt += Child(i)->ReadAttribute();
    }
    if(stratt.count() >= 13)
    {
        stratt.remove(8,5);
    }
    stratt += "    }\n";
    str += stratt;


    str += "    for(int i=0; i<node.childNodes().count(); i++)\n    {\n";
    str += "        QDomNode tmp = node.childNodes().at(i);\n";
    QString strnode = "";
    for(int i=0; i<ChildCount(); i++)
    {
        strnode += Child(i)->ReadChild();
    }
    if(strnode.count() >= 13)
    {
        strnode.remove(8,5);
    }
    strnode += "    }\n";
    str += strnode;
    str += "}\n\n";
    return str;
}

QString WXML2CodeClass::FromXMLStatement()
{
    QString str = "    void FromXML(QDomNode node);\n";
    return str;
}

QString WXML2CodeClass::ToXMLImplement()
{
    QString str = "QDomNode " + GetDataType() + "Base::ToXML(QDomNode par)\n{\n";
    str += "    QDomDocument doc = par.ownerDocument();\n";
    str += "    QDomElement  node = doc.createElement(\"" + GetName() + "\");\n";
    str += "    par.appendChild(node);\n";
    for(int i=0; i<ChildCount(); i++)
    {
        str += Child(i)->WriteAttribute();
        str += Child(i)->WriteChild();
    }
    str += "    return node;\n";
    str += "}\n\n";
    return str;
}

QString WXML2CodeClass::ToXMLStatement()
{
    QString str = "    QDomNode ToXML(QDomNode par);\n";
    return str;
}


WXML2CodeChild::WXML2CodeChild(QObject *parent) : WXML2CodeChildBase(parent)
{

}

WXML2CodeChild::~WXML2CodeChild()
{

}

QString WXML2CodeChild::ChildFieldStatement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "    " + GetDataType() + "* " + GetName() + ";\n";
        str += "    bool " + GetName() + "Exsit;\n";
        break;
    }
    case 1:
    {
        str += "    " + GetDataType() + "* "  + GetName() + ";\n";
        break;
    }
     case 2:
    {
        str += "    QVector <" + GetDataType() + " *> " + GetName() + "s;\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::ChildAddImplement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += GetDataType() + "* " + ParentClass()->GetDataType() + "Base::Add" + GetName() + "()\n{\n";
         str += "    if(" + GetName() + "Exsit)\n    {\n        return " + GetName() + ";\n    }\n";
         str += "    " + GetName() + " = new " + GetDataType() + "(this);\n";
         str += "    " + GetName() + "Exsit = true;\n";
         str += "    return " + GetName() + ";\n}\n\n";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += GetDataType() + "* " + ParentClass()->GetDataType() + "Base::Add" + GetName() + "()\n{\n";
         str += "    " + GetDataType() + " *tmp = new " + GetDataType() + "(this);\n";
         str += "    " + GetName() + "s.append(tmp);\n";
         str += "    return tmp;\n}\n\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildAddStatement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "    " + GetDataType() + "* Add" + GetName() + "();\n";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += "    " + GetDataType() + "* Add" + GetName() + "();\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildCountImplement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += "qint32 " + ParentClass()->GetDataType() + "Base::" + GetName() + "Count()\n{\n";
         str += "    return " + GetName() + "s.count();\n}\n\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildCountStatement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += "    qint32 " + GetName() + "Count();\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildGetImplement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += GetDataType() + "* " + ParentClass()->GetDataType() + "Base::Get" + GetName() + "()\n{\n";
        str += "    if(!" + GetName() + "Exsit)\n    {\n        return nullptr;\n    }\n";
        str += "    return " + GetName() + ";\n}\n\n";
        break;
    }
    case 1:
    {
        str += GetDataType() + "* " + ParentClass()->GetDataType() + "Base::Get" + GetName() + "()\n{\n";
        str += "    return " + GetName() + ";\n}\n\n";
        break;
    }
     case 2:
    {
        str += "QVector <" + GetDataType() + "*> " + ParentClass()->GetDataType() + "Base::Get" + GetName() + "()\n{\n";
        str += "    return " + GetName() + "s;\n}\n\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::ChildGetStatement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "    " + GetDataType() + "* Get" + GetName() + "();\n";
         break;
     }
     case 1:
     {
         str += "    " + GetDataType() + "* Get" + GetName() + "();\n";
         break;
     }
     case 2:
     {
         str += "    QVector <" + GetDataType() + "*> Get" + GetName() + "();\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildSetImplement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::Set" + GetName() + "(" + GetDataType() + " *_value)\n{\n";
        str += "    _value->setParent(this);\n";
        str += "    if(" + GetName() + "Exsit)\n    {\n";
        str += "        delete " + GetName() + ";\n    }\n";
        str += "    " + GetName() + "Exsit = true;\n";
        str += "    " + GetName() + " = _value;\n}\n\n";
        break;
    }
    case 1:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::Set" + GetName() + "(" + GetDataType() + " *_value)\n{\n";
        str += "    _value->setParent(this);\n";
        str += "    " + GetName() + " = _value;\n}\n\n";
        break;
    }
     case 2:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::Set" + GetName() + "(QVector <" + GetDataType() + "*> _value)\n{\n";
        str += "    " + GetName() + "Clear();\n";
        str += "    " + GetName() + "s.append(_value);\n}\n\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::ChildSetStatement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "    void Set" + GetName() + "(" + GetDataType() + " *_value);\n";
         break;
     }
     case 1:
     {
         str += "    void Set" + GetName() + "(" + GetDataType() + " *_value);\n";
         break;
     }
     case 2:
     {
         str += "    void Set" + GetName() + "(QVector <" + GetDataType() + "*> _value);\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildRemoveImplement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "void " + ParentClass()->GetDataType() + "Base::Reomve" + GetName() + "()\n{\n";
         str += "    if(" + GetName() + "Exsit)\n    {\n";
         str += "        delete " + GetName() + ";\n";
         str += "    " + GetName() + "Exsit = false;\n    }\n}\n\n";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += "void " + ParentClass()->GetDataType() + "Base::Reomve" + GetName() + "(" + GetDataType() + "* _value)\n{\n";
         str += "    " + GetName() + "s.removeOne(_value);\n    delete _value;\n}\n\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildRemoveStatement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "    void Reomve" + GetName() + "();\n";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += "    void Reomve" + GetName() + "(" + GetDataType() + "* _value);\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildClearImplement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += "void " + ParentClass()->GetDataType() + "Base::" + GetName() + "Clear()\n{\n";
         str += "    qDeleteAll(" + GetName() + "s);\n}\n\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildClearStatement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += "    void " + GetName() + "Clear();\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::ChildPublicStatements()
{
    QString str = "";
    str += ChildAddStatement();
    str += ChildCountStatement();
    str += ChildGetStatement();
    str += ChildSetStatement();
    str += ChildRemoveStatement();
    str += ChildClearStatement();
    return str;
}

QString WXML2CodeChild::ChildProtectedStatements()
{
    QString str = "";
    str += ChildFieldStatement();
    return str;
}

QString WXML2CodeChild::ChildBaseImplements()
{
    QString str = "";
    str += ChildAddImplement();
    str += ChildCountImplement();
    str += ChildGetImplement();
    str += ChildSetImplement();
    str += ChildRemoveImplement();
    str += ChildClearImplement();
    return str;
}

QString WXML2CodeChild::LeafFieldStatement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "    " + LeafType() + " " + GetName() + ";\n";
        str += "    bool " + GetName() + "Exsit;\n";
        break;
    }
    case 1:
    {
        str += "    " + LeafType() + " "  + GetName() + ";\n";
        break;
    }
     case 2:
    {
        str += "    QVector <" + LeafType() + "> " + GetName() + "s;\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafAddImplement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += LeafType() + " " + ParentClass()->GetDataType() + "Base::Add" + GetName() + "()\n{\n";
         str += "    if(" + GetName() + "Exsit)\n    {\n        return " + GetName() + ";\n    }\n";
         str += "    " + GetName() + "Exsit = true;\n";
         str += "    return " + GetName() + ";\n}\n\n";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += LeafType() + "* " + ParentClass()->GetDataType() + "Base::Add" + GetName() + "()\n";
         str += "    " + LeafType() + " tmp;\n";
         str += "    " + GetName() + "s.append(tmp);\n";
         str += "    return tmp;\n}\n\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::LeafAddStatement()
{
    QString str = "";
     switch (GetNumber())
     {
     case 0:
     {
         str += "    " + LeafType() + " Add" + GetName() + "();\n";
         break;
     }
     case 1:
     {
         str += "";
         break;
     }
     case 2:
     {
         str += "    " + LeafType() + "* Add" + GetName() + "();\n";
         break;
     }
     }
     return str;
}

QString WXML2CodeChild::LeafClearImplement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "";
        break;
    }
    case 1:
    {
        str += "";
        break;
    }
    case 2:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::" + GetName() + "Clear()\n{\n";
        str += "    " + GetName() + "s.clear();\n}\n\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafClearStatement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "";
        break;
    }
    case 1:
    {
        str += "";
        break;
    }
    case 2:
    {
        str += "    void " + GetName() + "Clear();\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafCountImplement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "";
        break;
    }
    case 1:
    {
        str += "";
        break;
    }
    case 2:
    {
        str += "qint32 " + ParentClass()->GetDataType() + "Base::" + GetName() + "Count()\n{\n";
        str += "    return " + GetName() + "s.count();\n}\n\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafCountStatement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "";
        break;
    }
    case 1:
    {
        str += "";
        break;
    }
    case 2:
    {
        str += "    qint32 " + GetName() + "Count();\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafGetImplement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += LeafType() + " " + ParentClass()->GetDataType() + "Base::Get" + GetName() + "()\n{\n";
        str += "    return " + GetName() + ";\n}\n\n";
        break;
    }
    case 1:
    {
        str += LeafType() + " " + ParentClass()->GetDataType() + "Base::Get" + GetName() + "()\n{\n";
        str += "    return " + GetName() + ";\n}\n\n";
        break;
    }
     case 2:
    {
        str += "QVector <" + GetDataType() + "> " + ParentClass()->GetDataType() + "Base::Get" + GetName() + "()\n{\n";
        str += "    return " + GetName() + "s;\n}\n\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafGetStatement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "    " + LeafType() + " Get" + GetName() + "();\n";
        break;
    }
    case 1:
    {
        str += "    " + LeafType() + " Get" + GetName() + "();\n";
        break;
    }
    case 2:
    {
        str += "    QVector <" + LeafType() + "> Get" + GetName() + "();\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafSetImplement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::Set" + GetName() + "(" + LeafType() + " _value)\n{\n";
        str += "    if(!" + GetName() + "Exsit)\n    {\n";
        str += "        " + GetName() + "Exsit = true;\n    }\n";
        str += "    " + GetName() + " = _value;\n}\n\n";
        break;
    }
    case 1:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::Set" + GetName() + "(" + LeafType() + " _value)\n{\n";
        str += "    " + GetName() + " = _value;\n}\n\n";
        break;
    }
     case 2:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::Set" + GetName() + "(QVector <" + LeafType() + "> _value)\n{\n";
        str += "    " + GetName() + "Clear;\n";
        str += "    " + GetName() + "s.append(_value);\n}\n\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafSetStatement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "    void Set" + GetName() + "(" + LeafType() + " _value);\n";
        break;
    }
    case 1:
    {
        str += "    void Set" + GetName() + "(" + LeafType() + " _value);\n";
        break;
    }
    case 2:
    {
        str += "    void Set" + GetName() + "(QVector <" + LeafType() + "> _value);\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafRemoveImplement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::Reomve" + GetName() + "()\n{\n";
        str += "    " + GetName() + "Exsit = false;\n}\n\n";
        break;
    }
    case 1:
    {
        str += "";
        break;
    }
    case 2:
    {
        str += "void " + ParentClass()->GetDataType() + "Base::Reomve" + GetName() + "(" + LeafType() + " _value)\n{\n";
        str += "    " + GetName() + "s.removeOne(_value);\n}\n\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafRemoveStatement()
{
    QString str = "";
    switch (GetNumber())
    {
    case 0:
    {
        str += "    void Reomve" + GetName() + "();\n";
        break;
    }
    case 1:
    {
        str += "";
        break;
    }
    case 2:
    {
        str += "    void Reomve" + GetName() + "(" + LeafType() + " _value);\n";
        break;
    }
    }
    return str;
}

QString WXML2CodeChild::LeafBaseImplements()
{
    QString str = "";
    str += LeafAddImplement();
    str += LeafCountImplement();
    str += LeafGetImplement();
    str += LeafSetImplement();
    str += LeafRemoveImplement();
    str += LeafClearImplement();
    return str;
}

QString WXML2CodeChild::LeafProtectedStatements()
{
    QString str = "";
    str += LeafFieldStatement();
    return str;
}

QString WXML2CodeChild::LeafPublicStatements()
{
    QString str = "";
    str += LeafAddStatement();
    str += LeafCountStatement();
    str += LeafGetStatement();
    str += LeafSetStatement();
    str += LeafRemoveStatement();
    str += LeafClearStatement();
    return str;
}

QString WXML2CodeChild::LeafType()
{
 return XMLTypes2CppType(String2XMLTypes(GetDataType()));
}

WXML2CodeClass* WXML2CodeChild::ParentClass()
{
    return dynamic_cast<WXML2CodeClass*>(this->parent());
}

QString WXML2CodeChild::ConvertStr()
{
    return XMLConvertStr(String2XMLTypes(GetDataType()), GetName() + "tmp");
}

QString WXML2CodeChild::ConvertXML()
{
    return StrConvertXML(String2XMLTypes(GetDataType()), GetName() + "tmp");
}

XMLNodeType WXML2CodeChild::NodeType()
{
    QString path = GetPath();
    if(path[0] == "@")
    {
        return AttributeNode;
    }
    else if(path[0] == "~")
    {
        return CDATASectionNode;
    }
    else if(path == "")
    {
        return TextNode;
    }
    return ElementNode;
}

QString WXML2CodeChild::ReadAttribute()
{
    QString str = "";
    if(NodeType() == AttributeNode)
    {
        str += "        else if(tmp.nodeName() == \"" +  GetPath().remove(0, 1) + "\")\n        {\n";
        str += "        QString " + GetName() + "tmp = tmp.nodeValue();\n";
        str += "        " + GetName() + " = " + ConvertXML() + ";\n        }\n";
    }
    return  str;
}

QString WXML2CodeChild::ReadChild()
{
    QString str = "";
    if(NodeType() == ElementNode)
    {
        if(GetLeaf())
        {
            str += "        else if(tmp.nodeName() == \"" +  GetPath() + "\")\n        {\n";
            switch (GetNumber())
            {
            case 0:
            {
                str += "            QString " + GetName() + "tmp = tmp.toElement().text();\n";
                str += "            " + GetName() + "Exsit = true;\n";
                str += "            " + GetName() + " = " + ConvertXML() + ";\n        }\n";
                break;
            }
            case 1:
            {
                str += "            QString " + GetName() + "tmp = tmp.toElement().text();\n";
                str += "            " + GetName() + " = " + ConvertXML() + ";\n        }\n";
                break;
            }
            case 2:
            {
                str += "            QString " + GetName() + "tmp = tmp.toElement().text();\n";
                str += "            " + GetName() + "s.append(" + ConvertXML() +");\n        }\n";
                break;
            }
            }
        }
        else
        {
            str += "        else if(tmp.nodeName() == \"" +  GetPath() + "\")\n        {\n";
            switch (GetNumber())
            {
            case 0:
            {
                str += "            Add" + GetName() + "()->FromXML(tmp);\n";
                str += "            " + GetName() + "Exsit = true;\n        }\n";
                break;
            }
            case 1:
            {
                str += "            Get" + GetName() + "()->FromXML(tmp);\n        }\n";
                break;
            }
            case 2:
            {
                str += "            Add" + GetName() + "()->FromXML(tmp);\n        }\n";
                break;
            }
            }
        }
    }
    else if(NodeType() == CDATASectionNode)
    {
        if(GetLeaf())
        {
            str += "        else if(tmp.nodeName() == \"" +  GetPath().remove(0, 1) + "\")\n        {\n";
            switch (GetNumber())
            {
            case 0:
            {
                str += "            QString " + GetName() + "tmp = tmp.toElement().text();\n";
                str += "            " + GetName() + "Exsit = true;\n";
                str += "            " + GetName() + " = " + ConvertXML() + ";\n        }\n";
                break;
            }
            case 1:
            {
                str += "            QString " + GetName() + "tmp = tmp.toElement().text();\n";
                str += "            " + GetName() + " = " + ConvertXML() + ";\n        }\n";
                break;
            }
            case 2:
            {
                str += "            QString " + GetName() + "tmp = tmp.toElement().text();\n";
                str += "            " + GetName() + "s.append(" + ConvertXML() +");\n        }\n";
                break;
            }
            }
        }
    }
    return  str;
}

QString WXML2CodeChild::WriteAttribute()
{
    QString str = "";
    if(NodeType() == AttributeNode)
    {
        str += "    QDomAttr " + GetName() + "Att=doc.createAttribute(\"" + GetPath().remove(0, 1) + "\");\n";
        str += "    " + LeafType() + " " + GetName() + "tmp = " + GetName() + ";\n";
        str += "    " + GetName() + "Att.setValue(" + ConvertStr() + ");\n";
        str += "    node.setAttributeNode(" + GetName() + "Att);\n";
    }
    return  str;
}

QString WXML2CodeChild::WriteChild()
{
    QString str = "";
    if(NodeType() == ElementNode)
    {
        if(GetLeaf())
        {
            switch (GetNumber())
            {
            case 0:
            {
                str += "    if(" + GetName() + "Exsit)\n    {\n";
                str += "        QDomElement " + GetName() + "node = doc.createElement(\"" +  GetPath() + "\");\n";
                str += "        node.appendChild(" + GetName() + "node);\n";
                str += "        " + LeafType() + " " + GetName() + "tmp = " + GetName() + ";\n";
                str += "        " + GetName() + "node.appendChild(doc.createTextNode(" + ConvertStr() + "));\n    }\n";
                break;
            }
            case 1:
            {
                str += "    QDomElement " + GetName() + "node = doc.createElement(\"" +  GetPath() + "\");\n";
                str += "    node.appendChild(" + GetName() + "node);\n";
                str += "    " + LeafType() + " " + GetName() + "tmp = " + GetName() + ";\n";
                str += "    " + GetName() + "node.appendChild(doc.createTextNode(" + ConvertStr() + "));\n";
                break;
            }
            case 2:
            {
                str += "    for(int i=0; i<" + GetName() + "Count(); i++)\n    {\n";
                str += "        QDomElement " + GetName() + "text = doc.createElement(\"" +  GetPath() + "\");\n";
                str += "        " + LeafType() + " " + GetName() + "tmp = " + GetName() + "s[i];\n";
                str += "        node.appendChild(" + GetName() + "node);\n";
                str += "        " + GetName() + "node.appendChild(doc.createTextNode(" + ConvertStr() + "));\n    }\n";
                break;
            }
            }
        }
        else
        {
            switch (GetNumber())
            {
            case 0:
            {
                str += "    if(" + GetName() + "Exsit)\n    {\n";
                str += "        Get" + GetName() + "()->ToXML(node);\n    }\n";
                break;
            }
            case 1:
            {
                str += "    Get" + GetName() + "()->ToXML(node);\n";
                break;
            }
            case 2:
            {
                str += "    for(int i=0; i<" + GetName() + "Count(); i++)\n    {\n";
                str += "        " + GetName() + "s[i]->ToXML(node);\n    }\n";
                break;
            }
            }
        }
    }
    else if(NodeType() == CDATASectionNode)
    {
        switch (GetNumber())
        {
        case 0:
        {
            str += "    if(" + GetName() + "Exsit)\n    {\n";
            str += "        QDomElement " + GetName() + "node = doc.createElement(\"" +  GetPath().remove(0, 1) + "\");\n";
            str += "        node.appendChild(" + GetName() + "node);\n";
            str += "        " + LeafType() + " " + GetName() + "tmp = " + GetName() + ";\n";
            str += "        " + GetName() + "node.appendChild(doc.createCDATASection(" + ConvertStr() + "));\n    }\n";
            break;
        }
        case 1:
        {
            str += "    QDomElement " + GetName() + "node = doc.createElement(\"" +  GetPath().remove(0, 1) + "\");\n";
            str += "    node.appendChild(" + GetName() + "node);\n";
            str += "    " + LeafType() + " " + GetName() + "tmp = " + GetName() + ";\n";
            str += "    " + GetName() + "node.appendChild(doc.createCDATASection(" + ConvertStr() + "));\n";
            break;
        }
        case 2:
        {
            str += "    for(int i=0; i<" + GetName() + "Count(); i++)\n    {\n";
            str += "        QDomElement " + GetName() + "text = doc.createElement(\"" +  GetPath().remove(0, 1) + "\");\n";
            str += "        " + LeafType() + " " + GetName() + "tmp = " + GetName() + "s[i];\n";
            str += "        node.appendChild(" + GetName() + "node);\n";
            str += "        " + GetName() + "node.appendChild(doc.createCDATASection(" + ConvertStr() + "));\n    }\n";
            break;
        }
        }
    }
    return  str;
}

