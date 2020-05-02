#include "xml2codebase.h"
#include "xml2code.h"

WXML2CodeBase::WXML2CodeBase(QObject *parent) : WXML(parent)
{

}

WXML2CodeBase::~WXML2CodeBase()
{
    qDeleteAll(XMLClasss);
}

WXML2CodeClass* WXML2CodeBase::AddXMLClass()
{
    WXML2CodeClass *cls = new WXML2CodeClass(this);
    XMLClasss.append(cls);
    return cls;
}

void WXML2CodeBase::RemoveXMLClass(WXML2CodeClass *_value)
{
    XMLClasss.removeOne(_value);
    delete &_value;
}

void WXML2CodeBase::DeleteXMLClass(qint32 index)
{
    delete XMLClasss[index];
    XMLClasss.remove(index);
}

WXML2CodeClass* WXML2CodeBase::XMLClass(qint32 index)
{
    return this->XMLClasss[index];
}

qint32 WXML2CodeBase::XMLClassCount()
{
    return this->XMLClasss.count();
}

void WXML2CodeBase::FromXML(QDomNode node)
{
    for(int i=0; i<node.childNodes().count(); i++)
    {
        QDomNode tmp = node.childNodes().at(i);
        if(tmp.nodeName() == "XMLClass")
        {
            WXML2CodeClass *cls = new WXML2CodeClass(this);
            cls->FromXML(tmp);
            XMLClasss.append(cls);
        }
    }
    for(int i=0; i<node.attributes().count(); i++)
    {
        QDomNode tmp = node.attributes().item(i);
        if(tmp.nodeName() == "name")
        {
            Name = tmp.nodeValue();
        }
    }
}

QDomNode WXML2CodeBase::ToXML(QDomNode par)
{
    QDomDocument doc = par.ownerDocument();
    QDomElement  node = doc.createElement("XML2Code");
    par.appendChild(node);
    QDomAttr NameAtt=doc.createAttribute("name");
    NameAtt.setValue(Name);
    node.setAttributeNode(NameAtt);
    for(int i=0; i<XMLClasss.count(); i++)
    {
        XMLClasss[i]->ToXML(node);
    }
    return node;
}

QString WXML2CodeBase::GetName()
{
    return Name;
}

void WXML2CodeBase::SetName(QString _value)
{
    Name = _value;
}

WXML2CodeClassBase::WXML2CodeClassBase(QObject *parent) : WXML(parent)
{

}

WXML2CodeClassBase::~WXML2CodeClassBase()
{
    qDeleteAll(Childs);
}

WXML2CodeChild *WXML2CodeClassBase::Child(qint32 index)
{
    return Childs[index];
}

qint32 WXML2CodeClassBase::ChildCount()
{
    return Childs.count();
}

void WXML2CodeClassBase::FromXML(QDomNode node)
{
    for(int i=0; i<node.childNodes().count(); i++)
    {
        QDomNode tmp = node.childNodes().at(i);
        if(tmp.nodeName() == "Child")
        {
            WXML2CodeChild *cld = new WXML2CodeChild(this);
            cld->FromXML(tmp);
            Childs.append(cld);
        }
    }
    for(int i=0; i<node.attributes().count(); i++)
    {
        QDomNode tmp = node.attributes().item(i);
        if(tmp.nodeName() == "type")
        {
            DataType = tmp.nodeValue();
        }
        else if(tmp.nodeName() == "name")
        {
            Name = tmp.nodeValue();
        }
        else if(tmp.nodeName() == "root")
        {
            Root = Str2Bool(tmp.nodeValue());
        }
    }
}

QDomNode WXML2CodeClassBase::ToXML(QDomNode par)
{
    QDomDocument doc = par.ownerDocument();
    QDomElement  node = doc.createElement("XMLClass");
    par.appendChild(node);
    QDomAttr NameAtt=doc.createAttribute("name");
    NameAtt.setValue(Name);
    node.setAttributeNode(NameAtt);
    QDomAttr DataTypeAtt=doc.createAttribute("type");
    DataTypeAtt.setValue(DataType);
    node.setAttributeNode(DataTypeAtt);
    QDomAttr RootAtt=doc.createAttribute("root");
    RootAtt.setValue(Bool2Str(Root));
    node.setAttributeNode(RootAtt);
    for(int i=0; i<Childs.count(); i++)
    {
        Childs[i]->ToXML(node);
    }
    return node;
}

QString WXML2CodeClassBase::GetDataType()
{
    return DataType;
}

void WXML2CodeClassBase::SetDataType(QString _value)
{
    DataType = _value;
}

QString WXML2CodeClassBase::GetName()
{
    return Name;
}

void WXML2CodeClassBase::SetName(QString _value)
{
    Name = _value;
}

WXML2CodeChildBase::WXML2CodeChildBase(QObject *parent) : WXML(parent)
{

}

WXML2CodeChildBase::~WXML2CodeChildBase()
{

}

void WXML2CodeChildBase::FromXML(QDomNode node)
{
    for(int i=0; i<node.attributes().count(); i++)
    {
        QDomNode tmp = node.attributes().item(i);
        if(tmp.nodeName() == "leaf")
        {
            Leaf = Str2Bool(tmp.nodeValue());
        }
        else if(tmp.nodeName() == "visual")
        {
            Visual = Str2Bool(tmp.nodeValue());
        }
        else if(tmp.nodeName() == "name")
        {
            Name = tmp.nodeValue();
        }
        else if(tmp.nodeName() == "type")
        {
            DataType = tmp.nodeValue();
        }
        else if(tmp.nodeName() == "path")
        {
            Path = tmp.nodeValue();
        }
        else if(tmp.nodeName() == "number")
        {
            Number = tmp.nodeValue().toInt();
        }
    }
}

QDomNode WXML2CodeChildBase::ToXML(QDomNode par)
{
    QDomDocument doc = par.ownerDocument();
    QDomElement  node = doc.createElement("Child");
    par.appendChild(node);
    QDomAttr LeafAtt=doc.createAttribute("leaf");
    LeafAtt.setValue(Bool2Str(Leaf));
    node.setAttributeNode(LeafAtt);
    QDomAttr VisualAtt=doc.createAttribute("visual");
    VisualAtt.setValue(Bool2Str(Visual));
    node.setAttributeNode(VisualAtt);
    QDomAttr NameAtt=doc.createAttribute("name");
    NameAtt.setValue(Name);
    node.setAttributeNode(NameAtt);
    QDomAttr DataTypeAtt=doc.createAttribute("type");
    DataTypeAtt.setValue(DataType);
    node.setAttributeNode(DataTypeAtt);
    QDomAttr PathAtt=doc.createAttribute("path");
    PathAtt.setValue(Path);
    node.setAttributeNode(PathAtt);
    QDomAttr NumberAtt=doc.createAttribute("number");
    NumberAtt.setValue(QString::number(Number));
    node.setAttributeNode(NumberAtt);
    return node;
}

bool WXML2CodeChildBase::GetLeaf()
{
    return Leaf;
}

void WXML2CodeChildBase::SetLeaf(bool _value)
{
    Leaf = _value;
}

bool WXML2CodeChildBase::GetVisual()
{
    return Visual;
}

void WXML2CodeChildBase::SetVisual(bool _value)
{
    Visual = _value;
}

QString WXML2CodeChildBase::GetName()
{
    return Name;
}

void WXML2CodeChildBase::SetName(QString _value)
{
    Name = _value;
}

QString WXML2CodeChildBase::GetDataType()
{
    return DataType;
}

void WXML2CodeChildBase::SetDataType(QString _value)
{
    DataType = _value;
}

QString WXML2CodeChildBase::GetPath()
{
    return Path;
}

void WXML2CodeChildBase::SetPath(QString _value)
{
    Path = _value;
}

qint32 WXML2CodeChildBase::GetNumber()
{
    return Number;
}

void WXML2CodeChildBase::SetNumber(qint32 _value)
{
    Number = _value;
}


