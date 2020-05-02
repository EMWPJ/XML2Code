#include "wxml.h"

WXML::WXML(QObject *parent) : QObject(parent)
{

}

WXML::~WXML()
{

}

//void WXML::FromXML(QDomNode node)
//{

//}

//QDomNode WXML::ToXML(QDomNode par)
//{

//}

void WXML::Save(QString fileName)
{
    //打开或创建文件
    QFile file(fileName); //相对路径、绝对路径、资源路径都可以
    if(!file.open(QFile::WriteOnly|QFile::Truncate)) //可以用QIODevice，Truncate表示清空原来的内容
        return;

    QDomDocument doc;
    //写入xml头部
    QDomProcessingInstruction instruction; //添加处理命令
    instruction=doc.createProcessingInstruction("xml","version=\"1.0\" encoding=\"UTF-8\"");
    doc.appendChild(instruction);
    //添加根节点
    this->ToXML(doc);
    //输出到文件
    QTextStream out_stream(&file);
    doc.save(out_stream,2); //缩进4格
    file.close();
}

void WXML::Load(QString fileName)
{
    //打开或创建文件
    QFile file(fileName); //相对路径、绝对路径、资源路径都行
    if(!file.open(QFile::ReadOnly))
        return;

    QDomDocument doc;
    if(!doc.setContent(&file))
    {
        file.close();
        return;
    }
    file.close();
    this->FromXML(doc.documentElement());
}


