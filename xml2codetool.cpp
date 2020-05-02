#include "xml2codetool.h"
#include "ui_xml2codetool.h"

XML2CodeTool::XML2CodeTool(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::XML2CodeTool)
{
    ui->setupUi(this);
    WXML2Code x2c;
    x2c.Load("KML.cml");
    x2c.Save("KML1.cml");
//    x2c.Load("XML2Code.cml");
//    x2c.Save("XML2Code1.cml");
    x2c.ExportBaseCode("D:\\Qt\\build-XML2CodeTool-Desktop_Qt_5_14_1_MSVC2017_64bit-Debug");
    x2c.ExportCode("D:\\Qt\\build-XML2CodeTool-Desktop_Qt_5_14_1_MSVC2017_64bit-Debug");
}

XML2CodeTool::~XML2CodeTool()
{
    delete ui;
}

