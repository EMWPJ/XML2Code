#include "xml2codetool.h"
#include "ui_xml2codetool.h"

XML2CodeTool::XML2CodeTool(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::XML2CodeTool)
{
    ui->setupUi(this);
}

XML2CodeTool::~XML2CodeTool()
{
    delete ui;
}

