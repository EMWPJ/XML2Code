#include "xml2codetool.h"

#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    XML2CodeTool w;
    w.show();
    return a.exec();
}
