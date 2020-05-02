#ifndef XML2CODETOOL_H
#define XML2CODETOOL_H

#include <QWidget>
#include "xml2codebase.h"
#include "xml2code.h"

QT_BEGIN_NAMESPACE
namespace Ui { class XML2CodeTool; }
QT_END_NAMESPACE

class XML2CodeTool : public QWidget
{
    Q_OBJECT

public:
    XML2CodeTool(QWidget *parent = nullptr);
    ~XML2CodeTool();

private:
    Ui::XML2CodeTool *ui;
};
#endif // XML2CODETOOL_H
