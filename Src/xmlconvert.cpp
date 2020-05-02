#include "xmlconvert.h"


XMLTypes String2XMLTypes(QString _value)
{
    if(_value.toLower() == "xs:entities")
    {
       return xs_ENTITIES;
    }
    else if(_value.toLower() == "xs:entity")
    {
       return xs_ENTITY;
    }
    else if(_value.toLower() == "xs:id")
    {
       return xs_ID;
    }
    else if(_value.toLower() == "xs:idref")
    {
       return xs_IDREF;
    }
    else if(_value.toLower() == "xs:idrefslanguage")
    {
       return xs_IDREFSlanguage;
    }
    else if(_value.toLower() == "xs:name")
    {
       return xs_Name;
    }
    else if(_value.toLower() == "xs:ncname")
    {
       return xs_NCName;
    }
    else if(_value.toLower() == "xs:nmtoken")
    {
       return xs_NMTOKEN;
    }
    else if(_value.toLower() == "xs:nmtokens")
    {
       return xs_NMTOKENS;
    }
    else if(_value.toLower() == "xs:normalizedstring")
    {
       return xs_normalizedString;
    }
    else if(_value.toLower() == "xs:qname")
    {
       return xs_QName;
    }
    else if(_value.toLower() == "xs:string")
    {
       return xs_string;
    }
    else if(_value.toLower() == "xs:token")
    {
       return xs_token;
    }
    else if(_value.toLower() == "xs:date")
    {
       return xs_date;
    }
    else if(_value.toLower() == "xs:time")
    {
       return xs_time;
    }
    else if(_value.toLower() == "xs:datetime")
    {
       return xs_dateTime;
    }
    else if(_value.toLower() == "xs:duration")
    {
       return xs_duration;
    }
    else if(_value.toLower() == "xs:byte")
    {
       return xs_byte;
    }
    else if(_value.toLower() == "xs:decimal")
    {
       return xs_decimal;
    }
    else if(_value.toLower() == "xs:int")
    {
       return xs_int;
    }
    else if(_value.toLower() == "xs:integer")
    {
       return xs_integer;
    }
    else if(_value.toLower() == "xs:long")
    {
       return xs_long;
    }
    else if(_value.toLower() == "xs:negativeinteger")
    {
       return xs_negativeInteger;
    }
    else if(_value.toLower() == "xs:nonnegativeinteger")
    {
       return xs_nonNegativeInteger;
    }
    else if(_value.toLower() == "xs:nonpositiveinteger")
    {
       return xs_nonPositiveInteger;
    }
    else if(_value.toLower() == "xs:positiveinteger")
    {
       return xs_positiveInteger;
    }
    else if(_value.toLower() == "xs:short")
    {
       return xs_short;
    }
    else if(_value.toLower() == "xs:unsignedlong")
    {
       return xs_unsignedLong;
    }
    else if(_value.toLower() == "xs:unsignedint")
    {
       return xs_unsignedInt;
    }
    else if(_value.toLower() == "xs:unsignedshort")
    {
       return xs_unsignedShort;
    }
    else if(_value.toLower() == "xs:unsignedbyte")
    {
       return xs_unsignedByte;
    }
    else if(_value.toLower() == "xs:anyuri")
    {
       return xs_anyURI;
    }
    else if(_value.toLower() == "xs:base64binary")
    {
       return xs_base64Binary;
    }
    else if(_value.toLower() == "xs:boolean")
    {
       return xs_boolean;
    }
    else if(_value.toLower() == "boolean")
    {
       return xs_boolean;
    }
    else if(_value.toLower() == "bool")
    {
       return xs_boolean;
    }
    else if(_value.toLower() == "xs:double")
    {
       return xs_double;
    }
    else if(_value.toLower() == "xs:float")
    {
       return xs_float;
    }
    else if(_value.toLower() == "xs:hexbinary")
    {
       return xs_hexBinary;
    }
    else if(_value.toLower() == "xs:notation")
    {
       return xs_NOTATION;
    }
    else if(_value.toLower() == "string")
    {
       return xs_string;
    }
    else if(_value.toLower() == "integer")
    {
       return xs_integer;
    }
    else if(_value.toLower() == "double")
    {
       return xs_double;
    }
    else if(_value.toLower() == "float")
    {
       return xs_float;
    }
    else if(_value.toLower() == "real")
    {
       return xs_double;
    }
    else if(_value.toLower() == "tdatetime")
    {
       return xs_dateTime;
    }
    else if(_value.toLower() == "tnewcomplex")
    {
       return xml_TNewComplex;
    }
    else if(_value.toLower() == "arraycoordinates")
    {
       return xml_ArrayCoordinates;
    }
    else if(_value.toLower() == "tarray1d")
    {
       return xml_Array1D;
    }
    else if(_value.toLower() == "tarray1i")
    {
       return xml_Array1I;
    }
    else if(_value.toLower() == "tmatrx2d")
    {
       return xml_Matrx2D;
    }
    else if(_value.toLower() == "tmatrx2i")
    {
       return xml_Matrx2I;
    }
    else if(_value.toLower() == "tarray1b")
    {
       return xml_Array1B;
    }
    else if(_value.toLower() == "tarray1bl")
    {
       return xml_Array1BL;
    }
    else if(_value.toLower() == "tmatrx2b")
    {
       return xml_Matrx2B;
    }
    else if(_value.toLower() == "tmatrx2bl")
    {
       return xml_Matrx2BL;
    }
    else if(_value.toLower() == "wpoint2i")
    {
       return W_Point2I;
    }
    else if(_value.toLower() == "wpoint2d")
    {
       return W_Point2D;
    }
    else if(_value.toLower() == "wpoint3i")
    {
       return W_Point3I;
    }
    else if(_value.toLower() == "wpoint3d")
    {
       return W_Point3D;
    }
    else if(_value.toLower() == "wpoint2is")
    {
       return W_Point2Is;
    }
    else if(_value.toLower() == "wpoint2ds")
    {
       return W_Point2Ds;
    }
    else if(_value.toLower() == "wpoint3is")
    {
       return W_Point3Is;
    }
    else if(_value.toLower() == "wpoint3ds")
    {
       return W_Point3Ds;
    }
    else if(_value.toLower() == "pointer")
    {
       return xml_Pointer;
    }
    else
    {
      return xs_Class;
    }
}

QString XMLTypes2CppType(XMLTypes _value)
{
    switch (_value)
    {
    case xs_ENTITIES:
    {
        return "QString";
    }
    case xs_ENTITY:
    {
        return "QString";
    }
    case xs_ID:
    {
        return "QString";
    }
    case xs_IDREF:
    {
        return "QString";
    }
    case xs_IDREFSlanguage:
    {
        return "QString";
    }
    case xs_Name:
    {
        return "QString";
    }
    case xs_NCName:
    {
        return "QString";
    }
    case xs_NMTOKEN:
    {
        return "QString";
    }
    case xs_NMTOKENS:
    {
        return "QString";
    }
    case xs_normalizedString:
    {
        return "QString";
    }
    case xs_QName:
    {
        return "QString";
    }
    case xs_string:
    {
        return "QString";
    }
    case xs_token:
    {
        return "QString";
    }
    case xs_date:
    {
        return "QDate";
    }
    case xs_time:
    {
        return "QTime";
    }
    case xs_dateTime:
    {
        return "QDateTime";
    }
    case xs_duration:
    {
        return "QTime";
    }
    case xs_byte:
    {
        return "QChar";
    }
    case xs_decimal:
    {
        return "qreal";
    }
    case xs_int:
    {
        return "qint32";
    }
    case xs_integer:
    {
        return "qint32";
    }
    case xs_long:
    {
        return "qint64";
    }
    case xs_negativeInteger:
    {
        return "qint32";
    }
    case xs_nonNegativeInteger:
    {
        return "quint32";
    }
    case xs_nonPositiveInteger:
    {
        return "qint32";
    }
    case xs_positiveInteger:
    {
        return "quint32";
    }
    case xs_short:
    {
        return "qint16";
    }
    case xs_unsignedLong:
    {
        return "quint64";
    }
    case xs_unsignedInt:
    {
        return "quint32";
    }
    case xs_unsignedShort:
    {
        return "quint16";
    }
    case xs_unsignedByte:
    {
        return "quint8";
    }
    case xs_anyURI:
    {
        return "QString";
    }
    case xs_base64Binary:
    {
        return "QString";
    }
    case xs_boolean:
    {
        return "bool";
    }
    case xs_double:
    {
        return "qreal";
    }
    case xs_float:
    {
        return "qreal";
    }
    case xs_hexBinary:
    {
        return "QString";
    }
    case xs_NOTATION:
    {
        return "QString";
    }
    case xs_Class:
    {
        return "Class";
    }
    case xml_TNewComplex:
    {
        return "std::complex<double>";
    }
    case xml_ArrayCoordinates:
    {
        return "QVector<QPoint3F>";
    }
    case xml_Array1D:
    {
        return "QVector<qreal>";
    }
    case xml_Array1I:
    {
        return "QVector<qint32>";
    }
    case xml_Matrx2D:
    {
        return "QVector<QVector<qreal>>";
    }
    case xml_Matrx2I:
    {
        return "QVector<QVector<qint32>>";
    }
    case xml_Array1B:
    {
        return "QByteArray";
    }
    case xml_Array1BL:
    {
        return "QVector<bool>";
    }
    case xml_Matrx2B:
    {
        return "QVector<QByteArray>";
    }
    case xml_Matrx2BL:
    {
        return "QVector<QVector<bool>>";
    }
    case W_Point2I:
    {
        return "QPoint";
    }
    case W_Point2D:
    {
        return "QPointF";
    }
    case W_Point3I:
    {
        return "QPoint3";
    }
    case W_Point3D:
    {
        return "QPoint3F";
    }
    case W_Point2Is:
    {
        return "QVector<QPoint>";
    }
    case W_Point2Ds:
    {
        return "QVector<QPointF>";
    }
    case W_Point3Is:
    {
        return "QVector<QPoint3>";
    }
    case W_Point3Ds:
    {
        return "QVector<QPoint3F>";
    }
    case xml_Pointer:
    {
        return "QPointer";
    }
    }
}

bool Str2Bool(QString str)
{
    if(str=="1" || str.toLower() == "true")
    {
      return true;
    }
    else
    {
        return  false;
    }
}

QString Bool2Str(bool bl)
{
    if(bl)
    {
      return QString("1");
    }
    else
    {
      return QString("0");
    }
}


QString StrConvertXML(XMLTypes _value, QString name)
{
    switch (_value)
    {
    case xs_ENTITIES:
    {
        return name;
    }
    case xs_ENTITY:
    {
        return name;
    }
    case xs_ID:
    {
        return name;
    }
    case xs_IDREF:
    {
        return name;
    }
    case xs_IDREFSlanguage:
    {
        return name;
    }
    case xs_Name:
    {
        return name;
    }
    case xs_NCName:
    {
        return name;
    }
    case xs_NMTOKEN:
    {
        return name;
    }
    case xs_NMTOKENS:
    {
        return name;
    }
    case xs_normalizedString:
    {
        return name;
    }
    case xs_QName:
    {
        return name;
    }
    case xs_string:
    {
        return name;
    }
    case xs_token:
    {
        return name;
    }
    case xs_date:
    {
        return "QDate::fromString(" + name + ")";
    }
    case xs_time:
    {
        return "QTime::fromString(" + name + ")";
    }
    case xs_dateTime:
    {
        return "QDateTime::fromString(" + name + ")";
    }
    case xs_duration:
    {
        return "QTime::fromString(" + name + ")";
    }
    case xs_byte:
    {
        return "QByteArray(" + name + ")";
    }
    case xs_decimal:
    {
        return "" + name + ".toDouble()";
    }
    case xs_int:
    {
        return "" + name + ".toInt()";
    }
    case xs_integer:
    {
        return "" + name + ".toInt()";
    }
    case xs_long:
    {
        return "" + name + ".toLong()";
    }
    case xs_negativeInteger:
    {
        return "" + name + ".toInt()";
    }
    case xs_nonNegativeInteger:
    {
        return "" + name + ".toUInt()";
    }
    case xs_nonPositiveInteger:
    {
        return "" + name + ".toInt()";
    }
    case xs_positiveInteger:
    {
        return "" + name + ".toUInt()";
    }
    case xs_short:
    {
        return "" + name + ".toShort()";
    }
    case xs_unsignedLong:
    {
        return "" + name + ".toULong()";
    }
    case xs_unsignedInt:
    {
        return "" + name + ".toUInt()";
    }
    case xs_unsignedShort:
    {
        return "" + name + ".toUShort()";
    }
    case xs_unsignedByte:
    {
        return "" + name + ".toUInt()";
    }
    case xs_anyURI:
    {
        return name;
    }
    case xs_base64Binary:
    {
        return name;
    }
    case xs_boolean:
    {
        return "Str2Bool(" + name + ")";
    }
    case xs_double:
    {
        return "" + name + ".toDouble()";
    }
    case xs_float:
    {
        return "" + name + ".toFloat()";
    }
    case xs_hexBinary:
    {
        return name;
    }
    case xs_NOTATION:
    {
        return name;
    }
    case xs_Class:
    {
        return name;
    }
    case xml_TNewComplex:
    {
        return name;
    }
    case xml_ArrayCoordinates:
    {
        return "Str2Point3Fs(" + name + ")";
    }
    case xml_Array1D:
    {
        return name;
    }
    case xml_Array1I:
    {
        return name;
    }
    case xml_Matrx2D:
    {
        return name;
    }
    case xml_Matrx2I:
    {
        return name;
    }
    case xml_Array1B:
    {
        return name;
    }
    case xml_Array1BL:
    {
        return name;
    }
    case xml_Matrx2B:
    {
        return name;
    }
    case xml_Matrx2BL:
    {
        return name;
    }
    case W_Point2I:
    {
        return name;
    }
    case W_Point2D:
    {
        return name;
    }
    case W_Point3I:
    {
        return name;
    }
    case W_Point3D:
    {
        return name;
    }
    case W_Point2Is:
    {
        return name;
    }
    case W_Point2Ds:
    {
        return name;
    }
    case W_Point3Is:
    {
        return name;
    }
    case W_Point3Ds:
    {
        return name;
    }
    case xml_Pointer:
    {
        return name;
    }
    }
    return "";
}

QString XMLConvertStr(XMLTypes _value, QString name)
{
    switch (_value)
    {
    case xs_ENTITIES:
    {
        return name;
    }
    case xs_ENTITY:
    {
        return name;
    }
    case xs_ID:
    {
        return name;
    }
    case xs_IDREF:
    {
        return name;
    }
    case xs_IDREFSlanguage:
    {
        return name;
    }
    case xs_Name:
    {
        return name;
    }
    case xs_NCName:
    {
        return name;
    }
    case xs_NMTOKEN:
    {
        return name;
    }
    case xs_NMTOKENS:
    {
        return name;
    }
    case xs_normalizedString:
    {
        return name;
    }
    case xs_QName:
    {
        return name;
    }
    case xs_string:
    {
        return name;
    }
    case xs_token:
    {
        return name;
    }
    case xs_date:
    {
        return "" + name + ".toString()";
    }
    case xs_time:
    {
        return "" + name + ".toString()";
    }
    case xs_dateTime:
    {
        return "" + name + ".toString()";
    }
    case xs_duration:
    {
        return "" + name + ".toString()";
    }
    case xs_byte:
    {
        return "" + name + ".toString()";
    }
    case xs_decimal:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_int:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_integer:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_long:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_negativeInteger:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_nonNegativeInteger:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_nonPositiveInteger:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_positiveInteger:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_short:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_unsignedLong:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_unsignedInt:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_unsignedShort:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_unsignedByte:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_anyURI:
    {
        return name;
    }
    case xs_base64Binary:
    {
        return name;
    }
    case xs_boolean:
    {
        return "Bool2Str(" + name + ")";
    }
    case xs_double:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_float:
    {
        return "QString::number(" + name + ", 'g', 16)";
    }
    case xs_hexBinary:
    {
        return name;
    }
    case xs_NOTATION:
    {
        return name;
    }
    case xs_Class:
    {
        return name;
    }
    case xml_TNewComplex:
    {
        return name;
    }
    case xml_ArrayCoordinates:
    {
        return "Point3Fs2Str(" + name + ")";
    }
    case xml_Array1D:
    {
        return name;
    }
    case xml_Array1I:
    {
        return name;
    }
    case xml_Matrx2D:
    {
        return name;
    }
    case xml_Matrx2I:
    {
        return name;
    }
    case xml_Array1B:
    {
        return name;
    }
    case xml_Array1BL:
    {
        return name;
    }
    case xml_Matrx2B:
    {
        return name;
    }
    case xml_Matrx2BL:
    {
        return name;
    }
    case W_Point2I:
    {
        return name;
    }
    case W_Point2D:
    {
        return name;
    }
    case W_Point3I:
    {
        return name;
    }
    case W_Point3D:
    {
        return name;
    }
    case W_Point2Is:
    {
        return name;
    }
    case W_Point2Ds:
    {
        return name;
    }
    case W_Point3Is:
    {
        return name;
    }
    case W_Point3Ds:
    {
        return name;
    }
    case xml_Pointer:
    {
        return name;
    }
    }
    return "";
}
