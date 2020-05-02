#ifndef XMLCONVERT_H
#define XMLCONVERT_H
#include <QString>

enum XMLNodeType {
    ElementNode               = 1,
    AttributeNode             = 2,
    TextNode                  = 3,
    CDATASectionNode          = 4,
    EntityReferenceNode       = 5,
    EntityNode                = 6,
    ProcessingInstructionNode = 7,
    CommentNode               = 8,
    DocumentNode              = 9,
    DocumentTypeNode          = 10,
    DocumentFragmentNode      = 11,
    NotationNode              = 12,
    BaseNode                  = 21,// this is not in the standard
    CharacterDataNode         = 22 // this is not in the standard
};

enum  XMLTypes{xs_ENTITIES, xs_ENTITY, xs_ID, xs_IDREF, xs_IDREFSlanguage, xs_Name, xs_NCName, xs_NMTOKEN, xs_NMTOKENS, xs_normalizedString, xs_QName,
  xs_string, xs_token, xs_date, xs_time, xs_dateTime, xs_duration, xs_byte, xs_decimal, xs_int, xs_integer, xs_long, xs_negativeInteger,
  xs_nonNegativeInteger, xs_nonPositiveInteger, xs_positiveInteger, xs_short, xs_unsignedLong, xs_unsignedInt, xs_unsignedShort, xs_unsignedByte, xs_anyURI,
  xs_base64Binary, xs_boolean, xs_double, xs_float, xs_hexBinary, xs_NOTATION, xs_Class, xml_TNewComplex, xml_ArrayCoordinates, xml_Array1D, xml_Array1I,
  xml_Matrx2D, xml_Matrx2I, xml_Array1B, xml_Array1BL, xml_Matrx2B, xml_Matrx2BL, W_Point2I, W_Point2D, W_Point3I, W_Point3D, W_Point2Is, W_Point2Ds,
  W_Point3Is, W_Point3Ds,xml_Pointer};

XMLTypes String2XMLTypes(QString _value);
QString XMLTypes2CppType(XMLTypes _value);

QString XMLConvertStr(XMLTypes _value, QString name);
QString StrConvertXML(XMLTypes _value, QString name);

bool Str2Bool(QString str);
QString Bool2Str(bool bl);

#endif // XMLCONVERT_H
