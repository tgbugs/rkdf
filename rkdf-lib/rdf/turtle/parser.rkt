#lang brag
turtleDoc            : statement*
@statement           : directive /end | triples /end | /ws+
@end                 : ws* PERIOD
@directive           : prefixID | baseID
prefixID             : /PREFIX /ws+ prefixName* /COLON /ws+ uriref
baseID               : /BASE /ws+ uriref
triples              : subject predicateObjectList /ws*
@predicateObjectList : polist ( /SEMICOLON /ws* polist )* /SEMICOLON* /ws*
polist               : verb objectList
@objectList          : object | olist
@olist               : object ( /ws* /COMMA /ws* object )*
verb                 : predicate | type /ws+
@type                : CHAR-AL
@comment             : COMMENT
@subject             : resource | blank /ws*
@predicate           : resource
@object              : resource | blank /ws* | literal /ws*
@literal             : bareString | langString | datatypeString | integer | double | decimal | boolean
bareString           : quotedString
datatypeString       : quotedString /DATATYPE resource
langString           : quotedString /AMP language
integer              : ("-" | "+")* 0-9+
double               : ("-" | "+")* ( 0-9+ PERIOD 0-9* exponent | PERIOD 0-9+ exponent | 0-9+ exponent )
decimal              : ("-" | "+")* ( 0-9+ PERIOD 0-9* | PERIOD 0-9+ | 0-9+ )
@exponent            : (CHAR-EL | CHAR-EU) ("-" | "+")* 0-9+
boolean              : TRUE | FALSE
@blank               : nodeID | OSBCSB | square | collection
square               : /OSB /ws* predicateObjectList /ws* /CSB
@itemList            : /ws* object+
collection           : /OP itemList* /CP
@ws                  : TAB | NEWLINE | CR | SPACE | comment
resource             : uriref | qname
nodeID               : "_:" name
@qname               : qnamef | qnameb
qnamef               : prefixName+ /COLON name+ /ws*
qnameb               : /COLON name+ /ws*
@uriref              : URIREF /ws*
language             : lower+ ("-" (lower | 0-9)+ )*
@lower               : CHAR-AL | CHAR-EL | LOWER
@nameStartChar       : lower | CHAR-EU | NSCHAR
@nameChar            : nameStartChar | "-" | 0-9 | PERIOD
name                 : nameChar*
prefixName           : nameStartChar nameChar*
relativeURI          : charsafe*
@charsafe            : ( nameStartChar | 0-9 | CHARSAFE | COLON | PERIOD | COMMA | SPACE
                                       | OSB | CSB | OP | CP | DATATYPE | AMP )
@chars               : charsafe*
quotedString         : string | longString
@string              : STRING
@longString          : LONGSTRING
character            : CHAR
echaracter           : character | TAB | NEWLINE | CR
hex                  : 0-9 | A-F  ; 0-9 A-F
scharacter           : ( echaracter -  DQUOTE) | DQUOTE
lcharacter           : echaracter | "\"" | TAB | NEWLINE | CR
