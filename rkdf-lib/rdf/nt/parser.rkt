#lang brag
ntDoc          : triple*
triple         : subject predicate object /PERIOD
@subject       : uriref | nodeID
@predicate     : uriref
@object        : uriref | literal | nodeID
uriref         : URIREF
@literal       : bareString | langString | datatypeString
nodeID         : NODE
bareString     : STRING
langString     : STRING LANG
datatypeString : STRING /DATATYPE URIREF
