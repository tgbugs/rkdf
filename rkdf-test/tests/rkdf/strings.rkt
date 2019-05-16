#lang rdf/turtle
@prefix a: <https://example.org/> .

a:subject a:predicate "string" .
a:subject a:predicate "string with \" in it" .

a:subject a:predicate """longstring""" .
a:subject a:predicate """longstring with \" in it""" .
