#!/usr/bin/env bash
echo loading ...
cat <(echo "#lang racket/base") \
    <(echo "(require rdf/utils)") \
    <(echo "(provide store)") \
    <(echo "(define store (list") \
    <(rapper -i turtle -o ntriples "${1}" |\
          #grep "SCR_013472" scicrunch-registry.nt |\
          #grep "SCR_000871" scicrunch-registry.nt |\
          sed 's/^/(triple\n/' |\
          sed 's/\(^_:\S\+\)\ /\1\n/' |\
          sed 's/\(^<\S\+>\)\ /\1\n/' |\
          sed 's/\(^<\S\+>\)\ /\1\n/' |\
          #sed 's/>\ _:/>\n_:/g' |\
          #sed 's/>\ "/>\n"/g' |\
          sed 's/\ .$/\n./' |\
          sed 's/_:\(\w\+\)$/(BNode "\1")/' |\
          sed 's/^<\(\S\+\)>/(URI "\1")/' |\
          sed 's/\^\^<\(\S\+\)>/(URI "\1")/' |\
          sed 's/^\(".\+"\)\((URI\ \S\+)\)/(Literal \1 \2 null)/' |\
          sed 's/^\(".\+"\)@\(\S\+\)/(Literal \1 null "\2")/' |\
          sed 's/^\(".\+"\)$/(Literal \1 null null)/' |\
          sed 's/^\.$/)/'
     ) \
    <(echo "))") > "${2}" &&
echo compiling ... &&
raco make "${2}"
