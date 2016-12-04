#!/bin/bash

EXE=(../tp_huffman)

if [ $# = 0 ] ; then
	echo "usage: tests_huffman <FICHIER TEXTE A COMPRESSER PUIS DECOMPRESSER>"
else
	echo "[ Test $1 ]"
	$EXE -c $1 $1.comp
	$EXE -d $1.comp $1.comp.txt
	ok=$(diff $1 $1.comp.txt)
	echo ""
	if [ "$ok" = "" ] ; then
		echo "[ Resultat OK : $1 et $1.comp.txt sont IDENTIQUES ]"
	else
		echo "[ Resultat KO : $1 et $1.comp.txt sont DIFFERENTS ]"
	fi
fi ;

