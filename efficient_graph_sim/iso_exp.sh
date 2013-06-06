#!/bin/bash

export g=$1
export q=$2
export g_length=$(cat $g | wc -l)
export q_length=$(cat $q | wc -l)
export g_edges=/tmp/ge
export q_edges=/tmp/qe
export g_cols=/tmp/gc
export q_cols=/tmp/qc


echo Data Graph Size:  $g_length
echo Query Graph Size: $q_length

scala -cp scala/preprocessor Preprocessor $g $g_edges $g_cols $q $q_edges $q_cols 
c/iso $g_length $q_length $g_edges $q_edges $g_cols $q_cols

rm $g_edges
rm $q_edges
rm $g_cols
rm $q_cols

