#!/bin/sh

replacementline=$(echo "(defvar *ponderaciones* '($1 $2 $3))")
sedcommand="950s/.*/${replacementline}/"
sed -i "$sedcommand" pruebasmancala.cl
OUTPUT=$(sbcl --noinform --disable-ldb --script pruebasmancala.cl)
echo $OUTPUT
