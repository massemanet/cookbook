#!/bin/bash

if [ -z "$1" ] ; then
    filepattern="*"
else
    filepattern="$1"
fi

for f in $filepattern ; do
    if [ -f "$f" ] ; then
        echo -n $f
        2>/dev/null emacs -batch "$f" \
            --eval "(progn(whitespace-cleanup)
                    (untabify (point-min) (point-max)))" \
            -f save-buffer
        echo " $?"
    fi
done
