#!/bin/bash

if [ ! -f "$1" ]; then
    echo "no such file"
    exit 1
fi
echo -n "opened..."
convert "$1" -resize 256x256 -transparent white $$-8.png
echo -n " converted..."

convert $$-8.png -resize   16x16 $$-4.png
convert $$-8.png -resize   32x32 $$-5.png
convert $$-8.png -resize   64x64 $$-6.png
convert $$-8.png -resize 128x128 $$-7.png
echo -n " resized ..."
convert $$-4.png $$-5.png $$-6.png $$-7.png $$-8.png -colors 256 "$1.ico"
echo -n " created..."
rm $$-*.png
echo " cleaned up."
echo "created $1.ico"
