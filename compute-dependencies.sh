#!/bin/bash
# computes the required contents of file `dependencies.txt`
# from the currently installed packages

BASEURL="https://git.ps.informatik.uni-kiel.de/curry-packages"
for p in `ls .cpm/packages`
do
    revp=`echo $p | rev`
    revn=`echo "$revp" | cut -f 2- -d '-'`
    name=`echo $revn | rev`
    revv=`echo "$revp" | cut -f 1 -d '-'`
    vers=`echo $revv | rev`
    echo $name $BASEURL/$name.git v$vers
done

