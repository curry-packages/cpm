#!/bin/bash

if [ ! -d vendor ]
then
    mkdir vendor
fi

while read dep
do
    name=`echo "$dep" | cut -f 1 -d ' '`
    url=`echo "$dep" | cut -f 2 -d ' '`
    ref=`echo "$dep" | cut -f 3 -d ' '`
    
    if [ -d "vendor/$name" ]
    then
        echo "Package $name already exists, skipping"
    else
        echo "Fetching package $name at $ref from $url"
        git clone $url "vendor/$name"
        pushd .
        cd "vendor/$name"
        git checkout $ref
        popd
    fi
done <dependencies.txt

# The following file is required by the package `cass`:
CASSCONFIG=vendor/cass/src/CASS/PackageConfig.curry
echo "module CASS.PackageConfig where" > $CASSCONFIG
echo "packageVersion :: String"       >> $CASSCONFIG
echo "packageVersion = \"3.0.1 \""     >> $CASSCONFIG
echo "packagePath :: String"          >> $CASSCONFIG
echo "packagePath = \"`pwd`/vendor/cass\"" >> $CASSCONFIG
echo "getPackagePath :: IO String"         >> $CASSCONFIG
echo "getPackagePath = return packagePath" >> $CASSCONFIG
echo "packageExecutable :: String"    >> $CASSCONFIG
echo "packageExecutable = \"\""       >> $CASSCONFIG
