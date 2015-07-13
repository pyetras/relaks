#!/bin/bash

mkdir -p $HOME/.m2/repository/org/apache/drill
mkdir -p $HOME/.m2/repository/com/bethecoder
BUILD=`pwd`
cd $HOME/.m2/repository/org/apache/drill && tar xzf $BUILD/lib/drill-1.1.0-SNAPSHOT.tgz
cd $HOME/.m2/repository/com/bethecoder && tar xzf $BUILD/lib/ascii_table.tgz
