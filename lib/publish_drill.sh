#!/bin/bash

mkdir -p $HOME/.m2/repository/org/apache/drill
BUILD=`pwd`
cd $HOME/.m2/repository/org/apache/drill && tar xzf $BUILD/lib/drill-1.1.0-SNAPSHOT.tgz
