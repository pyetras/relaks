#!/bin/bash

mkdir -p $HOME/.m2/repository/com/bethecoder
BUILD=`pwd`
cd $HOME/.m2/repository/com/bethecoder && tar xzf $BUILD/lib/ascii_table.tgz
