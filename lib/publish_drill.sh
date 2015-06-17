#!/bin/bash 

mkdir -p $HOME/.downloads
cd $HOME/.downloads
wget -N https://s3.amazonaws.com/relaks/drill-1.1.0-SNAPHOT.tgz
mkdir -p $HOME/.m2/repository/org/apache/drill
cd $HOME/.m2/repository/org/apache/drill && tar xzf $HOME/.downloads/drill-1.1.0-SNAPHOT.tgz
