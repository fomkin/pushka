#!/usr/bin/env bash

sbt ++$TRAVIS_SCALA_VERSION -Dfile.encoding=UTF8 -J-XX:MaxPermSize=1024M coreJVM/test
sbt ++$TRAVIS_SCALA_VERSION -Dfile.encoding=UTF8 -J-XX:MaxPermSize=1024M coreJS/test

if [ "$TRAVIS_SCALA_VERSION" == "2.11.8" ]; then
  sbt ++$TRAVIS_SCALA_VERSION -Dfile.encoding=UTF8 -J-XX:MaxPermSize=1024M testNative/run
fi
