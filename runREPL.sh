#!/usr/bin/env bash

prefix="luogu/build/libs"
classpath="${prefix}/luogu-0.0.1.jar:${prefix}/luogu-0.0.1-dependencies.jar"

./gradlew assemble
kotlinc -cp ${classpath}
