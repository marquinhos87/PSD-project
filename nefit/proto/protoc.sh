#!/bin/sh
protoc -I=. --java_out=src/main/java nefit.proto
