#!/bin/sh
protoc -I=. --java_out=main/java nefit.proto
