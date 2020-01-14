#!/bin/sh
protoc -I=. --java_out=. nefit.proto
