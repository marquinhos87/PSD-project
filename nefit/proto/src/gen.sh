#!/bin/bash
protoc -I=. --java_out=. nefit.proto
