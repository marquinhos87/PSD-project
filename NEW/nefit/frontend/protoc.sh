#!/bin/sh
../rebar3 get-deps
escript _build/default/plugins/gpb/bin/protoc-erl -o src/ ../proto/nefit.proto
