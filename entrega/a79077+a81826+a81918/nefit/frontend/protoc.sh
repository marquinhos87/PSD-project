#!/bin/sh
../rebar3 get-deps
escript _build/default/plugins/gpb/bin/protoc-erl -modsuffix proto -o src/ ../shared/nefit.proto
