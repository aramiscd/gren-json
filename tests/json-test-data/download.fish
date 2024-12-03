#!/usr/bin/env fish

wget --accept=json --no-parent --recursive --no-clobber \
    https://raw.githubusercontent.com/json-iterator/test-data/refs/heads/master/large-file.json

wget --accept=json --no-parent --recursive --no-clobber \
    https://microsoftedge.github.io/Demos/json-dummy-data/
