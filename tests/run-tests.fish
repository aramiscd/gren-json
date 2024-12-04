#!/usr/bin/env fish

gren make src/Main.gren --output=app
and node app
and rm app
