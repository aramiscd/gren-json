#!/usr/bin/env fish

clear
# and gren make src/Main.gren --optimize
and gren make src/Main.gren
and node app
