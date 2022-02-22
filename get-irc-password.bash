#!/bin/bash
grep "$1" ~/.authinfo | sed 's/.*password \([^ ]\+ \).*/\1/g'
