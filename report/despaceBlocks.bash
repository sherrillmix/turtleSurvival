#!/bin/bash
sed 's/\s\+<</<</' $1|sed 's/\s\+@/@/'
