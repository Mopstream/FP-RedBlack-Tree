#!/bin/bash

stack build
stack exec lab2-exe | dot -Tsvg > $1.svg
eog $1.svg