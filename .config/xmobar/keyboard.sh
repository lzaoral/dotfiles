#!/bin/bash
ibus engine | cut -d: -f2 | tr '[:lower:]' '[:upper:]'
