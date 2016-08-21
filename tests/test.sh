#!/bin/bash

set -e
HOST=${HOST:-localhost}

# Check features
curl -s -H "Content-Type: application/json" http://$HOST:8080/features \
    | grep -qs '["realm"]'

# Create a host
curl -H "Content-Type: application/json" -dhostname=foo -duserclass=bar -X POST "http://$HOST:8080/realm/example.com"
echo
curl -H "Content-Type: application/json" -X DELETE "http://$HOST:8080/realm/example.com/foo"
# Removing nonexistent host is o.k.
curl -H "Content-Type: application/json" -X DELETE "http://$HOST:8080/realm/example.com/foo"
