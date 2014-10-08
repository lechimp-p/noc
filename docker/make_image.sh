#!/bin/bash

cp -r ../Server src
docker build -t lechimp/noc-server .
rm -r src
