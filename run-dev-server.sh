#!/bin/bash

docker run -t -i -d -p 8000:8000 --name noc-server -v /opt/NoC-data:./NoC-data "cd /opt/NoC-data && /opt/NoC-Server/dist/build/NoC-Server-dev/NoC-Server-dev"
