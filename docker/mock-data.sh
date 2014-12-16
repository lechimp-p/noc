#!/bin/bash

cd ../python
python mock_noc.py http://$(../docker/docker_ip)/api
