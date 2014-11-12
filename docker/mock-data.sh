#!/bin/bash

cd ../python
python mock_noc.py http://$(boot2docker_ip)/api
