#!/bin/bash

cd ../python
python mock_noc.py http://$(docker_ip)/api
