#!/bin/bash

cd ../python
python mock_noc.py http://$(boot2docker ip)/api
