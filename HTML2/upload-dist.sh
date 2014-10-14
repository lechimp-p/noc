#!/bin/bash

scp -r -P 50022 dist/* root@$(boot2docker ip):/opt/NoC-website 
