#!/bin/bash

scp -r -P 50022 dist/* root@$(boot2docker_ip):/opt/NoC-website 
