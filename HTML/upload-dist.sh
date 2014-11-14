#!/bin/bash

scp -r -P 50022 dist/* root@$(../docker/docker_ip):/opt/NoC-website 
