#!/bin/bash

echo 'password' > scp -P 50022 dist/* lechimp@$(boot2docker ip):/opt/NoC-website 
