#!/bin/bash

cd /opt/NoC-Server-git
git init --bare
/usr/sbin/sshd -D -e
