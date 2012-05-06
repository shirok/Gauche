#!/bin/sh
awk '{print $1}' "../ssl/openssl.pid" | xargs kill -9
