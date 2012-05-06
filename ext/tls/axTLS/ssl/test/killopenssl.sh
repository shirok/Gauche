#!/bin/sh
awk '{print $1}' "../ssl/openssl.pid" | xargs kill -9
rm -f ../ssl/openssl.pid
