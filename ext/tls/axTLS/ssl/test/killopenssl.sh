#!/bin/sh
if [ -f "../ssl/openssl.pid" ]; then
    awk '{print $1}' "../ssl/openssl.pid" | xargs kill -9
    rm -f ../ssl/openssl.pid
fi
