#!/bin/bash
if [[ -z "$1" ]]; 
then address_prefix="192.168.0"; 
else address_prefix=$1;
fi;

var=$( for ip in $(seq 1 254); do 
           /home/dries/bin/is_ip_address_up.sh $address_prefix.$ip&
       done);

echo "$var" | grep "up" | sort --version-sort;

