#!/usr/bin/env bash
ip_address=$1;
ping -c 1 -W 1 $ip_address>/dev/null; 
if [ $? -eq 0 ]; then 
    echo "ip_address(up, '$ip_address')."; 
else 
    echo "ip_address(down, '$ip_address')."; 
fi ; 
