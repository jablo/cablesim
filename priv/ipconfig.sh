#!/bin/sh
#
# Configure a network interface with an IP address and netmask
#
INTF=$1
IP=$2
MASK=$3

sudo /sbin/ifconfig $INTF $IP netmask $MASK

