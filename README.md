Cable modem and CMTS IP Simulator
=================================

Run the demo
-------------
<pre>
rebar compile
cd ebin
erl 
application:start(cablesim).
</pre>
will simulate 2 CMTSes each with 1 cable modem attached, having  mta and cpe booting.
Currently Dhcp server IP address, and CMTS gi-addresses are hard-coded to 192.168.56.102
and .103. You will have to add network interfaces and ip addresses to your computer's
configuration manually before starting the demo.

Background
----------
I work with cable modem provisioning systems. I often have a need to test the 
provisioning systems. It takes a lot of time to find, attach, get online etc.
a test cable modem - let alone the time it takes to "clean up" in the 
different provisioning systems after any previous use of that cable modem.

Likewise, stress- and performance testing the provisioning systems is challenging.
I have therefore looked around for software simulation solutions to this problem.
Without any success. There are companies selling solutions that can mimic any
single device. However, in the cable modem world - and seen from the provisoning
systems' perspective, the customer end devices are really "masqueraded" behind
the Cable Modem Termination Systems (CMTS). That is, seen from the provisioning
systems the CMTS does all the DHCP, TFTP and ToD relaying. 

I have therefore set out to write a CMTS + Cable modem (and, as soon as time allows
it, MTA/SIP and CPE) simulation software. I want it immediately scalable. Running
something on the order of 10 CMTSes each with 5000 cable modems, 5000 mtas and
5000 CPEs attached is my goal.

This is the reason I chose Erlang - with each component being a process (or a combination
of processes) with several small state machines / servers, it should be possible to
scale up indefinitely using Erlang's distributed mechanisms while still maintaining
a central point of control / configuration. But that remains to be seen, this is after
all my first Erlang program. It is a very draft first version. Currently completed is:

- A CMTS simulation that implements DHCP relay.
- A cable modem simulation that implelements DHCP client protocol, Tftp config file download
- DHCP signatures for Cable modem, MTA and CPE (one example so far).
- Combined device simulation, of a cable-modem with built in MTA and Router.

Ideas boiling up:
- Add handling of more than one DHCP server
- Add minimal interpretation of the cable modem config file so the simulation can react
  eg. on MTA enable/disable, og report some configured values like uplink/downlink speeds.
- On-demand service: ReSTful web service interface to create a device on-demand and 
  put it "online".
- A subscriber behaviour simulation component, ie code that simulates different 
  customer behaviour: how often is the modem reset, poweroff, poweron, at what time 
  schedules etc.
- Feed back to graphite or similar to get performance graphs
- Add implementation of DHCPv6 client so we can do experiments with IPv6
- Configuration - some way of describing and executing different scenarios

Example modem boot sequence
---------------------------
<pre>
jablo@jablo-VirtualBox:~/temp/cablesim$ sudo tcpdump -ni eth2 udp
tcpdump: verbose output suppressed, use -v or -vv for full protocol decode
listening on eth2, link-type EN10MB (Ethernet), capture size 65535 bytes
22:53:20.508928 IP 192.168.56.102.67 > 192.168.56.105.67: BOOTP/DHCP, Request from 00:00:00:00:00:03, length 391
22:53:20.517789 IP 192.168.56.105.67 > 192.168.56.102.67: BOOTP/DHCP, Reply, length 314
22:53:20.518595 IP 192.168.56.102.67 > 192.168.56.105.67: BOOTP/DHCP, Request from 00:00:00:00:00:03, length 397
22:53:20.530486 IP 192.168.56.105.67 > 192.168.56.102.67: BOOTP/DHCP, Reply, length 314
22:53:20.531439 IP 192.168.56.102.67 > 192.168.56.105.67: BOOTP/DHCP, Request from 00:00:00:00:00:04, length 450
22:53:20.531512 IP 192.168.56.102.67 > 192.168.56.105.67: BOOTP/DHCP, Request from 00:00:00:00:00:05, length 297
22:53:20.533207 IP 192.168.56.102.36656 > 192.168.56.105.69:  18 RRQ "unprov.cm" octet
22:53:20.540370 IP 192.168.56.105.67 > 192.168.56.102.67: BOOTP/DHCP, Reply, length 328
22:53:20.540499 IP 192.168.56.105.59894 > 192.168.56.102.36656: UDP, length 304
22:53:20.542130 IP 192.168.56.102.67 > 192.168.56.105.67: BOOTP/DHCP, Request from 00:00:00:00:00:04, length 456
22:53:20.544441 IP 192.168.56.105.67 > 192.168.56.102.67: BOOTP/DHCP, Reply, length 300
22:53:20.546820 IP 192.168.56.102.67 > 192.168.56.105.67: BOOTP/DHCP, Request from 00:00:00:00:00:05, length 303
22:53:20.548800 IP 192.168.56.102.36656 > 192.168.56.105.59894: UDP, length 4
22:53:20.554119 IP 192.168.56.105.67 > 192.168.56.102.67: BOOTP/DHCP, Reply, length 328
22:53:20.564366 IP 192.168.56.105.67 > 192.168.56.102.67: BOOTP/DHCP, Reply, length 300
^C
15 packets captured
</pre>

Done so far:
------------
Needless to say, a lot of features are missing. In somewhat prioritized order my list is:

- [2013-03-12 DONE] Add device "signatures" (ie. option 43 etc.) so we can mimic the DHCP client packets sent
  by the different manufacturers' devices.
- [2013-03-14 DONE - dhcp only] Make it possible to have different DHCP, TFTP, ToD etc. protocol modules
   to simulate different device behaviours, even faulty devices.
- [2013-03-14 DONE - dhcp only] Make it possible to build a multifunction cable modem device by combining
   - Cable modem DHCP + TFTP + ToD simulator
   - DHCP + TFTP simulator for an embedded MTA or SIP device
   - DHCP simulator for an embedded CPE device (router)
- [2013-03-19 Done - TFTP] Write TFTP and maybe ToD state machine modules, so an IP complete cable modem
  with embedded mta/cpe can be simulated. ToD isn't really interesting.

/Jacob Lorensen
<jalor@yousee.dk>

Various links:
--------------
- http://www.tcpipguide.com/free/t_DHCPGeneralOperationandClientFiniteStateMachine.htm
- http://www.erlang.org/doc/reference_manual/records.html
- http://www.erlang.org/doc/design_principles/fsm.html
- http://www.erlang.org/doc/reference_manual/modules.html
- https://github.com/yrashk/gen_fsm2/blob/master/example/src/example_fsm.erl
- http://www.erlang.org/documentation/doc-4.8.2/doc/design_principles/fsm.html

