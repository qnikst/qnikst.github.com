---
author: Alexander Vershilov
date: 2013-08-12
title: Using ebtables to use internet connection in guest OS
tags: linux, gentoo, networking
license: cc-by-sa
---

# Abstract

In this post I address the problem of having internetconnections
for in guests systems in case of using wifi or mixed connections.
It's a common situation for laptops and other wifi devices.

# Idea

The common sulution with bridge creation doesn't work as wifi 
adapters doesn't support mac changin (and thus bridging) by design.
The common solution in this case is ```ebtables```.


    +------+ 
    | lvm1 | ----+                          
    +------+     |   +-------+          +------+
                 +---+ tun   +          | eth0 |------> Internel
                 |   +--+----+          +---+--+
    +------+     |      |                   |
    | lvm2 |-----+      +------[ bridge ]---+
    +------+                        |
                                 ebtales
                                    |
                               [ wlan0 ]--------------> Internet

## Realization

1. Verify that you have all required kernel options:

  * ebtables support
  * snat support
  * maybe more

2. Install ebtales

    \# emerge ebtables

3. Add ebtables option:

    \# ebtables -t nat -A POSTROUTING -o wlan0 -j snat --to-src <wlan0_mac> \
                                                       --snat-arp ACCEPT
    \# 
      

4. Save ebtables table

    \# /etc/init.d/ebtables save

5. Start ebtables

    \# /etc/init.d/ebtables start

Optional:

Add ebtables to default runlevel:
    rc-update add ebtables default

# Additional

I have more compicated situation because I'm using eth0 at home and wifi
in university or guest networks thus I need to use ebtables only when I have
to ethernet connection. For this purpose I'll use stacked runlevels
I'll describe it functionallity in the next posts. As an addition solution
it's possible to use ifup to check if ethernet cable exists and start 
ebtables script it there is no connection.

Here are my current conf.d script:

    # LAN Network
    modules_eth0="ifconfig"

    # Bridge for virtual networks
    config_br0="dhcp"
    brctl_br0="
       setfd 0
       stp off
    "
    bridge_br0="eth0"
    RC_NEED_br0="net.eth0"

    # Ethernet
    config_eth0="null"

    # WIFI
    config_wlan0="dhcp"
    wpa_supplicant_wlan0="-Dwext"


# Links

[TODO] ebtables docs
[TODO] nat docs
[TODO] openrc page
http://forums.gentoo.org/viewtopic-t-430571-start-0.html
https://wiki.debian.org/BridgeNetworkConnections
