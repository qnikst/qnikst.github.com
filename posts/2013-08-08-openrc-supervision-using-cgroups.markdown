-----
author: Alexander Vershilov
date:   2013-07-31
title:  Supervision in pure OpenRC using cgroup subsystem.
tags:   OpenRC, gentoo, programming, linux, supervision
-----

# Abstract

This post describes how it's possible to improve cgroup support in OpenRC
to support user hooks, and shows a way to create basic supervision daemon
based on cgroups.

This post describes OpenRC-0.11/0.12_beta and some things can differ in 
later versions. Please notify me to post updates here if you find a 
differences.

# Introduction

## The problem

In a general case there are many services that should be run and restarted
if they fails. There are many other subproblems like when we should restart
services and when not. Many existing systems can solve those issues but have
different trade-offs. In this post I'll try to present a simple mechanism
that allowes to create basic supervision and other nice things.

## Idea

Linux kernel provides a mechanism to track groups of processes - `Cgroups`.
All process childs will belong to the same cgroups and that groups are easily
trackable from user space. If you want to understand cgroups better you may
read following docs [cgroups](https://www.kernel.org/doc/Documentation/cgroups/cgroups.txt).
Cgroups provides a way of setting limits and controlling groups, that is also
usefull but at this moment it's out of the scope.

When all processes dies kernel will call 'release_notify_agent' script and will
provide a path to cgroup, this may be used to remove empty cgroups and make some
additional actions. 

Idea is that we can check service state to understand if we need to restart it.

# Details 

## Implementation

Here are improvements and files that should be added to OpenRC to provide 
required functionallity.

### Restart daemon

First we need to create a deamon for restarting a services, because we can't 
start service from agent, as it has `PF_NO_SETAFFINITY` flag and thus cgroups
will not work for any of it's children. So lets have a very simple daemon, it
will be extended in the next posts

```bash
#!/bin/sh
if [ $# -lt 1 ] ; then
   echo "usage is $0 <path to fifo>" 
   exit 1
fi

while [ -p $1 ] ; do 
   while read line ; do
      echo "rc-service $line";
   done <$1
done
```

### Release notify agent improvement

Current release notify agent is very simple idea is to extend it to support user 
hooks. There are some different way to do it:

  1. Add it to the service state. Requires hook in a script
  2. Create static structure in a filesystem

We will use 2. as it's simplier and doesn't lead to a init script hacking. We will
have following file structure:

In /etc/conf.d/cgroups there will be hooks 'cgroup-release' for default one
'service-name.cgroup-release' for service specific one. Here is my example.

```
/etc/conf.d/cgroups/
|-- cgroup-release                                            # default release hook
|-- service1.cgroup-release -> service-restart.cgroup-release # service release hook
`-- service-restart.cgroup-release                            # example script

```

This approach doesn't scale on a multiple hooks but it may be improved after discussion
with upstream. Each script can return $RC_CGROUP_CONTINUE exit code, so cgroup will be
not deleted after a hook.

Here is script itself (newer version can be found on [github](https://github.com/qnikst/openrc/blob/cgroups.release_notification/sh/cgroup-release-agent.sh.in)):

```bash
PATH=/bin:/usr/bin:/sbin:/usr/sbin
cgroup=/sys/fs/cgroup/openrc
cgroup_rmdir=1
RC_SVCNAME=${1}

if [ -n "${RC_SVCNAME}" ] ; then
hook=@SYSCONFDIR@/conf.d/cgroups/${RC_SVCNAME}.cgroup-release
[ -f "$hook" -a -x "$hook" ] || hook=@SYSCONFDIR@/conf.d/cgroups/cgroup-release;
if [ -x "$hook" ]; then
"$hook" cleanup "$RC_SVCNAME" || case $? in $RC_CGROUP_CONTINUE) cgroup_rmdir=0;; esac ;
fi
else
cgroup_rmdir=1
fi

if [ ${cgroup_rmdir} -a -d ${cgroup}/"$1" ]; then
for $c in /sys/fs/cgroup/* ; do
rmdir "${c}"/openrc_"$1"
done;
rmdir $cgroup/"${1}"
fi
```

Restart service script. This script simply checks service state and if it's
32 (service failed) then start a new instance and set `$RC_CGROUP_CONTINUE`

```bash
#!/bin/sh
# This script is run for service that need to be restarted
# if it's last process leaves cgroup.

action=$1
service=$2

if [ x$action == x"cleanup" ] ; then

    rc-service $service status > /dev/null
    case $? in
        32) 
        /etc/init.d/${service} -d restart
        exit $RC_CGROUP_CONTINUE
                ;;
        *) 
            return 0;;
    esac
fi
```



## Other solutions

The general supervision is quite complicated problems as there are many conditions when
we can think that our service failed, like:

  * main process dies
  * all service children dies
  * service to not write logs for some time
  * big resource memory/cpu consuming
  * service to not respond on logs for some time
  * log fd is closed.

Some of the options can be translated to another, like big resource consuming can be
translated to process death by setting correct limits. And process death (and in some
cases even children death) can be tracked by log fd 
(in case of a process in background). 

One more thing that you may need complicated hooks, that have a state do decide what
to do with failed service, like do not restart if it was failed many times in a small
time period.

So full features system will be very complicated so non-specialized subsystems address
only a part of a problem domain. Here are some examples for other supervision systems:

  * monit
  * s6
  * daemon-tools
  * angel
  * systemd
  * upstart

# Related work

  1. work on inclusion of a user hooks to OpenRC release agent.
  2. improve restart script to track really dead services that can be restart

# Conclusions and futher work

It's possible to create a very simple and extensible supervision system on the top of OpenRC, 
by extending notification systems. Also there are more usecases for it, like:

    * adding system wide notification mechanism via dbus
    * additional logging system


