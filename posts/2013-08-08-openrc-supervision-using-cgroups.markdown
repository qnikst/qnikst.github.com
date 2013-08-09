-----
author: Alexander Vershilov
date:   2013-08-08
title:  Supervision in pure OpenRC using cgroup subsystem.
tags:   OpenRC, gentoo, programming, linux, supervision
-----

<div style="float:right;width:200px;font-size:0.5em;">
  Updates:
    <ul>
      <li> 2008.08.09 - small corrections, acknowledgement section added </li>
    </ul>
  Versions:
    <ul>
      <li> Kernel >=2.6.24 && <=3.10 </li>
      <li> Openrc 0.12 </li>
    </ul>
</div>

# Abstract

This post describes how it's possible to improve cgroup support in OpenRC
to support user hooks, and shows how to create a basic supervision daemon
based on cgroups.

This post describes OpenRC-0.11/0.12_beta and some things may change in 
later versions. Please notify me to post updates here if you find
such changes.

# Introduction

## The problem

In a general case, there are many services that should be run and restarted
when they fail. There are many other subproblems like when should we restart
services and when not. Many existing systems can solve those issues but have
different trade-offs. In this post I'll try to present a simple mechanism
that allows to create basic supervision and other nice things.

## Idea

The Linux kernel provides a mechanism to track groups of processes - `Cgroups`.
All process children will put in the process's cgroup. And it's easy to track 
cgroups from user space. If you want to understand cgroups better you may
read [cgroups documentation](https://www.kernel.org/doc/Documentation/cgroups/cgroups.txt).
Cgroups provide a way of setting limits and controlling groups, that is also
useful but at this moment it's out of the scope.

When all processes in a group die, kernel will call 'release_notify_agent' script, 
proving the path to the cgroup. This may be used to remove empty cgroups and take
additional actions. 

Idea is that we can check service state to decide if we should restart it.

# Details 

## Implementation

Here are improvements and files that should be added to OpenRC to provide 
the required functionality.

### Restart daemon

First we need to create a daemon to restart a services, because we can't 
start service from agent, as it has `PF_NO_SETAFFINITY` flag and thus cgroups
will not work for any of its children. So let's have a very simple daemon, it
will be extended in the next posts

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

### Release notify agent improvement

The current release notify agent is very simple; so we extend it to support
user hooks. There are some different ways to do it:

  1. Add it to the service state. (Requires hook in the init script)
  2. Create static structure in a filesystem

We will use 2. as it's simpler and doesn't lead to a init script hacking. We will
have following file structure:

In /etc/conf.d/cgroups there will be hooks, 'cgroup-release' for default one
'service-name.cgroup-release' for service specific one. Here is my example.

```
/etc/conf.d/cgroups/
|-- cgroup-release                                            # default release hook
|-- foo.cgroup-release -> service-restart.cgroup-release      # service release hook
`-- service-restart.cgroup-release                            # example script

```
 

This approach doesn't scale on a multiple hooks but it may be improved after discussion
with upstream. Each script can return $RC_CGROUP_CONTINUE exit code, so cgroup will not be
deleted after a hook.

Here is a script itself (newer version can be found on [github](https://github.com/qnikst/openrc/blob/cgroups.release_notification/sh/cgroup-release-agent.sh.in)):

```bash
PATH=/bin:/usr/bin:/sbin:/usr/sbin
cgroup=/sys/fs/cgroup/openrc
cgroup_rmdir=1
RC_SVCNAME=$1
RC_CGROUP_CONTINUE=3; 
export RC_CGROUP_CONTINUE RC_SVCNAME PATH;

if [ -n "${RC_SVCNAME}" ] ; then
  hook=@SYSCONFDIR@/conf.d/cgroups/${RC_SVCNAME}.cgroup-release
  [ -x "$hook" ] || hook=@SYSCONFDIR@/conf.d/cgroups/cgroup-release;
  if [ -x "$hook" ]; then
    "$hook" cleanup "$RC_SVCNAME" || case $? in $RC_CGROUP_CONTINUE) cgroup_rmdir=0;; esac ;
  fi
fi

if [ ${cgroup_rmdir} -eq 1 ] && [ -d "${cgroup}/$1" ]; then
  for c in /sys/fs/cgroup/*/"openrc_$1" ; do
    rmdir "${c}"
  done;
  rmdir "$cgroup/${1}"
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

if [ cleanup = "$action" ] ; then

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

Generic supervision is quite a complicated problem as there are many conditions when
we may suppose that our service failed, like:

  * main process dies;
  * all service children die;
  * service does not write logs for some time;
  * large resource memory/cpu consuming;
  * service does not respond to control call;
  * log fd is closed.

Some of the options can be translated to another, like large resource consuming can be
translated to process death by setting correct limits. And process death (and in some
cases even children deaths) can be tracked by log fd 
(in case of a process in background). 

More complex hooks may be also needed, when deciding what to do with failed service,
e.g. do not restart if it has failed many times in a short period of time.

So with all required features will be very complicated. So non-specialized
subsystems address only a part of a problem domain. Here are some other examples of
supervision systems:

  * monit (full featured)
  * s6  (pid, fd based)
  * daemon-tools
  * angel
  * systemd (pid, cgroups based)
  * upstart (pid based)

# Future work

  1. work on inclusion of a user hooks to OpenRC release agent.
  2. improve restart script to track really dead services that can be restarted

# Conclusions and futher work

It's possible to create a very simple and extensible supervision system based on OpenRC, 
by extending notification systems. Also there are more usecases for it, like:

 * adding system wide notification mechanism via dbus
 * additional logging system

# Acknowledgements

I want to thank igli for code corrections and usefull tips, and Kirill Zaborsky for correcting
lingual mistakes.

