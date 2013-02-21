----
author: Alexander Vershilov
date: 2013/02/04
title: OpenRC Extended cgroups support
keywords: gentoo, openrc, linux
----

# Openrc has extended cgroups support

Staring with openrc-0.12 (not released ATM) openrc supports cgroup
limit configuration. The simpliest and most reasonable way to configure it
is use a multiline per-process values:

At first I should note that openrc has optional cgroup support to 
add it you need to set:

```
rc_controller_cgroups="YES"
```

Otherwise one of the option will be applied and openrc "plugin" will not
be loaded. As always settings can be set in rc.conf file and can be 
overloaded in '/etc/conf.d/foo' file for service foo.

Each option is specified by name of the limit and value. Each option may
be a multivalue, e.g.

```
# rc_cgroup_cpu="
# cpu.shares 512
# "
```

For more information about the adjustments that can be made with
cgroups, see `Documentation/cgroups/*` in the linux kernel source tree.

Currently next controllers are supported:

  * blkio -- block io controller
  * cpu   -- cpu controller
  * cpuacct -- cpu accounting information
  * cpuset  -- extended cpu configuration
  * devices -- devices access control
  * memory  -- memory management
  * net_prio -- network priority options


### Why do I ever need cgroups?

You can check kernel documentation. But roughly speaking you can monitor
service processes, and manage resources much better.

### Differences with other system managers

There are some differences between how systemd works, systemd creates hierarchies
for system daemons and users in each controller. Openrc uses it's own cgroup
to monitor daemons, and create a group called 'openrc_<servicename>' in controller
that is configures. 

So you can easily use other cgroup daemons like libcgroup with openrc without any
problem

## Future work

There are some work that can be done to make cgroup support better:

  * configure controller merging
  * cgroup-cleanup, i.e. destroy all childs when stopping service (there are some
      pathes but they were not applied upstream). We will wait for the real use
      cases here
  * cgroup-watchdog, we can monitor if service is dead either by notify_agent 
    (will not require any resources but will not restart service with childs alive)
    or by inotify (will require a watchdog service running but will have no such
    problems)
  * notify-agent callbacks, currently we use notify agent only on openrc cgroup
    and there is no callback, but it can be fixed
  * there is an abitify to make an api for freezer, but we'd wait for the real
    use case before implementing it.

Usefull links:

  * [Red Hat manual](https://access.redhat.com/knowledge/docs/en-US/Red_Hat_Enterprise_Linux/6/html/Resource_Management_Guide/ch01.html)
  * [kernel documentation](http://www.kernel.org/doc/Documentation/cgroups/cgroups.txt)
  * [wiki](http://en.wikipedia.org/wiki/Cgroups)
