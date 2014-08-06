----
date: 06/11/2013
author: Alexander Vershilov
title: Supervision inside OpenRC
----

The role of the supervision in the init systems
becomes crucial. And many administrators  wants some kind of s-vision
support from the scratch. Here are some thoughts about this problem
and a way how things will be solved inside OpenRC.

<small>(As always I'm sorry for my english and maybe problems in technitial details, so any feedback is apprecated</small>

# The problem.

The idea of supervision is to take care about running processes and restart
them in the case if they fail. It's interesting to say that 'fail' may mean
different things:

  1. Process exited (with non zero EXIT_CODE);
  2. Process fail to work as expected.

In the most systems only first meaning is addressed, however it's not very
carefull and in systems where _reliance_ is required we need to use additional
tools to support 2.

Another set of problem is about how we will decide if we want to restart
service and run cleanup actions. And this problem is not easy at all, i.e. 
we may want to stop restarting service if it fails constantly, or increase
timeout in restarts and so on.

Also we may want additional features like say, remote access/control
to supervised services, additinal notification.

And it's clear that it's insane to support all required tools inside one
service management package, and there the solution will be:
'give a user possibility to delegate supervision problem to the standalone application'.

So the solution would be the following: 

  1. Let OpenRC know that user may want s-vision for some services and what module he want to use;
  2. Provide a set of existing modules;
  3. Give an ability to create new modules.

# Monit

I've started to work on 'monit' approach. Monit (http://mmonit.com/monit/) is a monitoring 
that allowes user to monitor services and give much possibilites for their control.
This tool provides a special language that describes how each service should be monitored
started and restarted.

Currently it's possible to run monit as a standalone daemon and then control supervision
via monit itself by calling `monit foo start`/`monit foo stop`. However having OpenRC as
a common point for services control is a good idea.

All the code available on s-vision branch (https://github.com/qnikst/openrc/compare/s-vision)
and will be merged to mainline after some discussion with other developers and administrators
that have a good experience with running monit.

In order to put service under monit control one need:

  1. Run monit under inittab control (or other low level s-vision subsystem). This can be done
  by adding:
      
        MO:2345:respawn:/usr/bin/monit -Ic /etc/monitrc

  to inittab

  2. Add temporary config path under monit control (/etc/monitrc)
        
        INCLUDE /run/openrc-monit/*

  2. Create control file for service under `/etc/conf.d/monit-files/servicename`

  3. Add s-vision module to conf.d file, i.e.
        
        rc_supervise_module="monit"
        rc_monit_type="file"

When you will trigger service start it will check that it is supervised, copy all related files
to the temporary config path, and then call `monit start service`, that will trigger file once
again, but init will understand that it is monitored, and start service as usual.

<span class="label label-warning"> Update: </span> the idea is just to hook up into 
`start_post` all call `monit restart` and `monit monitor` from there.

# Future work

There are a couple of work to be done before merging:

  1. Introduce other `monit_types` like: 

      * simple - where basic control file generated automatically on the fly
      * native - where existing configuration can be reused.

  2. Understand if it possible to run runscript only once

  3. Cleanup code

  4. Export general API that other s-vision subsystems can use

All comments are welcome
  



