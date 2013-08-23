---
title: Сохранение всех задач su- в свою cgroup.
author: Alexander Vershilov
date: 2012/02/04
tags: linux, pam, cgroups
license: by-nc-sa
---

Стоит задача: необходимо все программы запущенные через 'su' определить
в собственные cgroups. Это можно сделать за три простые шага.

  1. инициализируем нужную цгруппу, для этого нам нужна именованная 
  группа, чтобы исключить пересечение с системными группами:
  (данный шаг желательно делегировать системе инициализации дистрибутива)

``````shell
  localhost qnikst # mkdir /sys/fs/cgroup/su-log
  localhost qnikst # mount -n -t cgroup -o none,nodev,noexec,nosuid,name=test test /sys/fs/cgroup/su-log
``````

  2. Пишем простой баш скрипт:
  в этом сприпте мы создаём каталог для пользователя (если его нет и помещаем
  туда родительскую задачу (ту, пользуется средствами pam)

``````bash
localhost qnikst # cat /usr/local/bin/pam_exec_test.sh 
#!/bin/sh
p=/sys/fs/cgroup/su-log/${PAM_RUSER}
[ ! -d "${p}" ] && \
   mkdir ${p}
echo ${PPID} > "${p}"/tasks
``````

  3. пишем правило для pam.d (логи опциональны)

`````
session    optional             pam_exec.so   log=/var/log/pam_test_su.log seteuid /usr/local/bin/pam_exec_test.sh
`````
  4. наслаждаемся результатом.

В данном решении есть проблема: будучи суперпользователем пользователь может перемещать свои задачи
по cgroup-ам, для решения этой проблемы нужно писать политику selinux запрещающую переносить задачи
выше, чем cgroup породившего их процесса.
