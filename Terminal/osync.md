# Osync

Osync has been designed to not delete any data, but rather make backups of conflictual files or soft
deletes. Nevertheless, you should always have a neat backup of your data before trying a new sync
tool.

You can download the latest stable release of Osync at
[www.netpower.fr/osync](http://www.netpower.fr/osync) or
https://github.com/deajan/osync/archive/stable.tar.gz

You may also get the last development version at https://github.com/deajan/osync with the following
command

`$ git clone -b "stable" https://github.com/deajan/osync`

`$ bash install.sh`

Osync will install itself to /usr/local/bin and an example configuration file will be installed to
/etc/osync

Osync needs to run with bash shell. Using any other shell will most probably result in errors. If
bash is not your default shell, you may invoke it using

`$ bash osync.sh [options]`

## Daemon mode

Additionnaly, you may run osync in monitor mode, which means it will perform a sync upon file
operations on master replica. This can be a drawback on functionnality versus scheduled mode because
this mode only launches a sync task if there are file modifications on the master replica, without
being able to monitor the slave replica. Slave replica changes are only synced when master replica
changes occur, or when a given amount of time (default 600 seconds) passed without any changes on
master replica. File monitor mode can also be launched as a daemon with an init script. Please read
the documentation for more info. Note that monitoring changes requires inotifywait command
(inotify-tools package for most Linux distributions). BSD, MacOS X and Windows are not yet supported
for this operation mode, unless you find a inotify-tools package on these OSes.

```
sudo apt-get install inotify-tools
```

`# osync.sh /etc/osync/my_sync.conf --on-changes`

Osync file monitor mode may be run as system service with the osync-srv init script. Any
configuration file found in /etc/osync will then create a osync daemon instance. You may run the
install.sh script which should work in most cases or copy the files by hand (osync.sh to
/usr/bin/local, osync-srv to /etc/init.d, sync.conf to /etc/osync).

`$ service osync-srv start`

`$ chkconfig osync-srv on`
