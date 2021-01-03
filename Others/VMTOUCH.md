[Hoytech](https://hoytech.com/vmtouch/)

# vmtouch - the Virtual Memory Toucher

## Portable file system cache diagnostics and control

- [Download from github](https://github.com/hoytech/vmtouch)
- [Read the online manual](https://github.com/hoytech/vmtouch/blob/master/vmtouch.pod)

**vmtouch** is a tool for learning about and controlling the file system cache of unix and unix-like
systems. It is BSD licensed so you can basically do whatever you want with it.

## Quick install guide:

    $ git clone https://github.com/hoytech/vmtouch.git
    $ cd vmtouch
    $ make
    $ sudo make install

## What is it good for?

- Discovering which files your OS is caching
- Telling the OS to cache or evict certain files or regions of files
- Locking files into memory so the OS won't evict them
- Preserving virtual memory profile when failing over servers
- Keeping a "hot-standby" file-server
- Plotting filesystem cache usage over time
- Maintaining "soft quotas" of cache usage
- Speeding up batch/cron jobs
- And much more...

## Support

To complement the open source community, Hoytech offers services related to vmtouch:

- Advanced feature development
- Support contracts
- Training sessions

Please [contact Doug Hoyte](mailto:doug@hoytech.com?subject=vmtouch%20support) for more information.

## Examples

### Example 1

How much of the /bin/ directory is currently in cache?

    $ vmtouch /bin/
               Files: 92
         Directories: 1
      Resident Pages: 348/1307  1M/5M  26.6%
             Elapsed: 0.003426 seconds

### Example 2

How much of _big-dataset.txt_ is currently in memory?

    $ vmtouch -v big-dataset.txt
    big-dataset.txt
    [                                                            ] 0/42116

               Files: 1
         Directories: 0
      Resident Pages: 0/42116  0/164M  0%
             Elapsed: 0.005182 seconds

None of it. Now let's bring part of it into memory with **tail**:

    $ tail -n 10000 big-dataset.txt > /dev/null

Now how much?

    $ vmtouch -v big-dataset.txt
    big-dataset.txt
    [                                                    oOOOOOOO] 4950/42116

               Files: 1
         Directories: 0
      Resident Pages: 4950/42116  19M/164M  11.8%
             Elapsed: 0.006706 seconds

vmtouch tells us that 4950 pages at the end of the file are now resident in memory.

### Example 3

Let's **touch** the rest of /big-dataset.txt/ and bring it into memory (pressing enter a few times
to illustrate the animated progress bar you will see on your terminal):

    $ vmtouch -vt big-dataset.txt
    big-dataset.txt
    [OOo                                                 oOOOOOOO] 6887/42116
    [OOOOOOOOo                                           oOOOOOOO] 10631/42116
    [OOOOOOOOOOOOOOo                                     oOOOOOOO] 15351/42116
    [OOOOOOOOOOOOOOOOOOOOOo                              oOOOOOOO] 19719/42116
    [OOOOOOOOOOOOOOOOOOOOOOOOOOOo                        oOOOOOOO] 24183/42116
    [OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo                  oOOOOOOO] 28615/42116
    [OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo              oOOOOOOO] 31415/42116
    [OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo      oOOOOOOO] 36775/42116
    [OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo  oOOOOOOO] 39431/42116
    [OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO] 42116/42116

               Files: 1
         Directories: 0
       Touched Pages: 42116 (164M)
             Elapsed: 12.107 seconds

### Example 4

We have 3 big datasets, _a.txt_, _b.txt_, and _c.txt_ but only 2 of them will fit in memory at once.
If we have _a.txt_ and _b.txt_ in memory but would now like to work with _b.txt_ and _c.txt_, we
could just start loading up _c.txt_ but then our system would evict pages from both _a.txt_ (which
we want) and _b.txt_ (which we don't want).

So let's give the system a hint and **evict** _a.txt_ from memory, making room for _c.txt_:

    $ vmtouch -ve a.txt
    Evicting a.txt

               Files: 1
         Directories: 0
       Evicted Pages: 42116 (164M)
             Elapsed: 0.076824 seconds

### Example 5

**Daemonise** and **lock** all files in a directory into physical memory:

    vmtouch -dl /var/www/htdocs/critical/

## What other people are saying

People have found lots of uses for vmtouch over the years. Here are a few links in no particular
order:

### Articles

- **[FEATURED: Hosting Advice: Interview with Doug about vmtouch](https://www.hostingadvice.com/blog/vmtouch-delivers-file-system-cache-diagnostics/)**
- [Admin magazine: Performance Tuning Dojo: Tune-Up](http://www.admin-magazine.com/Articles/Tuning-Your-Filesystem-s-Cache)
- [Techniques for Warming Up a MongoDB Secondary](http://blog.parse.com/learn/engineering/techniques-for-warming-up-mongodb/)
- [Linux Memory Usage](http://www.inmotionhosting.com/support/website/server-usage/linux-check-memory-usage)
- [What a C programmer should know about memory](http://marek.vavrusa.com/c/memory/2015/02/20/memory/)
- [Playlists at Spotify - Using Cassandra to store version controlled objects](http://www.slideshare.net/JimmyMrdell/playlists-at-spotify-cassandra-summit-london-2013#slide32)
  (slide 32)
- [Understanding and optimizing Memory utilization](http://careers.directi.com/display/tu/Understanding+and+optimizing+Memory+utilization)
- [admon.org](http://www.admon.org/system-tuning/vmtouch-file-system-cache-diagnostics-and-control/)
- [thewebdev.de](http://thewebdev.de/vmtouch-virtual-memory-touch/)
- [Tune Up Paging with vmtouch](http://www.trevoroconnell.com/2012/02/tune-up-paging-with-vmtouch.html)
- [Linux Cached Memory](http://www.jothirams.com/linux-cached-memory/)
- [Of how much of a file is in RAM](http://fulmicoton.com/posts/pagecache/)
- [Manipulating the kernel's page cache with vmtouch](http://blog.armcd.co.uk/2012/05/manipulating-the-kernels-page-cache-with-vmtouch.html)
- [Memory management in Linux kernel](http://www.slideshare.net/e1coyot/memory-management-in-linux-kernel)
  (slide 16)
- [Supercomputing on the cheap with Parallella](http://radar.oreilly.com/2013/12/supercomputing-on-the-cheap-with-parallella.html)
- [System Design and Big Data, chapter 6](http://prismoskills.appspot.com/lessons/System_Design_and_Big_Data/Chapter_06_-_System_Design.jsp)
- [Lucene @ Yelp](http://issuu.com/lucidimagination12/docs/gaikaiwari_sudarshan_-_lucene_yelp_lucene_revoluti)
  (slide 16)
- [tuxdiary: vmtouch: portable file cache analyzer](http://tuxdiary.com/2016/01/26/vmtouch/)

### Real-world sightings

- [Linux kernel mailing list: zcache: Support zero-filled pages more efficiently](http://www.gossamer-threads.com/lists/linux/kernel/1693344)
- [comp.db.sqlite.general: Strange eviction from Linux page cache](http://comments.gmane.org/gmane.comp.db.sqlite.general/79457)
- [Emacs speed up 1000%](http://blog.binchen.org/posts/emacs-speed-up-1000.html)
- [Jolla Review: Some Rough Edges, But This Linux Smartphone Shows Promise](https://lwn.net/Articles/580335/)
  (vmtouch deployed on maemo phones?)
- [ceph-users: Ceph SSD array with Intel DC S3500's](http://lists.ceph.com/pipermail/ceph-users-ceph.com/2014-October/043509.html)
- [proxmox forums: CPU Performance Degradtion](http://forum.proxmox.com/archive/index.php/t-16798.html)
- [Argonne National Laboratory's Advanced Photon Source](https://confluence.aps.anl.gov/display/howtos/Evicting+data+from+filesystem+cache)
- [Elastic Search: Dealing with OS page cache evictions?](https://discuss.elastic.co/t/dealing-with-os-page-cache-evictions/21974/5)
- [Data-center deploy using torrent and mlock()](http://th30z.blogspot.ca/2012/06/data-center-deploy-using-torrent-and.html)
- [Making best use of 512mb Pi with tmpfs](http://forum.xbian.org/archive/index.php/thread-764.html)
- [redis-db: Issue with Redis replication while transferring rdb file from master to slave](https://groups.google.com/d/msg/redis-db/zaLGF1Bit0A/QsVGMJtTpKEJ)
- [mongodb-user: Oplog Memory Consumption](http://qnalist.com/questions/5278241/oplog-memory-consumption)
- [CentOS bugtracker: oom killer kills process rather than freeing cache](https://bugs.centos.org/view.php?id=7091)
- [LMDB mailing list](http://www.openldap.org/lists/openldap-technical/201511/msg00022.html)
- Used to optimize [ethereum sports betting](https://sportcrypt.com)

### Instagram

Discussion about instagram's usage of vmtouch:

- [What Powers Instagram: Hundreds of Instances, Dozens of Technologies](http://instagram-engineering.tumblr.com/post/13649370142/what-powers-instagram-hundreds-of-instances)
- [Instagram Architecture: 14 Million Users, Terabytes Of Photos, 100s Of Instances, Dozens Of Technologies](http://highscalability.com/blog/2011/12/6/instagram-architecture-14-million-users-terabytes-of-photos.html)
- [The Instagram Architecture Facebook Bought For A Cool Billion Dollars](http://highscalability.com/blog/2012/4/9/the-instagram-architecture-facebook-bought-for-a-cool-billio.html)
- [parse_vmtouch.py](https://gist.github.com/mikeyk/1424540) (script used by instagram)

### Stack-overflow and friends

- [Does the Linux filesystem cache files efficiently?](http://stackoverflow.com/questions/7118543/does-the-linux-filesystem-cache-files-efficiently)
- [Postgresql doesn't use memory for caching](http://stackoverflow.com/questions/23048001/postgresql-doesnt-use-memory-for-caching)
- [MongoDB, NUMA hardware, page faults](http://stackoverflow.com/questions/19995756/mongodb-numa-hardware-page-faults-but-enough-ram-for-working-set-touch-comman)
- [Know programs in cache](http://stackoverflow.com/questions/23662322/know-programs-in-cache)
- [Is it possible to list the files that are cached?](http://serverfault.com/questions/278454/is-it-possible-to-list-the-files-that-are-cached)
- [Tell the linux kernel to put a file in the disk cache?](http://serverfault.com/questions/406308/tell-the-linux-kernel-to-put-a-file-in-the-disk-cache)
- [Securely wipe an entire Linux server with itself](http://serverfault.com/questions/408614/securely-wipe-an-entire-linux-server-with-itself)
- [Caching/preloading files on Linux into RAM](http://serverfault.com/questions/43383/caching-preloading-files-on-linux-into-ram)
- [Why drop caches in Linux?](http://serverfault.com/questions/597115/why-drop-caches-in-linux)
- [Clear / Flush cached memory](http://serverfault.com/questions/435635/clear-flush-cached-memory)
- [limit filesystem cache size for specific files under linux](http://serverfault.com/questions/504193/limit-filesystem-cache-size-for-specific-files-under-linux)
- [Memory mapping files for a blazing fast webserver on Linux](http://serverfault.com/questions/592907/memory-mapping-files-for-a-blazing-fast-webserver-on-linux)
- [Performance difference between ramfs and tmpfs](http://serverfault.com/questions/590124/performance-difference-between-ramfs-and-tmpfs)
- [How do I lock a growing directory in memory?](http://unix.stackexchange.com/questions/203920/how-do-i-lock-a-growing-directory-in-memory)
- [How do I vmtouch a directory (not the files it contains)?](http://unix.stackexchange.com/questions/215202/how-do-i-vmtouch-a-directory-not-the-files-it-contains)
  (good question, I don't know of a userspace way to do this)
- [MySQL queries are 10 to 100 times slower after OS reboot](http://dba.stackexchange.com/questions/64925/mysql-queries-are-10-to-100-times-slower-after-os-reboot)
- [How can one examine what files are in Linux's page cache?](http://www.quora.com/How-can-one-examine-what-files-are-in-Linuxs-page-cache)

### OS packages/ports

- [Fedora Linux](https://admin.fedoraproject.org/pkgdb/package/vmtouch/)
- [RHEL/OpenSUSE/SLE](https://software.opensuse.org//download.html?project=utilities&package=vmtouch)
- [FreeBSD](http://www.freshports.org/sysutils/vmtouch/)
- [Debian](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=672696) (stalled)
- [Arch Linux](https://aur.archlinux.org/packages/vmtouch/)
- [Gentoo Linux](https://packages.gentoo.org/packages/dev-util/vmtouch)
- [Ubuntu PPA](https://launchpad.net/~pg-radadia/+archive/ubuntu/vmtouch)

### Non-english

- Spanish: [1](http://blog.renzocolnago.com/as-tecnologias-que-rodam-o-instagram/)
- French: [1](http://www.gcu-squad.org/2012/04/u-cant-vmtouch-this/)
  [2](https://wiki.koumbit.net/VmTouch)
- Chinese: [1](http://blog.chinaunix.net/uid-20662820-id-3480240.html)
  [2](http://blog.sina.com.cn/s/blog_56c9b55c0101195u.html)
  [3](http://www.damndigital.com/archives/39684) [4](http://blog.yufeng.info/archives/1903)
  [5](http://anykoro.sinaapp.com/2012/11/25/ramdisk-vmtouch-and-mlock/)
  [6](http://xiezhenye.com/2013/05/mongodb-%E5%86%85%E5%AD%98%E4%BD%BF%E7%94%A8.html)
- Russian: [1](http://users.livejournal.com/_winnie/353678.html)
  [2](http://habrahabr.ru/company/yandex/blog/231957/) [3](http://bulimov.ru/it/meminfo-visualizer/)
- Polish: [1](https://nfsec.pl/root/4694)

### Misc

- [OpenHub](https://www.openhub.net/p/vmtouch)
- [Awesome C directory](https://github.com/uhub/awesome-c)
- [Computer Security Resources](http://thefiringline.com/library/compsec.html)

### Other tools

- [linux-ftools](https://code.google.com/p/linux-ftools/)
- [cachemaster](https://github.com/caisonglu/cachemaster) (inspired by vmtouch)
- [pcstat](https://github.com/tobert/pcstat)
- [fmlock](https://github.com/datenwolf/fmlock)
- [nocache](https://github.com/Feh/nocache/)
- [ureadahead](http://manpages.ubuntu.com/manpages/ureadahead.8.html)

There are also lots of mentions on twitter using the [#vmtouch](https://twitter.com/hashtag/vmtouch)
hash-tag

Have another link? Please [let me know](https://hoytech.com/about)!
