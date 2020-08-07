## A Note About Automatic Mount At Boot Time

Open Disk app to see disk details.

You need to edit [/etc/fstab](https://www.cyberciti.biz/faq/tag/etcfstab/) file, enter:  
`$ sudo vi /etc/fstab`

Add the following line for ext3 file system:

```sh
/dev/sdb1    /media/newhd   ext3    defaults     0        2
```

Add the following line for Windows FAT32 file system:

```sh
/dev/sdb1    /media/windowshd   vfat    defaults     0        2
```

Save and close the file.
