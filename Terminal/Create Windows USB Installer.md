## unmount USB disk

```shell
diskutil list

diskutil unmountDisk /dev/disk5
```

## Copy files to USB drive

```shell
sudo dd if=./Win10_1709_English_x64.iso of=/dev/disk5 bs=1m
```
