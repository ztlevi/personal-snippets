# Ubuntu Nvidia Installation issues

## Black screen: Disable Xorg to resolve the confict with Nvidia

1. Enter TTY or recovery mode shell
2. Sudo edit `/etc/default/grub`
3. Change `GRUB_CMDLINE_LINUX_DEFAULT` from `"quiet splash"` to `"nouveau.modeset=1"`
4. Update grub config by `sudo update-grub`

## Change `GRUB_CMDLINE_LINUX_DEFAULT`

**To temporarily add a boot parameter to a kernel:**

1.  Start your system and wait for the GRUB menu to show (if you don't see a GRUB menu, press and hold the left

    <kbd>Shift</kbd> key right after starting the system).

2.  Now highlight the kernel you want to use, and press the

    <kbd>e</kbd> key. You should be able to see and edit the commands associated with the highlighted kernel.

3.  Go down to the line starting with `linux` and add your parameter `foo=bar` to its end.
4.  Now press

    <kbd>Ctrl</kbd> + <kbd>x</kbd> to boot.

**To make this change permanent:**

1.  From a terminal (or after pressing <kbd>Alt</kbd> + <kbd>F2</kbd>) run:

    ```
    gksudo gedit /etc/default/grub
    ```

    (or use `sudo nano` if `gksudo` or `gedit` are not available) and enter your password.

2.  Find the line starting with `GRUB_CMDLINE_LINUX_DEFAULT` and append `foo=bar` to its end. For example:

    ```
    GRUB_CMDLINE_LINUX_DEFAULT="quiet splash foo=bar"
    ```

    Save the file and close the editor.

3.  Finally, start a terminal and run:

    ```
    sudo update-grub
    ```

    to update GRUB's configuration file (you probably need to enter your password).

## Ubuntu enter recovery mode

1.  Switch on your computer.
2.  Wait until the BIOS has finished loading, or has almost finished. (During this time you will probably see a logo of
    your computer manufacturer.)
3.  Quickly press and hold the

    <kbd>Shift</kbd> key (or <kbd>Escape</kbd> key on some computers), which will bring up the GNU GRUB menu. (If you
    see the Ubuntu logo, you've missed the point where you can enter the GRUB menu.) **\*\***

4.  Select the line which starts with "Advanced options". **\***

5.  Select the line ending with _"(recovery mode)"_, probably the second line, something like:

    > Ubuntu GNU/Linux, with Linux 3.8.0-26-generic (recovery mode)

6.  Press Return and your machine will begin the boot process.

7.  After a few moments, your workstation should display a menu with a number of options. One of the options (you may
    need to scroll down to the bottom of the list) will be "Drop to root shell prompt". Press Return with this option
    highlighted.

8.  The root partition is mounted read-only. To mount it read/write, enter the command

    ```
    mount -o remount,rw /
    ```

9.  If you have /home, /boot, /tmp, or any other mount point on a separate partition, you can mount them with the
    command

    ```
    mount --all
    ```
