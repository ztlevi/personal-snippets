# Linux multiple display scale

Change the scale in `settings` -> `devices` -> `Display` to 200%

Open Nvidia settings

```
sudo nvidia-settings
```

![Imgur](https://i.imgur.com/9xuZLEh.png)

![Imgur](https://i.imgur.com/n2UTYWT.png)

![Imgur](https://i.imgur.com/S6RkDX3.png)

## Saving

Now we will save the new settings by clicking **Save to X Configuration File**.

![save-x-config.png](https://help.ubuntu.com/community/NvidiaMultiMonitors?action=AttachFile&do=get&target=save-x-config.png "save-x-config.png")

I prefer to _uncheck_ **Merge with existing file.** Now click **Save**. If it gives you an error, "Unable to remove old
X config backup file '/etc/X11/xorg.conf.backup' then click OK and open the save dialog again. Click **Show
preview...**, select all and copy. Now open a terminal and run
