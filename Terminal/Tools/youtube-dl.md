# Download Audio from YouTube

https://gist.github.com/umidjons/8a15ba3813039626553929458e3ad1fc

`-i` - ignore errors

`-c` - continue

`--extract-audio` - extract audio track

`--audio-format mp3` - convert to mp3

`--audio-quality 0` - the best audio quality, `-t` is deprecated, use `-o` instead

`--yes-playlist` - affirm that url points to a playlist

`-o "%(title)s.%(ext)s"` - set the output file title

YT_URL - video url from youtube

## Polished commands

```
# Download single entry
/usr/bin/python $(which youtube-dl) -i -o "%(title)s.%(ext)s" --embed-thumbnail --extract-audio --audio-format mp3 --audio-quality 0 YT_URL

# Download playlist
/usr/bin/python $(which youtube-dl) -ic -o "%(title)s.%(ext)s" --yes-playlist --embed-thumbnail --extract-audio --audio-format mp3 --audio-quality 0 YT_URL
```

## Not polished commands

```
# Download playlist, --download-archive downloaded.txt add successfully downloaded files into downloaded.txt
youtube-dl --download-archive downloaded.txt --no-overwrites -ict --yes-playlist --extract-audio --audio-format mp3 --audio-quality 0 --socket-timeout 5 https://www.youtube.com/playlist?list=UUCvVpbYRgYjMN7mG7qQN0Pg

# Retry until success, no -i option
while ! youtube-dl --download-archive downloaded.txt --no-overwrites -ct --yes-playlist --extract-audio --audio-format mp3 --audio-quality 0 --socket-timeout 5 <YT_PlayList_URL>; do echo DISCONNECTED; sleep 5; done
```
