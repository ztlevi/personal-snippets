#### 20 FFmpeg Commands For Beginners

The typical syntax of the FFmpeg command is:

```
ffmpeg [global_options] {[input_file_options] -i input_url} ...
 {[output_file_options] output_url} ...
```

We are now going to see some important and useful FFmpeg commands.

**1. Getting audio/video file information**

To display your media file details, run:

```
$ ffmpeg -i video.mp4
```

**Sample output:**

```
ffmpeg version 3.3 Copyright (c) 2000-2017 the FFmpeg developers
 built with gcc 6.3.1 (GCC) 20170306
 configuration: --prefix=/usr --disable-debug --disable-static --disable-stripping --enable-avisynth --enable-avresample --enable-fontconfig --enable-gmp --enable-gnutls --enable-gpl --enable-ladspa --enable-libass --enable-libbluray --enable-libfreetype --enable-libfribidi --enable-libgsm --enable-libiec61883 --enable-libmodplug --enable-libmp3lame --enable-libopencore_amrnb --enable-libopencore_amrwb --enable-libopenjpeg --enable-libopus --enable-libpulse --enable-libschroedinger --enable-libsoxr --enable-libspeex --enable-libssh --enable-libtheora --enable-libv4l2 --enable-libvidstab --enable-libvorbis --enable-libvpx --enable-libwebp --enable-libx264 --enable-libx265 --enable-libxcb --enable-libxvid --enable-netcdf --enable-shared --enable-version3
 libavutil 55. 58.100 / 55. 58.100
 libavcodec 57. 89.100 / 57. 89.100
 libavformat 57. 71.100 / 57. 71.100
 libavdevice 57. 6.100 / 57. 6.100
 libavfilter 6. 82.100 / 6. 82.100
 libavresample 3. 5. 0 / 3. 5. 0
 libswscale 4. 6.100 / 4. 6.100
 libswresample 2. 7.100 / 2. 7.100
 libpostproc 54. 5.100 / 54. 5.100
Input #0, mov,mp4,m4a,3gp,3g2,mj2, from 'video.mp4':
 Metadata:
 major_brand : isom
 minor_version : 512
 compatible_brands: isomiso2avc1mp41
 encoder : Lavf57.22.100
 Duration: 00:43:18.69, start: 0.000000, bitrate: 1039 kb/s
 Stream #0:0(und): Video: h264 (High) (avc1 / 0x31637661), yuv420p, 1280x714 [SAR 1071:1072 DAR 120:67], 899 kb/s, 23.98 fps, 23.98 tbr, 24k tbn, 47.95 tbc (default)
 Metadata:
 handler_name : VideoHandler
 Stream #0:1(und): Audio: aac (LC) (mp4a / 0x6134706D), 48000 Hz, stereo, fltp, 132 kb/s (default)
 Metadata:
 handler_name : SoundHandler
At least one output file must be specified
```

As you see in the above output, FFmpeg displays the media file information along with FFmpeg details
such as version, configuration details, copyright notice, build and library options etc.

If you don’t want to see the FFmpeg banner and other details, but only the media file information,
use **-hide_banner** flag like below.

```
$ ffmpeg -i video.mp4 -hide_banner
```

**Sample output:**

[![](http://www.ostechnix.com/wp-content/uploads/2017/05/sk@sk_001.png)](http://www.ostechnix.com/wp-content/uploads/2017/05/sk@sk_001.png)

See? Now, it displays only the media file details.

\_\_**Recommended Download** –
[**Free Guide: “Spotify Music Streaming: The Unofficial Guide”**](https://ostechnix.tradepub.com/free/w_make141/prgm.cgi)

**2. Converting video files to different formats**

FFmpeg is powerful audio and video converter, so It’s possible to convert media files between
different formats. Say for example, to convert mp4 file to avi file, run:

```
$ ffmpeg -i video.mp4 video.avi
```

Similarly, you can convert media files to any format of your choice.

For example, to convert youtube flv format videos to mpeg format, run:

```
$ ffmpeg -i video.flv video.mpeg
```

If you want to preserve the quality of your source video file, use ‘-qscale 0’ parameter:

```
$ ffmpeg -i input.webm -qscale 0 output.mp4
```

To check list of supported formats by FFmpeg, run:

```
$ ffmpeg -formats
```

**3. Converting video files to audio files**

To convert a video file to audio file, just specify the output format as .mp3, or .ogg, or any other
audio formats.

The above command will convert input.mp4 video file to output.mp3 audio file.

```
$ ffmpeg -i input.mp4 -vn -ab 320 output.mp3
```

Also, you can use various audio transcoding options to the output file as shown below.

```
$ ffmpeg -i input.mp4 -vn -ar 44100 -ac 2 -ab 320 -f mp3 output.mp3
```

Here,

- **-vn** – Indicates that we have disabled video recording in the output file.
- **-ar** – Set the audio frequency of the output file. The common values used are 22050, 44100,
  48000 Hz.
- **-ac** – Set the number of audio channels.
- **-ab** – Indicates the audio bitrate.
- **-f** – Output file format. In our case, it’s mp3 format.

**4. Change resolution of video files**

If you want to set a particular resolution to a video file, you can use following command:

```
$ ffmpeg -i input.mp4 -filter:v scale=1280:720 -c:a copy output.mp4
```

Or,

```
$ ffmpeg -i input.mp4 -s 1280x720 -c:a copy output.mp4
```

The above command will set the resolution of the given video file to 1280×720.

Similarly, to convert the above file to 640×480 size, run:

```
$ ffmpeg -i input.mp4 -filter:v scale=640:480 -c:a copy output.mp4
```

Or,

```
$ ffmpeg -i input.mp4 -s 640x480 -c:a copy output.mp4
```

This trick will help you to scale your video files to smaller display devices such as tablets and
mobiles.

**5. Compressing video files**

It is always better to reduce the media files size to lower size to save the harddisk’s space.

The following command will compress and reduce the output file’s size.

```
$ ffmpeg -i input.mp4 -vf scale=1280:-1 -c:v libx264 -preset veryslow -crf 24 output.mp4
```

Please note that you will lose the quality if you try to reduce the video file size. You can lower
that crf value to 23 or lower if 24 is too aggressive.

You could also transcode the audio down a bit and make it stereo to reduce the size by including the
following options.

```
-ac 2 -c:a aac -strict -2 -b:a 128k
```

\_\_**Recommended Download** –
[**Free Guide: “PLEX, a Manual: Your Media, With Style”**](https://ostechnix.tradepub.com/free/w_make75/prgm.cgi)

**6. Compressing Audio files**

Just compressing video files, you can compress audio files using **-ab** flag in order to save some
disk space.

Let us say you have an audio file of 320 kbps bitrate. You want to compress it by changing the
bitrate to any lower value like below.

```
$ ffmpeg -i input.mp3 -ab 128 output.mp3
```

The list of various available audio bitrates are:

1.  96kbps
2.  112kbps
3.  128kbps
4.  160kbps
5.  192kbps
6.  256kbps
7.  320kbps

**7. Removing audio stream from a media file**

If you don’t want to a audio from a video file, use **-an** flag.

```
$ ffmpeg -i input.mp4 -an output.mp4
```

Here, ‘an’ indicates no audio recording.

The above command will undo all audio related flags, because we don’t audio from the input.mp4.

**8. Removing video stream from a media file**

Similarly, if you don’t want video stream, you could easily remove it from the media file using ‘vn’
flag. vn stands for no video recording. In other words, this command converts the given media file
into audio file.

The following command will remove the video from the given media file.

```
$ ffmpeg -i input.mp4 -vn output.mp3
```

You can also mention the output file’s bitrate using ‘-ab’ flag as shown in the following example.

```
$ ffmpeg -i input.mp4 -vn -ab 320 output.mp3
```

**9. Extracting images from the video**

Another useful feature of FFmpeg is we can easily extract images from a video file. This could be
very useful, if you want to create a photo album from a video file.

To extract images from a video file, use the following command:

```
$ ffmpeg -i input.mp4 -r 1 -f image2 image-%2d.png
```

Here,

- **-r** – Set the frame rate. I.e the number of frames to be extracted into images per second. The
  default value is 25.
- **-f** – Indicates the output format i.e image format in our case.
- **image-%2d.png** – Indicates how we want to name the extracted images. In this case, the names
  should start like image-01.png, image-02.png, image-03.png and so on. If you use %3d, then the
  name of images will start like image-001.png, image-002.png and so on.

**10. Cropping videos**

It is somewhat similar to change the resolution of the video file. let us say you want to a video
with size 300×300. You could do that using command:

```
$ ffmpeg -i input.mp4 -croptop 100 -cropbottom 100 -cropleft 300 -cropright 300 output.mp4
```

Please note that cropping videos will affect the quality. Do not do this unless it is necessary.

**11. Convert a specific portion of a video**

Sometimes, you might want to convert only a specific portion of the video file to different format.
Say for example, the following command will convert the first 50 seconds of given video.mp4 file to
video.avi format.

```
$ ffmpeg -i input.mp4  -t 50 output.avi
```

Here, we specify the the time in seconds. Also, it is possible to specify the time in **hh.mm.ss**
format.

**12. Set the aspect ratio to video**

You can set the aspect ration to a video file using **-aspect** flag like below.

```
$ ffmpeg -i input.mp4 -aspect 16:9 output.mp4
```

The commonly used aspect ratios are:

- 16:9
- 4:3
- 16:10
- 5:4
- 2:21:1
- 2:35:1
- 2:39:1

**13. Adding poster image to audio files**

You can add the poster images to your files, so that the images will be displayed while playing the
audio files. This could be useful to host audio files in Video hosting or sharing websites.

```
$ ffmpeg -loop 1 -i inputimage.jpg -i inputaudio.mp3 -c:v libx264 -c:a aac -strict experimental -b:a 192k -shortest output.mp4
```

**14. Trim a media file using start and stop times**

To trim down a video to smaller clip using start and stop times, we can use the following command.

```
$ ffmpeg -i input.mp4 -ss 00:00:50 -codec copy -t 50 output.mp4
```

Here,

- –s – Indicates the starting time of the video clip. In our example, starting time is the 50th
  second.
- -t – Indicates the total time duration.

This is very helpful when you want to cut a part from an audio or video file using starting and
ending time.

Similarly, we can trim down the audio file like below.

```
$ ffmpeg -i audio.mp3 -ss 00:01:54 -to 00:06:53 -c copy output.mp3
```

**15. Split video files into multiple parts**

Some websites will allow you to upload only a specific size of video. In such cases, you can split
the large video files into multiple smaller parts like below.

```
$ ffmpeg -i input.mp4 -t 00:00:30 -c copy part1.mp4 -ss 00:00:30 -codec copy part2.mp4
```

Here, **-t 00:00:30** indicates a part that is created from the start of the video to the 30th
second of video. **-ss 00:00:30** shows the starting time stamp for the video. It means that the 2nd
part will start from the 30th second and will continue up to the end of the original video file.

\_\_**Recommended Download** –
[**Free Guide: “How to Start Your Own Successful Podcast”**](https://ostechnix.tradepub.com/free/w_make235/prgm.cgi)

**16. Joining multiple video parts into one**

FFmpeg will also join the multiple video parts and create a single video file.

Create **join.txt** file that contains the exact paths of the files that you want to join. All files
should be same format (same codec). The path name of all files should be mentioned one by one like
below.

```
/home/sk/myvideos/part1.mp4
/home/sk/myvideos/part2.mp4
/home/sk/myvideos/part3.mp4
/home/sk/myvideos/part4.mp4
```

Now, join all files using command:

```
$ ffmpeg -f concat -i join.txt -c copy output.mp4
```

The above command will join part1.mp4, part2.mp4, part3.mp4, and part4,mp4 files into a single file
called “output.mp4”.

**17. Add subtitles to a video file**

We can also add subtitles to a video file using FFmpeg. Download the correct subtitle for your video
and add it your video as shown below.

```
$ fmpeg -i input.mp4 -i subtitle.srt -map 0 -map 1 -c copy -c:v libx264 -crf 23 -preset veryfast output.mp4
```

**18. Preview or test video or audio files**

You might want to preview to verify or test whether the output file has been properly transcoded or
not. To do so, you can play it from your Terminal with command:

```
$ ffplay video.mp4
```

[![](http://www.ostechnix.com/wp-content/uploads/2017/05/Menu_004.png)](http://www.ostechnix.com/wp-content/uploads/2017/05/Menu_004.png)

Similarly, you can test the audio files as shown below.

```
$ ffplay audio.mp3
```

[![](http://www.ostechnix.com/wp-content/uploads/2017/05/Menu_005-3.png)](http://www.ostechnix.com/wp-content/uploads/2017/05/Menu_005-3.png)

**19. Increase/decrease video playback speed**

FFmpeg allows you to adjust the video playback speed. To increase the video playback speed, run:

```
$ ffmpeg -i inputvideo.mp4 -vf "setpts=0.5*PTS" outputvideo.mp4
```

To decrease playback speed, run:

```
$ ffmpeg -i inputvideo.mp4 -vf "setpts=4.0*PTS" outputvideo.mp4
```

**20. Getting help**

In this guide, I have merely covered the most commonly used FFmpeg commands. FFmpeg has a lot more
different options to do various advanced functions. To learn more about it, refer the man page.

```
$ man ffmpeg
```

And, that’s all. I hope this guide will help you to getting started with FFmpeg. If you find this
guide useful, please share it on your social, and professional networks. More good stuffs to come.
Stay tuned!
