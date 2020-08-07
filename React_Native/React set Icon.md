## https://blog.bam.tech/developper-news/change-your-react-native-app-icons-in-a-single-command-line

To use it, you need node 6 installed.

Then, install the generator with:

```shell
npm install -g yo generator-rn-toolbox
```

To generate your icons, the generator uses ImageMagick. On macOS, you can install it with:

```shell
brew install imagemagick
```

```shell
yo rn-toolbox:assets --icon <path to your icon>
# For instance
yo rn-toolbox:assets --icon ../icon.png
```
