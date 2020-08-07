# Magick

```sh
# install imagemagick
brew install imagemagick

# convert a single image
magick convert foo.HEIC foo.jpg

# bulk convert multiple images
magick mogrify -monitor -format jpg *.HEIC

# Optimize
convert input.jpg -scale 20% -size 24% -quality [0..100] output.jpg
```
