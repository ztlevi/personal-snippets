## https://www.w3schools.com/css/css_image_sprites.asp

# Image Sprites - Simple Example

Instead of using three separate images, we use this single image ("img_navsprites.gif"):

```css
#home {
  width: 46px;
  height: 44px;
  background: url(img_navsprites.gif) 0 0;
}

#next {
  width: 43px;
  height: 44px;
  background: url(img_navsprites.gif) -91px 0;
}
```
