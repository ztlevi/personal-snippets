# Autorandr

https://github.com/phillipberndt/autorandr/blob/v1.0/README.md#how-to-use

## Step1: Modify monitor setup with arandr or xrandr

for example:

```bash
xrandr --output DisplayPort-0 --primary --auto  --output DisplayPort-1 --auto --right-of DisplayPort-0 --dpi 144
```

## Step2: Save the layout

```bash
autorandr --force --save default && autorandr --default default
```

## Step3: Activate the layout

By default, it activates the default layout. You can put this in `.xprofile` in order to activate on
boot.

```bash
autorandr --change || echo "No autorandr profile found!"
```
