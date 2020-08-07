# Flat-remix desktop theme

For those of you want to try a different theme on Ubuntu. Here is the instructions.

1. Logout and login via Gnome desktop environment. You can see a gear button when inputing password. Click the gear
   button and select Gnome.
2. Install `sudo apt install chrome-gnome-shell`.

   Then install [Gnome user theme extension](https://extensions.gnome.org/extension/19/user-themes/) via browser.

3. Use the following commands to extract themes and icons

```sh
cd /tmp
# themes
git clone https://github.com/daniruiz/flat-remix-gtk.git
mkdir -p ~/.themes
cp -r flat-remix-gtk/Flat-Remix-GTK-* ~/.themes
# icons
git clone https://github.com/daniruiz/flat-remix.git
mkdir -p ~/.icons
cp -r flat-remix/Flat-Remix-* ~/.icons
```

5. Install gnome-tweaks with `sudo apt-get install gnome-tweak-tool`. You should be able to select the theme and icons
   under Gnome-tweaks' Appearance section

# What's more?

[Dash to Dock](https://extensions.gnome.org/extension/307/dash-to-dock/) is an alternative to the default dock.

And more themes at [Opendesktop](https://www.pling.com/browse/cat/366/order/latest/)
