# Compile Notes

```shell
# Insall Emacs source on Ubuntu 17.10 (xfce4)
uname -a
> Linux Slug 4.13.0-16-generic #19-Ubuntu SMP Wed Oct 11 18:35:14 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux
mkdir ~/src
cd ~/src
sudo apt-get update
sudo apt-get install git
git clone -b master git://git.sv.gnu.org/emacs.git
sudo apt-get install build-essential automake texinfo libjpeg-dev libncurses5-dev
sudo apt-get install libtiff5-dev libgif-dev libpng-dev libxpm-dev libgtk-3-dev libgnutls28-dev
cd emacs/
# read INSTALL.REPO
./autogen.sh
# configure recommended I add --with-mailutils
./configure --with-modules --with-mailutils
make
# check it's working
src/emacs --version
> GNU Emacs 27.0.50
# run it
src/emacs &
# install it globally
sudo make install
```
