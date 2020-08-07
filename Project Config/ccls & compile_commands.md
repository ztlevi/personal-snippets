# Build CCLS

https://github.com/MaskRay/ccls/wiki/Build

```
brew install cmake
```

Ubuntu install [gcc-7](https://gist.github.com/jlblancoc/99521194aba975286c80f93e47966dc5) and `sudo apt install zlib1g-dev libncurses-dev`.

```shell
git clone --depth=1 --recursive https://github.com/MaskRay/ccls
cd ccls
wget -c http://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
tar xf clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$PWD/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04
cmake --build Release
sudo cp -f ./Release/ccls /usr/local/bin/ccls
```

#### [No `.ccls` or `compile_commands.json`](https://github.com/MaskRay/ccls/wiki/Getting-started#no-ccls-or-compile_commandsjson)

If neither file exists, ccls assumes there is an imaginary `.ccls` with one line: `%clang`.

This is convenient for trying out a one-file project. `git init` to let the language client know it is a project, e.g. `cd /tmp; mkdir c; cd c; git init; touch a.cc`

## Cmake

```shell
 mkdir -p cmake-build-debug
 (cd cmake-build-debug; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=YES ..)
 ln -s cmake-build-debug/compile_commands.json
```

## Build EAR

Bear is a tool that generates a compilation database for clang tooling. It can be used for any project based on Makefile.

```shell
bear make
# generates compile_commands.json
```

# Clang-format options

https://clang.llvm.org/docs/ClangFormatStyleOptions.html
