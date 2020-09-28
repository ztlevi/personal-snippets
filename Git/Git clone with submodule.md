# Clone with modules

```
git clone --recurse-submodules -j8 <url>
```

# Update all modules

```bash
git submodule deinit --force --all
git submodule update --init --recursive
```
