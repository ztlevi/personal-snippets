# ZSTD

## Compression

For example, you can compress a directory with `tar` using zstd with the command line:

```bash
tar --zstd -cf directory.tar.zst directory/
```

If you have an older version of `tar` without native zstd support you can use:

```bash
tar --use-compress-program zstd -cf directory.tar.zst directory/
```

## Decompression

```bash
tar -zstd -xvf archive.tar.zst
```
