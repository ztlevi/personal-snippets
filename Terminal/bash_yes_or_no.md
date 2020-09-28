# Bash yes or no

In response to your edit, here's how you'd create and use a `confirm` command based on the first
version in my answer (it would work similarly with the other two):

```
confirm() {
    # call with a prompt string or use a default
    read -r -p "${1:-Are you sure?} [y/N]" response
    case "$response" in
        [yY][eE][sS]|[yY])
            true
            ;;
        *)
            false
            ;;
    esac
}
```

To use this function:

```
confirm && hg push ssh://..
```

or

```
confirm "Would you really like to do a push?" && hg push ssh://..
confirm "Would you really like to do a push?" || exit 0
```
