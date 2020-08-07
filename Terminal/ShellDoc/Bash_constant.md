# Constant

### Constants and Environment Variable Names[](https://google.github.io/styleguide/shellguide.html#constants-and-environment-variable-names)

All caps, separated with underscores, declared at the top of the file.

Constants and anything exported to the environment should be capitalized.

```bash
# Constant
readonly PATH_TO_FILES='/some/path'

# Both constant and environment
declare -xr ORACLE_SID='PROD'
```

Some things become constant at their first setting (for example, via getopts). Thus, it’s OK to set a constant in
getopts or based on a condition, but it should be made readonly immediately afterwards. For the sake of clarity
`readonly` or `export` is recommended instead of the equivalent `declare` commands.

```bash
VERBOSE='false'
while getopts 'v' flag; do
  case "${flag}" in
    v) VERBOSE='true' ;;
  esac
done
readonly VERBOSE
```
