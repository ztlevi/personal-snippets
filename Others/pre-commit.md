# [Pre-commit](https://pre-commit.com)

## Example plugins

`.pre-commit-config.yaml`

```yaml
# Install with `pip3 install pre-commit` after every clone
# Then do `pre-commit install` to install the hooks
repos:
  - repo: https://github.com/python/black
    rev: "master"
    hooks:
      - id: black
  - repo: https://github.com/prettier/prettier
    rev: "master" # Use the sha or tag you want to point at
    hooks:
      - id: prettier
  - repo: local
    hooks:
      - id: black
        name: black
        entry: black -q
        files: '\.py' # or use "types: [python]"
        language: system
      - id: git stage
        name: git stage update if files are formatted
        entry: git add -u
        language: system
      - id: Message
        name: "!!! If failed, do git commit again !!!"
        entry: echo "Do git commit again now!!!"
        language: system
```

## Repository local hooks

https://pre-commit.com/#repository-local-hooks

<small><a href="#repository-local-hooks">¶</a></small>

Repository-local hooks are useful when:

- The scripts are tightly coupled to the repository and it makes sense to distribute the hook
  scripts with the repository.
- Hooks require state that is only present in a built artifact of your repository (such as your
  app's virtualenv for pylint).
- The official repository for a linter doesn't have the pre-commit metadata.

You can configure repository-local hooks by specifying the `repo` as the sentinel `local`.

_new in 0.13.0_: local hooks can use any language which supports `additional_dependencies` or
`docker_image` / `fail` / `pcre` / `pygrep` / `script` / `system`. This enables you to install
things which previously would require a trivial mirror repository.

A `local` hook must define `id`, `name`, `language`, `entry`, and `files` / `types` as specified
under [Creating new hooks](https://pre-commit.com/#new-hooks).

Here's an example configuration with a few `local` hooks:

```yaml
- repo: local
  hooks:
    - id: pylint
      name: pylint
      entry: python -m pylint.__main__
      language: system
      types: [python]
    - id: black
      name: black
      entry: black -q
      files: \.py
      language: system
      types: [python]
    - id: check-x
      name: Check X
      entry: ./bin/check-x.sh
      language: script
      files: \.x$
    - id: scss-lint
      name: scss-lint
      entry: scss-lint
      language: ruby
      language_version: 2.1.5
      types: [scss]
      additional_dependencies: ["scss_lint:0.52.0"]
    - id: prettier
      name: prettier
      entry: prettier --write
      language: node
      files: "\\.(\
        css|less|scss\
        |graphql|gql\
        |html\
        |js|jsx\
        |json\
        |md|markdown|mdown|mkdn\
        |mdx\
        |ts|tsx\
        |vue\
        |yaml|yml\
        )$"
```
