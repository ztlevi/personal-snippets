# Eslint

```sh
# For modern eslint & prettier
npm i -g eslint babel-eslint eslint-plugin-babel eslint-plugin-react \
  eslint-plugin-import eslint-plugin-prettier eslint-config-prettier
```

# .prettierc

```
printWidth: 80
singleQuote: true
trailingComma: es5
bracketSpacing: true
semi: true
useTabs: false
tabWidth: 2
```

# prettier all js files

`prettier --write "**/*.js"`

# package.json

`"prettier": "prettier --write \"**/*.js\"",`

# STEPS

1. Add prettier to your project:

```bash
yarn add prettier --dev --exact
```

2. Verify by running against a file:

```bash
yarn prettier --write src/index.js
```

3. Run prettier when commiting files:

```bash
yarn add pretty-quick husky --dev
```

Then edit package.json:

```json
{
  "scripts": {
    "precommit": "pretty-quick --staged"
  }
}
```

# Hooks: https://prettier.io/docs/en/precommit.html

Install it along with husky:

```bash
yarn add lint-staged husky --dev
```

and add this config to your package.json:

```json
{
  "husky": {
    "hooks": {
      "pre-commit": "lint-staged"
    }
  },
  "lint-staged": {
    "*.{js,json,css,md}": ["prettier --write", "git add"]
  }
}
```

See https://github.com/okonet/lint-staged#configuration for more details about how you can configure lint-staged.
