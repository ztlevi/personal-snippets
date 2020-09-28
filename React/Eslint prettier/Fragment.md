```shell
npm i -S eslint prettier babel-eslint
```

```shell
npm i -D eslint-config-prettier eslint-plugin-babel eslint-plugin-prettier eslint-plugin-react eslint-plugin-import
```

.eslintrc

```json
  "extends": [
    "eslint:recommended",
    "prettier",
    "prettier/react",
    "plugin:react/recommended",
    "plugin:import/errors",
    "plugin:import/warnings"
  ],
```

CLI helper tool eslint-config-prettier also ships with a little CLI tool to help you check if your
configuration contains any rules that are unnecessary or conflict with Prettier.

First, add a script for it to package.json:

```
{
  "scripts": {
    "eslint-check": "eslint --print-config .eslintrc.js | eslint-config-prettier-check"
  }
}
```

Then run npm run eslint-check.

(Swap out .eslintrc.js with the path to your config if needed.)

Exit codes:

0: No problems found. 1: Unexpected error. 2: Conflicting rules found.
