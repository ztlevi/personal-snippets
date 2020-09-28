By far the easiest way to use SQLite with electron is with electron-builder.

First, add a postinstall step in your package.json:

```
"scripts": {
   "postinstall": "install-app-deps"
   ...
}
```

and then install the necessary dependencies and build:

```
npm install --save-dev electron-builder
npm install --save sqlite3
npm run postinstall
```

electron-builder will build the native module for your platform, with the correct name for the
Electron binding; and you can then require it in code as normal.
