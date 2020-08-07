package.json```
{
    "scripts":{
        "dev": "node -r babel-register src/bin/www"
        "clean": "rm -rf dist",
        "build": "npm run clean && mkdir dist && babel server -s -d dist",
        "production": "npm run build && node dist/bin/www"
    },
    "devDependencies": {
        "babel-cli": "xxx",
        "babel-preset-env": "xxx",
        "babel-register": "xxx",
    }
}
```;
