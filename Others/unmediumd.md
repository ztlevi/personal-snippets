# unmediumed

A Markdown API for Medium that deploys to AWS Lambda.

## Usage

Change the domain name of a Medium post URL to `unmediumed.com` to view as Markdown. The `http` protocol and the
`medium.com` domain will be automatically assumed for `medium.com` domain posts. For publications with custom domains
the full URL should be provided.

```
# with protocol and medium domain
https://unmediumed.com/https://medium.com/@danielireson/three-tips-when-using-npm-as-a-website-build-system-827d29606715

# without protocol
https://unmediumed.com/medium.com/@danielireson/three-tips-when-using-npm-as-a-website-build-system-827d29606715

# without medium domain
https://unmediumed.com/@danielireson/three-tips-when-using-npm-as-a-website-build-system-827d29606715

# publication with custom domain
https://unmediumed.com/https://medium.freecodecamp.org/how-to-build-a-serverless-url-shortener-using-aws-lambda-and-s3-4fbdf70cbf5c
```
