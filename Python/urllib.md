# Urllib

```python
try:
    import urlparse
    from urllib import urlencode
except: # For Python 3
    import urllib.parse as urlparse
    from urllib.parse import urlencode

url = "http://stackoverflow.com/search?q=question"
params = {'lang':'en','tag':'python'}

url_parts = list(urlparse.urlparse(url))
query = dict(urlparse.parse_qsl(url_parts[4]))
query.update(params)

url_parts[4] = urlencode(query)

print(urlparse.urlunparse(url_parts))
```
