## https://developers.google.com/web/tools/chrome-devtools/network-performance/reference#filter-by-type

# Filter requests by properties

Use the Filter text box to filter requests by properties, such as the domain or size of the request.

If you can't see the text box, the Filters pane is probably hidden. See Hide the Filters pane.

You can use multiple properties simultaneously by separating each property with a space. For
example, `mime-type:image/gif` `larger-than:1K` displays all GIFs that are larger than one kilobyte.
These multi-property filters are equivalent to AND operations. OR operations are currently not
supported.

Below is a complete list of supported properties.

- `domain`. Only display resources from the specified domain. You can use a wildcard character (_)
  to include multiple domains. For example, _.com displays resources from all domain names ending in
  .com. DevTools populates the autocomplete dropdown menu with all of the domains it has
  encountered.
- `has-response-header`. Show the resources that contain the specified HTTP response header.
  DevTools populates the autocomplete dropdown with all of the response headers that it has
  encountered.
- `is`. Use is:running to find WebSocket resources.
- `larger-than`. Show resources that are larger than the specified size, in bytes. Setting a value
  of 1000 is equivalent to setting a value of 1k.
- `method`. Show resources that were retrieved over a specified HTTP method type. DevTools populates
  the dropdown with all of the HTTP methods it has encountered.
- `mime-type`. Show resources of a specified MIME type. DevTools populates the dropdown with all
  MIME types it has encountered.
- `mixed-content`. Show all mixed content resources (mixed-content:all) or just the ones that are
  currently displayed (mixed-content:displayed).
- `scheme`. Show resources retrieved over unprotected HTTP (scheme:http) or protected HTTPS
  (scheme:https).
- `set-cookie-domain`. Show the resources that have a Set-Cookie header with a Domain attribute that
  matches the specified value. DevTools populates the autocomplete with all of the cookie domains
  that it has encountered.
- `set-cookie-name`. Show the resources that have a Set-Cookie header with a name that matches the
  specified value. DevTools populates the autocomplete with all of the cookie names that it has
  encountered.
- `set-cookie-value`. Show the resources that have a Set-Cookie header with a value that matches the
  specified value. DevTools populates the autocomplete with all of the cookie values that it has
  encountered.
- `status-code`. Only show resources whose HTTP status code match the specified code. DevTools
  populates the autocomplete dropdown menu with all of the status codes it has encountered.

# Filter requests by type

To filter requests by request type, click the **XHR, JS, CSS, Img, Media, Font, Doc, WS (WebSocket),
Manifest, or Other** (any other type not listed here) buttons on the Network panel.

If you can't see these buttons, the Filters pane is probably hidden. See Hide the Filters pane.

To enable multiple type filters simultaneously, hold Command (Mac) or Control (Windows, Linux) and
then click.

# Filter requests by time

Click and drag left or right on the Overview pane to only display requests that were active during
that time frame. The filter is inclusive. Any request that was active during the highlighted time is
shown.
