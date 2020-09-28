# Tutorial

Using Chrome (but Firefox will probably also work).

- Open DevTools
- Click the Download button.
- Download but cancel immediately
- Open the 'Network' tab in DevTools. Search for 'Zip?authKey=' in DevTools and open it (click).
  This is a POST request.
- `<Download URL>` is the General section's Request URL.
- Click 'View source' to the right of 'Form data' at the bottom, that's `<raw form data>`
- Construct the command as follows:

`wget --post-data='<raw form data>' '<Download URL>'`
