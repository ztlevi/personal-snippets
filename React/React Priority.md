https://github.com/koba04/react-timeslicing-demo/blob/master/src/App.js

```jsx
<InputText
  value={inputText}
  onChange={({ target: { value } }) => {
    this.setState(() => ({ inputText: value })); // high priority
    const wrapper = enabledTimeSlicing ? requestAnimationFrame : cb => cb(); // requestAnimationFrame has low priority
    wrapper(() => {
      this.setState(() => ({ listText: value }));
    });
  }}
/>
```
