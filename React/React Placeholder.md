```jsx
const App = () => (
  <React.Placeholder delayMs={1000} fallback={<Loading />}>
    <EntrySection />
  </React.Placeholder>
);

const EntrySection = () => <Entry />;

const Entry = () => {
  // throw Promise if a cache doesn’t have an entry data
  const entry = fetchEntryWithCache();
  return <p>{entry.data}</p>;
};
```
