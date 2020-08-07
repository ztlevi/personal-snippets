# MAP

```
map<string, int>::iterator it;

for ( it = symbolTable.begin(); it != symbolTable.end(); it++ )
{
    std::cout << it->first  // string (key)
              << ':'
              << it->second   // string's value
              << std::endl ;
}
```

With C++11 ( and onwards ),

```
for (auto const& x : symbolTable)
{
    std::cout << x.first  // string (key)
              << ':'
              << x.second // string's value
              << std::endl ;
}
```

With C++17 ( and onwards ),

```
for( auto const& [key, val] : symbolTable )
{
    std::cout << key         // string (key)
              << ':'
              << val        // string's value
              << std::endl ;
}
```

Using iterator next:

```
  unordered_map<int, set<int>> x;
  for (auto i = x.begin(); i != x.end(); ++i)
    for (auto j = next(i); j != x.end(); ++j) {
```

# Set

```
std::set<unsigned long>::iterator it;
for (it = SERVER_IPS.begin(); it != SERVER_IPS.end(); ++it)
{
    u_long f = *it; // Note the "*" here
}
```

If you have C++11 features, you can use a range-based for loop:

```
for(auto const& f : SERVER_IPS) {
  // use f here
}
```

# Vector

Using Range C++11

```
for(auto const& value: a) {
     /* std::cout << value; ... */
```
