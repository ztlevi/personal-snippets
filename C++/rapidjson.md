# RapidJson

## Basic

1. Parse a JSON string into DOM.

```cpp
    const char* json = "{\"project\":\"rapidjson\",\"stars\":10}";
    Document d;
    d.Parse(json);
```

2. Modify it by DOM.

```cpp
    Value& s = d["stars"];
    auto& a = d["stars"];
    s.SetInt(s.GetInt() + 1);
```

3. Stringify the DOM

```cpp
    StringBuffer buffer;
    Writer<StringBuffer> writer(buffer);
    d.Accept(writer);
```

## Iterate

```c++
  for (rj::Value::ConstMemberIterator iter = document.MemberBegin();
       iter != document.MemberEnd(); ++iter) {
    auto key = iter->name.GetString();
    auto arrs = iter->value.GetArray();
    for (rj::SizeType i = 0; i < arrs.Size(); ++i) {
      auto arr = arrs[i].GetInt();
    }
  }
```
