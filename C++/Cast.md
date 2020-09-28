# Cast

- `static_cast` performs implicit conversions, the reverses of implicit standard conversions, and
  (possibly unsafe) base to derived conversions.
- `reinterpret_cast` converts one pointer to another without changing the address, or converts
  between pointers and their numerical (integer) values.
- `const_cast` only changes cv-qualification; all other casts cannot cast away constness.
- `dynamic_cast` casts up and down class hierarchies only, always checking that the conversion
  requested is valid.
