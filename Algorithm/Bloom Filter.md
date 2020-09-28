https://www.youtube.com/watch?v=-SuTGoFYjZs

# False positive

False positive matches are possible, but false negatives are not – in other words, a query returns
either "possibly in set" or "definitely not in set".

- False positive: a result that indicates that a given condition is present when it actually is not
  present
- False negative: where an actual 'hit' was disregarded by the test and seen as a 'miss'

# Scalable Bloom filters

https://redislabs.com/blog/rebloom-bloom-filter-datatype-redis/

Typically Bloom filters must be created with a foreknowledge of how many entries they will contain.
The bpe number needs to be fixed, and likewise the width of the bit array is also fixed.

Unlike hash tables, Bloom filters cannot be “rebalanced” because there is no way to know which
entries are part of the filter (the filter can only determine whether a given entry is not present,
but does not actually store the entries which are present).

In order to allow Bloom filters to ‘scale’ and be able to accommodate more elements than they’ve
been designed to, they may be stacked. Once a single Bloom filter reaches capacity, a new one is
created atop it. Typically the new filter will have greater capacity than the previous one in order
to reduce the likelihood of needing to stack yet another filter.

In a stackable (scalable) Bloom filter, checking for membership now involves inspecting each layer
for presence. Adding new items now involves checking that it does not exist beforehand, and adding
it to the current filter. Hashes still only need to be computed once however.

When creating a Bloom filter – even a scalable one, it’s important to have a good idea of how many
items it is expected to contain. A filter whose initial layer can only contain a small number of
elements will degrade performance significantly because it will take more layers to reach a larger
capacity.
