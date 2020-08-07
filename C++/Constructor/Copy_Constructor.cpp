#include <bits/stdc++.h>

using namespace std;

// MemoryBlock.h
#pragma once
#include <algorithm>
#include <iostream>

class MemoryBlock {
public:
  // Simple constructor that initializes the resource.
  explicit MemoryBlock(size_t length)
      : _length(length), _data(new int[length]) {
    std::cout << "In MemoryBlock(size_t). length = " << _length << "."
              << std::endl;
  }

  // Destructor.
  ~MemoryBlock() {
    std::cout << "In ~MemoryBlock(). length = " << _length << ".";

    if (_data != nullptr) {
      std::cout << " Deleting resource.";
      // Delete the resource.
      delete[] _data;
    }

    std::cout << std::endl;
  }

  // Copy constructor.
  MemoryBlock(const MemoryBlock &other)
      : _length(other._length), _data(new int[other._length]) {
    std::cout << "In MemoryBlock(const MemoryBlock&). length = "
              << other._length << ". Copying resource." << std::endl;

    std::copy(other._data, other._data + _length, _data);
  }

  // Copy assignment operator.
  MemoryBlock &operator=(const MemoryBlock &other) {
    std::cout << "In operator=(const MemoryBlock&). length = " << other._length
              << ". Copying resource." << std::endl;

    if (this != &other) {
      // Free the existing resource.
      delete[] _data;

      _length = other._length;
      _data = new int[_length];
      std::copy(other._data, other._data + _length, _data);
    }
    return *this;
  }

  // Retrieves the length of the data resource.
  size_t Length() const { return _length; }

private:
  size_t _length; // The length of the resource.
  int *_data;     // The resource.
};

int main() {
  // Create a vector object and add a few elements to it.
  vector<MemoryBlock> v;
  v.push_back(MemoryBlock(25));
  v.push_back(MemoryBlock(75));

  // Insert a new element into the second position of the vector.
  v.insert(v.begin() + 1, MemoryBlock(50));
}
