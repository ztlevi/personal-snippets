#include <bits/stdc++.h>

using namespace std;

enum Type { Club, Dimand, Hert, Spade };

class Suit {
private:
  int value;
  Suit(int v) { value = v; }

public:
  int getValue() { return value; }
  static Suit getSuitFromValue(int value){...};
};

class Card {
  bool available = true;
  int faceValue;
  Suit suit;

public:
  Card(int c, Suit s) {
    faceValue = c;
    suit = s;
  }
  virtual int value();
};
