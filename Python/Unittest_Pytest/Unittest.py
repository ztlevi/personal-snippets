assert var1 == var2, msg
assert var1 != var2, msg
assert expr, msg
# Incidentally, during the course of jotting down this blog I reviewed some old inherited code and changed this:
self.assertEqual(len(errors),0)
# into this:
assert len(errors) == 0


import unittest

class TestStringMethods(unittest.TestCase):

    def test_upper(self):
        self.assertEqual('foo'.upper(), 'FOO')

    def test_isupper(self):
        self.assertTrue('FOO'.isupper())
        self.assertFalse('Foo'.isupper())

    def test_split(self):
        s = 'hello world'
        self.assertEqual(s.split(), ['hello', 'world'])
        # check that s.split fails when the separator is not a string
        with self.assertRaises(TypeError):
            s.split(2)

if __name__ == '__main__':
    unittest.main()
    
    