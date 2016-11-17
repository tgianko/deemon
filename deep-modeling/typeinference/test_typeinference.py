#!/usr/bin/env python

import unittest
import typeinference

class TestStringMethods(unittest.TestCase):
    BASIC_SAMPLES = [
        [["abc", "123", "1.2"], typeinference.STRING_TYPE],
        [["1", "2", "3"], typeinference.INT_TYPE],
        [["1", "123", "1.2"], typeinference.FLOAT_TYPE],
        [["123", "123", "1.2."], typeinference.STRING_TYPE],        
    ]

    def test_basic_inference(self):
        for test in self.BASIC_SAMPLES:
            inferred_type = typeinference.infer_syntactic_type(test[0])
            self.assertEqual(inferred_type, test[1])

if __name__ == '__main__':
    unittest.main()