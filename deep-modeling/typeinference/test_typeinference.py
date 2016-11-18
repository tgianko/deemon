#!/usr/bin/env python

import unittest
import typeinference

class TestSyntacticInference(unittest.TestCase):
    BASIC_SAMPLES = [
        [["abc", "123", "1.2"], typeinference.STRING_TYPE],
        [["1", "2", "3"], typeinference.INT_TYPE],
        [["1", "123", "1.2"], typeinference.FLOAT_TYPE],
        [["123", "123", "1.2."], typeinference.STRING_TYPE],   
        [[""], typeinference.STRING_TYPE],   
        [[".123", "123", "1.2"], typeinference.FLOAT_TYPE],
        [["0", "09"], typeinference.INT_TYPE],                                                  
    ]

    ADVANCED_SAMPLES = [
        [["abc", "abc", "1..2"], typeinference.STRING_TYPE],
        [["true", "false"], typeinference.BOOL_TYPE],
        # [["1", "2", "3"], typeinference.INT_TYPE], # TODO UUID
        [["a872fcd", "a", "0", "123"], typeinference.HEX_TYPE], # TODO HEX
        [["https://www.google.de", "http://heise.de"], typeinference.URL_TYPE], # URL
        [["www.google.de"], typeinference.STRING_TYPE], # An URL needs a protocol        
        [[""], typeinference.STRING_TYPE]
    ]

    def test_basic_inference(self):
        self._run_matrix_test(self.BASIC_SAMPLES)
        

    def test_advanced_inference(self):
        self._run_matrix_test(self.ADVANCED_SAMPLES)


    def _run_matrix_test(self, matrix):
        for idx, test in enumerate(matrix):
            inferred_type = typeinference.infer_syntactic_type(test[0])
            self.assertEqual(inferred_type, test[1], "Assertion no. " + str(idx) + " resolved to type: " + str(inferred_type))


if __name__ == '__main__':
    unittest.main()