#!/usr/bin/env python

import unittest
import typeinference


class TestSyntacticInference(unittest.TestCase):
    BASIC_SAMPLES = [
        [["abc", "123", "1.2"], typeinference.SYN_TYPE_STRING],
        [["1", "2", "3"], typeinference.SYN_TYPE_INT],
        [["1", "123", "1.2"], typeinference.SYN_TYPE_FLOAT],
        [["123", "123", "1.2."], typeinference.SYN_TYPE_STRING],
        [[""], typeinference.SYN_TYPE_STRING],
        [[".123", "123", "1.2"], typeinference.SYN_TYPE_FLOAT],
        [["0", "09"], typeinference.SYN_TYPE_INT],
    ]

    ADVANCED_SAMPLES = [
        [["abc", "abc", "1..2"], typeinference.SYN_TYPE_STRING],
        [["true", "false"], typeinference.SYN_TYPE_BOOL],
        [["123e4567-e89b-12d3-a456-426655440000",
            "123e4567-e89b-12d3-a456-42665544abcd"], typeinference.SYN_TYPE_UUID],
        [["a872fcd", "a", "0", "123"], typeinference.SYN_TYPE_HEX],
        [["https://www.google.de", "http://heise.de"], typeinference.SYN_TYPE_URL],
        [["www.google.de"], typeinference.SYN_TYPE_STRING],  # An URL needs a protocol
        [["", "123e4567-e89b-12d3-a456-42665544000"], typeinference.SYN_TYPE_STRING], # Last block is just 11 digits long
        [["../", "test/a.html"], typeinference.SYN_TYPE_PATH]
    ]

    def test_basic_inference(self):
        self._run_matrix_test(self.BASIC_SAMPLES)

    def test_advanced_inference(self):
        self._run_matrix_test(self.ADVANCED_SAMPLES)

    def _run_matrix_test(self, matrix):
        for idx, test in enumerate(matrix):
            inferred_type = typeinference.infer_syntactic_type(test[0])
            self.assertEqual(inferred_type, test[
                             1], "Assertion no. " + str(idx) + " resolved to type: " + str(inferred_type))


class TestSemanticInference(unittest.TestCase):
    SAMPLES = [
        [[{"user": "user1", "value": "a"}, {"user": "user2", "value": "a"}], typeinference.SEM_TYPE_CONSTANT],
        [[{"user": "user1", "value": "a"}, {"user": "user2", "value": "a"}, {"user": "user2", "value": "b"}], typeinference.SEM_TYPE_UNCERTAIN],
        [[{"user": "user1", "value": "a"}, {"user": "user2", "value": "c"}, {"user": "user2", "value": "b"}], typeinference.SEM_TYPE_SESSION_UNIQUE],
        [[{"user": "user1", "value": "a"}, {"user": "user2", "value": "b"}, {"user": "user2", "value": "b"}], typeinference.SEM_TYPE_USER_UNIQUE]
    ]

    def test_semantic_inference(self):
        self._run_matrix_test(self.SAMPLES)

    def _run_matrix_test(self, matrix):
        for idx, test in enumerate(matrix):
            inferred_type = typeinference.infer_semantic_type(test[0])
            self.assertEqual(inferred_type, test[
                             1], "Assertion no. " + str(idx) + " resolved to type: " + str(inferred_type))


if __name__ == '__main__':
    unittest.main()
