import test_collector

import os
import subprocess
import sys
import unittest


class TestOutput(unittest.TestCase):
    def __init__(self, test_data):
        super(TestOutput, self).__init__()
        self.test_data = test_data

    def clean_subproject(self):
        cmd = "make proj={} clean".format(self.test_data.name)
        p = subprocess.run(cmd.split(),
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
        self.assertEqual(p.returncode, 0,
                         'Failed to run {}: {} {}'.format(cmd,
                                                          p.stdout,
                                                          p.stderr))

    def build_subproject(self):
        self.clean_subproject()
        cmd = "make {}_tests proj={}".format(self.test_data.test_type,
                                             self.test_data.name)
        p = subprocess.run(cmd.split(),
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
        self.assertEqual(p.returncode, 0,
                         'Failed to run {}: {} {}'.format(cmd,
                                                          p.stdout,
                                                          p.stderr))

    def setUp(self):
        self.build_subproject()

    def runTest(self):
        cmd = "./build/test_{} {}".format(self.test_data.name,
                                          self.test_data.input)
        p = subprocess.run(cmd.split(),
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
        self.assertEqual(p.returncode, self.test_data.exit_code,
                         "{}: in={} out={}".format(self.test_data.name,
                                                   self.test_data.input,
                                                   self.test_data.output))
        self.assertEqual(p.stdout, bytes(self.test_data.output, encoding='ascii'),
                         "{}: in={} out={}".format(self.test_data.name,
                                                   self.test_data.input,
                                                   self.test_data.output))


if __name__ == '__main__':
    suite = unittest.TestSuite()
    output_tests = test_collector.get_tests(__file__)
    tests = [TestOutput(test) for test in output_tests]
    suite.addTests(tests)
    result = unittest.TextTestRunner().run(suite)
    sys.exit(0 if result.wasSuccessful() else 1)
