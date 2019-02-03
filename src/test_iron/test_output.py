import test_collector

import os
import subprocess
import sys
import unittest


class TestOutput(unittest.TestCase):
    def __init__(self, test_name, test_in, test_out):
        super(TestOutput, self).__init__()
        self.test_name = test_name
        self.test_in = test_in
        self.test_out = test_out

    def clean_subproject(self):
        cmd = "make proj={} clean".format(self.test_name)
        p = subprocess.run(cmd.split(),
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
        self.assertEqual(p.returncode, 0,
                         'Failed to run {}: {} {}'.format(cmd,
                                                          p.stdout,
                                                          p.stderr))

    def build_subproject(self):
        self.clean_subproject()
        cmd = "make proj={}".format(self.test_name)
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
        cmd = "./build/test_{} {}".format(self.test_name, self.test_in)
        p = subprocess.run(cmd.split(),
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
        assert p.stdout == bytes(self.test_out, encoding='ascii')


if __name__ == '__main__':
    suite = unittest.TestSuite()
    test_data = test_collector.get_tests(__file__)
    tests = [TestOutput(name, t_in, t_out) for name, t_in, t_out in test_data]
    suite.addTests(tests)
    result = unittest.TextTestRunner().run(suite)
    sys.exit(0 if result.wasSuccessful() else 1)
