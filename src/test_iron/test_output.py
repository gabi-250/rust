import os
import pytest
import subprocess
from . import test_collector


def clean_subproject(proj_name):
    cmd = "make proj={} clean".format(proj_name)
    p = subprocess.run(cmd.split())
    assert p.returncode == 0, 'Failed to run {}'.format(cmd)


def build_subproject(proj_name):
    clean_subproject(proj_name)
    cmd = "make proj={}".format(proj_name)
    p = subprocess.run(cmd.split())
    assert p.returncode == 0, 'Failed to run {}'.format(cmd)


@pytest.mark.parametrize('test_name,test_in,test_out', test_collector.get_tests(__file__))
def test_stdout(test_name, test_in, test_out):
    build_subproject(test_name)
    cmd = "./build/test_{} {}".format(test_name, test_in)
    p = subprocess.run(cmd.split(),
                       stdout=subprocess.PIPE)
    assert p.stdout == bytes(test_out, encoding='ascii')

