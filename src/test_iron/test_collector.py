import json
import os


CARGO_PROJS = 'cargo_projs'


def get_test_dir(filename):
    return  os.path.join(os.path.dirname(os.path.realpath(filename)),
                         CARGO_PROJS)


def get_tests(filename):
    test_dir = get_test_dir(filename)
    tests = []
    for name in os.listdir(test_dir):
        test_name = name
        proj_dir = os.path.join(test_dir, name)
        json_files = extract_files_with_extension(os.path.join(test_dir, name),
                                                  'json')
        for test_file in json_files:
            with open(os.path.join(proj_dir, test_file)) as f:
                data = json.load(f)
                for test in data:
                    tests.append((test_name, test['input'], test['output']))
    return tests


def extract_files_with_extension(dir_name, extension):
    return [name for name in os.listdir(dir_name) if name.endswith(extension)]
