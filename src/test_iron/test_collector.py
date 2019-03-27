import json
import os


PROJ_DIR = 'cargo_projs'


class TestData:
    def __init__(self, data, test_type):
        self.__dict__.update(data)
        self.test_type = test_type



def get_test_dir(filename):
    return  os.path.join(os.path.dirname(os.path.realpath(filename)),
                         PROJ_DIR)


def get_tests(filename):
    test_dir = get_test_dir(filename)
    tests = []
    for name in os.listdir(test_dir):
        proj_dir = os.path.join(test_dir, name)
        json_files = extract_files_with_extension(os.path.join(test_dir, name),
                                                  'json')
        for test_file in json_files:
            with open(os.path.join(proj_dir, test_file)) as f:
                data = json.load(f)
                for test_json in data['tests']:
                    test_json['name'] = name
                    test_data = TestData(test_json, data['proj_type'])
                    tests.append(test_data)
    return tests


def extract_files_with_extension(dir_name, extension):
    return [name for name in os.listdir(dir_name) if name.endswith(extension)]
