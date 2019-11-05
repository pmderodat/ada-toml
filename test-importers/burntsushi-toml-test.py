#! /usr/bin/env python3

"""
Import tests from <https://github.com/BurntSushi/toml-test/>.

Run this script from the top-level directory in ada-toml's repository, passing
to it the path to a checkout of the toml-test repository as its first argument.
"""

import glob
import os.path
import shutil
import sys


burntsushi_root = sys.argv[1]
output_dir = os.path.join(os.getcwd(), 'tests', 'burntsushi-toml-test')
os.mkdir(output_dir)


def in_path(*args):
    return os.path.join(burntsushi_root, *args)


def out_path(*args):
    return os.path.join(output_dir, *args)


def create_test_name(toml_file):
    return os.path.basename(toml_file)[:-5]


def write_test_yaml(test_dir, content):
    with open(os.path.join(test_dir, 'test.yaml'), 'w', encoding='utf-8') as f:
        print(content, file=f)


def import_valid(toml_file):
    assert toml_file.endswith('.toml')
    test_name = create_test_name(toml_file)
    json_file = os.path.join(os.path.dirname(toml_file), test_name + '.json')

    # Create the test directory
    test_dir = out_path('valid', test_name)
    os.mkdir(test_dir)

    # Copy the TOML to parse
    shutil.copy(toml_file, os.path.join(test_dir, 'input.toml'))

    # Create the test.yaml, including the expected JSON
    with open(json_file, 'r', encoding='utf-8') as f:
        json = f.read().strip()
    write_test_yaml(test_dir,
                    'driver: decoder'
                    '\noutput: {}'.format(json))


def import_invalid(toml_file):
    assert toml_file.endswith('.toml')
    test_name = create_test_name(toml_file)

    # Create the test directory
    test_dir = out_path('invalid', test_name)
    os.mkdir(test_dir)

    # Copy the TOML to parse
    shutil.copy(toml_file, os.path.join(test_dir, 'input.toml'))

    # Create the test.yaml
    write_test_yaml(test_dir,
                    'driver: decoder'
                    '\nerror: True')


os.mkdir(out_path('valid'))
for valid in glob.glob(in_path('tests', 'valid', '*.toml')):
    import_valid(valid)

os.mkdir(out_path('invalid'))
for valid in glob.glob(in_path('tests', 'invalid', '*.toml')):
    import_invalid(valid)
