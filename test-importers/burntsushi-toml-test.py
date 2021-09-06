#! /usr/bin/env python3

"""
Import tests from <https://github.com/BurntSushi/toml-test/>.

Run this script from the top-level directory in ada-toml's repository, passing
to it the path to a checkout of the toml-test repository as its first argument.
"""

import json
import os.path
import shutil
import sys

import yaml


burntsushi_root = sys.argv[1]
tests_subdir = os.path.join(burntsushi_root, "tests")
output_dir = os.path.join(os.getcwd(), 'tests', 'burntsushi-toml-test')
os.mkdir(output_dir)


def in_path(*args):
    return os.path.join(burntsushi_root, *args)


def out_path(*args):
    return os.path.join(output_dir, *args)


def create_test_name(toml_file):
    relname = os.path.relpath(toml_file, tests_subdir)
    return relname[:-5]


def write_test_yaml(test_dir, content):
    with open(os.path.join(test_dir, 'test.yaml'), 'w', encoding='utf-8') as f:
        yaml.dump(content, f)


def import_valid(toml_file):
    assert toml_file.endswith('.toml')
    test_name = create_test_name(toml_file)
    json_file = toml_file[:-5] + '.json'

    # Create the test directory
    test_dir = out_path(test_name)
    os.makedirs(test_dir)

    # Copy the TOML to parse
    shutil.copy(toml_file, os.path.join(test_dir, 'input.toml'))

    # Create the test.yaml, including the expected JSON
    with open(json_file, 'r', encoding='utf-8') as f:
        json_doc = json.load(f)
    write_test_yaml(test_dir, {"driver": "decoder", "output": json_doc})


def import_invalid(toml_file):
    assert toml_file.endswith('.toml')
    test_name = create_test_name(toml_file)

    # Create the test directory
    test_dir = out_path(test_name)
    os.makedirs(test_dir)

    # Copy the TOML to parse
    shutil.copy(toml_file, os.path.join(test_dir, 'input.toml'))

    # Create the test.yaml
    write_test_yaml(test_dir, {"driver": "decoder", "error": True})


def iter_tests(root_dir):
    for path, _, filenames in os.walk(root_dir):
        for fn in filenames:
            if fn.endswith(".toml"):
                yield os.path.join(path, fn)


for valid in iter_tests(in_path('tests', 'valid')):
    import_valid(valid)

for valid in iter_tests(in_path('tests', 'invalid')):
    import_invalid(valid)
