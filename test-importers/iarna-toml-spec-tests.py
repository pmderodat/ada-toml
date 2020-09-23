#! /usr/bin/env python3

"""
Import tests from <https://github.com/iarna/toml-spec-tests/>.

Run this script from the top-level directory in ada-toml's repository, passing
to it the path to a checkout of the toml-spec-test repository as its first
argument.
"""

import datetime
import glob
import json
import os.path
import shutil
import sys

import yaml


# Some test YAML documents create big recursions in the YAML parser
sys.setrecursionlimit(10000)


iarna_root = sys.argv[1]
output_dir = os.path.join(os.getcwd(), 'tests', 'iarna-toml-spec-tests')
os.mkdir(output_dir)


def in_path(*args):
    return os.path.join(iarna_root, *args)


def out_path(*args):
    return os.path.join(output_dir, *args)


def create_test_name(toml_file):
    return os.path.basename(toml_file)[:-5]


def write_test_yaml(test_dir, content):
    with open(os.path.join(test_dir, 'test.yaml'), 'w', encoding='utf-8') as f:
        print(content, file=f)


def yaml_to_json(yaml):
    """
    Turn a YAML document to the equivalent JSON document that ada_toml_decode
    program will output.
    """
    if isinstance(yaml, dict):
        return {key: yaml_to_json(value)
                for key, value in yaml.items()}
    elif isinstance(yaml, list):
        return [yaml_to_json(item) for item in yaml]

    if yaml is False:
        kind = 'bool',
        image = 'false'
    elif yaml is True:
        kind = 'bool'
        image = 'true'
    elif isinstance(yaml, int):
        kind = 'integer'
        image = str(yaml)
    elif isinstance(yaml, str):
        kind = 'string'
        image = yaml
    elif isinstance(yaml, float):
        kind = 'float'
        image = str(yaml)
    elif isinstance(yaml, datetime.datetime):
        kind = 'local-datetime'
        image = '{:>04}-{:>02}-{:>02}T{:>02}:{:>02}:{:>02}'.format(
            yaml.year, yaml.month, yaml.day,
            yaml.hour, yaml.minute, yaml.second
        )
        if yaml.microsecond:
            image = '{}.{:>03}'.format(image, yaml.microsecond)
    else:
        raise ValueError('Unsupported YAML value ({}): {}'
                         .format(type(yaml), yaml))

    return {'type': kind, 'value': image}


def import_valid(toml_file):
    assert toml_file.endswith('.toml')
    test_name = create_test_name(toml_file)
    yaml_file = os.path.join(os.path.dirname(toml_file), test_name + '.yaml')

    # Create the test directory
    test_dir = out_path('values', test_name)
    os.mkdir(test_dir)

    # Copy the TOML to parse
    shutil.copy(toml_file, os.path.join(test_dir, 'input.toml'))

    # Create the test.yaml, including the expected JSON
    try:
        with open(yaml_file, 'r', encoding='utf-8') as f:
            data = yaml.safe_load(f)
    except FileNotFoundError:
        print('warning: could not find YAML for {}'.format(toml_file))
        return
    data = yaml_to_json(data)
    write_test_yaml(test_dir,
                    'driver: decoder'
                    '\noutput: {}'.format(json.dumps(data)))


def import_invalid(toml_file):
    assert toml_file.endswith('.toml')
    test_name = create_test_name(toml_file)

    # Create the test directory
    test_dir = out_path('errors', test_name)
    os.mkdir(test_dir)

    # Copy the TOML to parse
    shutil.copy(toml_file, os.path.join(test_dir, 'input.toml'))

    # Create the test.yaml
    write_test_yaml(test_dir,
                    'driver: decoder'
                    '\nerror: True')


os.mkdir(out_path('values'))
for toml_file in glob.glob(in_path('values', '*.toml')):
    import_valid(toml_file)

os.mkdir(out_path('errors'))
for toml_file in glob.glob(in_path('errors', '*.toml')):
    import_invalid(toml_file)
