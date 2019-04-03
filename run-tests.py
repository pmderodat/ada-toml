#! /usr/bin/env python

"""
e3.testsuite-based testsuite for Ada_TOML.

Just execute this script as a main to run the testsuite.
"""

from __future__ import absolute_import, print_function

import difflib
import json
import os.path
import pprint

import e3.testsuite
from e3.testsuite.driver import TestDriver
from e3.testsuite.result import TestStatus
from e3.os.process import Run


TESTSUITE_ROOT = os.path.dirname(os.path.abspath(__file__))


def canonicalize_json(value):
    """
    Decode all bytes in the ``value`` JSON value.

    This clones the input JSON value, replacing all bytes with the
    corresponding strings, assuming UTF-8 encoding.
    """
    if isinstance(value, (int, bool)):
        return value
    elif isinstance(value, str):
        return value.decode('utf-8')
    elif isinstance(value, list):
        return [canonicalize_json(item) for item in value]
    elif isinstance(value, dict):
        return {canonicalize_json(key): canonicalize_json(item)
                for key, item in value.iteritems()}
    else:
        raise ValueError('Invalid JSON value: {}'.format(value))


class DecoderTestDriver(TestDriver):
    """
    Test driver to run the "ada_toml_decode" program.

    This test driver runs the "ada_toml_decode" program on the test-provided
    "input.toml" input file and checks that its output is equivalent to the
    "output" entry in the test.yaml file.

    If the test.yaml file contains a "expected_error" entry, check instead that
    the program exits with a non-zero status code and that it emits the
    provided error message.
    """

    decoder_program = 'obj-checkers/ada_toml_decode'
    input_file = 'input.toml'

    def add_test(self, dag):
        self.add_fragment(dag, 'run')

    def push_status(self, log, status=TestStatus.FAIL):
        """
        Log content to the result, set test status and push the result.

        :param None|str log: Optional content to log.
        """
        if log:
            self.result.log += log
        self.result.set_status(status)
        self.push_result()

    def push_success(self):
        """
        Shortcut to set test status to PASS and to push the result.
        """
        self.push_status(None, TestStatus.PASS)

    def run(self, previous_values):
        test_dir = self.test_env['test_dir']

        # Get the expected JSON document, or the expected error message
        expected_error = None
        try:
            expected_json = self.test_env['output']
        except KeyError:
            expected_json = None
            expected_error = self.test_env['error']
        else:
            # The object decoded from test.yaml contains sometimes strings,
            # sometimes unicode objects. Canonicalize to Unicode to have the
            # same objects as from the JSON output.
            expected_json = canonicalize_json(expected_json)

        # Read the input TOML content
        with open(os.path.join(test_dir, self.input_file), 'rb') as f:
            input_str = f.read()

        # Run the decoder with the TOML content on the standard input
        with open(os.path.join(test_dir, self.input_file), 'rb') as f:
            p = Run([os.path.join(TESTSUITE_ROOT, self.decoder_program)],
                    input=f)

        # If we expected an error, make sure we have the expected one
        if expected_error:
            if p.status == 0:
                return self.push_status(
                    'Error expected, but parsing succeeded')
            elif p.out.strip() != expected_error:
                return self.push_status('Unexpected error:\n{}'.format(p.out))
            else:
                return self.push_success()

        # Otherwise, make sure the decoder succeeded and produced the expected
        # result.
        if p.status != 0:
            return self.push_status(
                'Decoder exitted with error status ({}):'
                '\n{}'.format(p.status, p.out))

        try:
            p_output_json = json.loads(p.out)
        except ValueError as exc:
            return self.push_status(
                'Cannot parse the output JSON document:'
                '\n{}'
                '\nOutput was: {}'.format(exc, p.out))

        if p_output_json != expected_json:
            p_output_pretty = pprint.pformat(p_output_json).splitlines()
            expected_pretty = pprint.pformat(expected_json).splitlines()

            diff = difflib.unified_diff(
                a=p_output_pretty, b=expected_pretty,
                fromfile='decoder output',
                tofile='expected output',
                lineterm='')

            return self.push_status(
                'Unexpected JSON output for the decoder:'
                '\n{}'.format('\n'.join(diff)))

        return self.push_success()


class Testsuite(e3.testsuite.Testsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {'decoder': DecoderTestDriver}

    @property
    def default_driver(self):
        return 'decoder'


if __name__ == '__main__':
    suite = Testsuite(os.path.dirname(__file__))
    suite.testsuite_main()

    # Display statistics about test results: number of tests per status
    stats = [(str(name).split('.')[1], count)
             for name, count in suite.test_status_counters.items()
             if count]
    for name, count in sorted(stats):
        print('{: <8} {}'.format(name + ':', count))
