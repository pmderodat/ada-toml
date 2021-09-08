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
import subprocess
import sys

from e3.fs import sync_tree
from e3.os.process import Run
from e3.testsuite import Testsuite
from e3.testsuite.driver.classic import (
    ClassicTestDriver, TestAbortWithError, TestAbortWithFailure)
from e3.testsuite.result import Log, binary_repr


class TestDriver(ClassicTestDriver):
    """
    Common code for test drivers.
    """

    def fail_if_diff(self, error_label, expected, actual, expected_file,
                     actual_file):
        """
        Shortcut to stop the test with a failure if "expected" and "actual" are
        different.

        :param str error_label: Label to include in the log in case of
            non-empty diff.
        :param list[str] expected: List of expected lines.
        :param list[str] actual: List of actual lines.
        :param str expected_file: Name of the file for expected lines. This is
            for display purposes only: the file name does not need to exist.
        :param str actual_file: Like "expected_file", but for actual lines.
        """
        diff = list(difflib.unified_diff(
            a=expected, b=actual, fromfile=expected_file, tofile=actual_file,
            lineterm=''))

        if diff:
            self.result.expected = Log('\n'.join(expected))
            self.result.out = Log('\n'.join(actual))
            self.result.diff = Log('\n'.join(diff))
            self.result.log += 'Diff:\n{}\n'.format(self.result.diff.log)
            raise TestAbortWithFailure(error_label)



class DecoderTestDriver(TestDriver):
    """
    Test driver to run the "ada_toml_decode" program.

    This test driver runs the "ada_toml_decode" program on the test-provided
    "input.toml" input file and checks that its output is equivalent to the
    "output" entry in the test.yaml file.

    If the test.yaml file contains an "error" entry, expect that
    "ada_toml_decode" exits with a non-zero status code. If "error" is not True,
    it must be a string: this checks that t it emits the provided error
    message.
    """

    decoder_program = 'obj-checkers/ada_toml_decode'
    encoder_program = 'obj-checkers/ada_toml_encode'
    input_file = 'input.toml'

    def _run_subprocess(self, args, input_file):
        self.result.log += "Running: {}\n".format(" ".join(args))
        result = subprocess.run(
            args,
            stdin=input_file, stdout=subprocess.PIPE, stderr=subprocess.STDOUT
        )
        self.result.log += "Status code: {}\n".format(result.returncode)
        try:
            result.decoded_stdout = result.stdout.decode('utf-8')
        except UnicodeDecodeError as exc:
            self.result.log += "Output:\n{}\n".format(
                binary_repr(result.stdout))
            raise TestAbortWithFailure(
                "Cannot decode UTF-8 output: {}".format(exc))
        self.result.log += "Output:\n{}\n".format(result.decoded_stdout)
        return result

    def _run_decoder(self, filename):
        with open(filename, 'rb') as f:
            return self._run_subprocess(
                [os.path.join(self.env.root_dir, self.decoder_program)], f,
            )

    def _run_encoder(self, filename):
        with open(filename, 'rb') as f:
            return self._run_subprocess(
                [os.path.join(self.env.root_dir, self.encoder_program)], f,
            )

    def fail_if_json_mismatch(
        self, error_label, expected, actual, expected_file, actual_file
    ):
        """
        Compare two JSON dumps and stop the test with a failure if "a" and "b"
        don't match.

        See "fail_if_diff" for arguments semantic. The only difference is that
        expected and actual are JSON objects.
        """
        def canonicalize(value):
            if isinstance(value, list):
                return {'type': 'array', 'value': value}
            elif isinstance(value, dict) and set(value) != {'type', 'value'}:
                return {'type': 'dict', 'value': value}
            else:
                return value

        def is_valid(value):
            if (
                not isinstance(value, dict)
                or set(value) != {'type', 'value'}
            ):
                return False
            t, v = value['type'], value['value']
            return (
                (t == 'dict' and isinstance(v, dict))
                or (t == 'array' and isinstance(v, list))
                or (t in ('string', 'float', 'integer', 'bool', 'datetime',
                          'datetime-local', 'date-local', 'time-local')
                    and isinstance(v, str))
            )

        def helper(path, expected, actual):
            path_label = 'root' if path is None else path
            path_prefix = path or ''
            expected = canonicalize(expected)
            actual = canonicalize(actual)
            if not is_valid(expected):
                raise TestAbortWithError(
                    "expected:{}: invalid object description"
                    .format(path_label)
                )
            if not is_valid(actual):
                raise TestAbortWithFailure(
                    "actual:{}: invalid object description".format(path_label)
                )

            def error(label):
                self.fail_if_diff(
                    '{}: {}'.format(path_label, label),
                    pprint.pformat(expected).splitlines(),
                    pprint.pformat(actual).splitlines(),
                    expected_file,
                    actual_file
                )

            t = expected['type']
            exp_v = expected['value']
            act_v = actual['value']
            if t != actual['type']:
                error('type mismatch')
            elif t == 'dict':
                # First check keys equivalence, then check the inner values
                if set(exp_v) != set(act_v):
                    error('unexpected dict keys')
                for k in sorted(exp_v):
                    helper('{}[{}]'.format(path_label, repr(k)),
                           exp_v[k], act_v[k])
            elif t == 'array':
                # First check array length, then check the inner values
                if len(exp_v) != len(act_v):
                    error('unexpected array length')
                for i, (e, a) in enumerate(zip(exp_v, act_v)):
                    helper('{}[{}]'.format(path_label, i), e, a)
            elif t == 'float':
                # First, check "special values"
                if (
                    # The standard isn't very clear about the semantics of the
                    # various NaN's, so accept when "nan" is expected whereas
                    # while ada_toml writes "-nan" (it happens in the
                    # BurntSushi testsuite).
                    (exp_v == 'nan'
                     and act_v not in ('nan', '+nan', '-nan'))
                    or (exp_v == '+nan'
                        and act_v not in ('nan', '+nan'))
                    or (exp_v == '-nan' and act_v != '-nan')
                    or (exp_v in ('inf', '+inf')
                        and act_v not in ('inf', '+inf'))
                    or (exp_v == '-inf' and act_v != '-inf')
                ):
                    error('value mismatch')

                # We cannot expect exact string representation matches for
                # floats. Just try to make sure both designated values are
                # "almost equal": check the precision for 10 decimals
                # (arbitrary, but deemed good enough for now).
                e = float(exp_v)
                a = float(act_v)
                abs_e = abs(e)
                abs_a = abs(a)
                epsilon = max(abs_a, abs_e) / 10**10
                if a < e - epsilon or a > e + epsilon:
                    error('value mismatch')
            elif t in ("datetime", "datetime-local", "time-local"):
                # Mandatory precision for the time part is millisecond:
                # truncate it so that we don't check further.

                def truncate_sub_second(value):
                    if "." in value:
                        prefix, sub_second = value.split(".")
                        return f"{prefix}.{sub_second[:3]}"
                    return value

                exp_v = truncate_sub_second(exp_v)
                act_v = truncate_sub_second(act_v)
                if exp_v != act_v:
                    error("value mismatch")
            elif exp_v != act_v:
                error('value mismatch')

        helper(None, expected, actual)

    def run(self):
        # Get the expected JSON document, or the expected error message
        expected_error = None
        try:
            expected_json = self.test_env['output']
        except KeyError:
            expected_json = None
            expected_error = self.test_env['error']

        # Run the decoder with the TOML content on the standard input
        p = self._run_decoder(self.working_dir(self.input_file))

        # If we expected an error, make sure we have the expected one
        if expected_error:
            out = p.stdout.decode('utf-8')
            if p.returncode == 0:
                raise TestAbortWithFailure(
                    'Error expected, but parsing succeeded')
            elif (
                expected_error is not True
                and out.strip() != expected_error
            ):
                raise TestAbortWithFailure('Unexpected error')
            else:
                return

        # Otherwise, make sure the decoder succeeded and produced the expected
        # result.
        if p.returncode != 0:
            raise TestAbortWithFailure(
                'Decoder exitted with error status ({})'.format(p.returncode))

        json_text_output = p.decoded_stdout
        try:
            p_output_json = json.loads(json_text_output)
        except (UnicodeDecodeError, ValueError) as exc:
            raise TestAbortWithFailure('Cannot parse the output JSON document')

        self.fail_if_json_mismatch(
            'Unexpected JSON output for the decoder',
            expected_json,
            p_output_json,
            'expected output',
            'decode output',
        )

        # Now, try to reformat a TOML document from the JSON output and make
        # sure it produces something that the decoder can re-parse the same
        # way.

        # Put the JSON input in a file (to ease post-mortem analysis)
        input_json_file = self.working_dir('input.json')
        with open(input_json_file, 'wb') as f:
            f.write(p.stdout)

        # Run the encoder on it
        p = self._run_encoder(input_json_file)
        if p.returncode != 0:
            raise TestAbortWithFailure('Encoder failed: {}'.format(p.stdout))

        # Put the resulting TOML document in a file
        input_toml_file = self.working_dir('second-input.toml')
        with open(input_toml_file, 'wb') as f:
            f.write(p.stdout)

        # Run the decoder on it
        p = self._run_decoder(input_toml_file)
        if p.returncode != 0:
            raise TestAbortWithFailure(
                'Second decoder exitted with error status ({})'
                .format(p.returncode)
            )

        try:
            new_json_text_output = p.decoded_stdout
        except UnicodeDecodeError as exc:
            raise TestAbortWithFailure(
                'Cannot decode the output JSON document: {}'.format(exc))
        if json_text_output != new_json_text_output:
            self.fail_if_diff('Unexpected second decoder output',
                              p.out.splitlines(),
                              json_text_output.splitlines(),
                              'second decoder output',
                              'first decoder output')


class RunProgramTestDriver(TestDriver):
    """
    Test driver to build and run a program that uses the Ada_TOML project.

    This test driver builds the test-provided "main.adb" program and then run
    it, checking that its status code is 0 and that its output matches the
    content of the test-provided "test.out" file.
    """

    main_file = 'main.adb'
    expected_output_file = 'test.out'

    def run(self):
        # Create a project file to build the test program and build it
        project_file = self.working_dir('p.gpr')
        with open(project_file, 'w') as f:
            f.write("""
                with "ada_toml";

                project P is
                    for Main use ("main.adb");
                    for Object_Dir use "obj";

                    package Compiler is
                        for Default_Switches ("Ada") use ("-g");
                    end Compiler;
                end P;
            """)
        self.shell(['gprbuild', '-P', project_file, '-p'])

        # Run the test program
        p = self.shell(['obj/main'])

        # Check that its output matches expectations
        with open(self.test_dir(self.expected_output_file), 'r') as f:
            expected_output = f.read().splitlines()

        p_output = p.out.splitlines()

        self.fail_if_diff(
            'Unexpected test program output',
            p.out.splitlines(),
            expected_output,
            '{} output'.format(self.main_file),
            self.expected_output_file)


class ADATomlTestsuite(Testsuite):
    tests_subdir = 'tests'
    test_driver_map = {'decoder': DecoderTestDriver,
                       'run-program': RunProgramTestDriver}

    @property
    def default_driver(self):
        return 'decoder'

    def add_options(self, parser):
        parser.add_argument(
            '--no-auto-path',
            action='store_true',
            help='Do not automatically add this repository to'
                 ' GPR_PROJECT_PATH. Adding it is the default for developer'
                 ' convenience.')

    def set_up(self):
        if not self.main.args.no_auto_path:
            old_value = os.environ.get('GPR_PROJECT_PATH', '')
            if old_value:
                new_value = '{}{}{}'.format(self.root_dir, os.path.pathsep,
                                            old_value)
            else:
                new_value = self.root_dir
            os.environ['GPR_PROJECT_PATH'] = new_value


if __name__ == '__main__':
    # Some test YAML documents create big recursions in the YAML parser
    sys.setrecursionlimit(10000)

    suite = ADATomlTestsuite()
    sys.exit(suite.testsuite_main())
