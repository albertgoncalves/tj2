#!/usr/bin/env python3

from os import environ
from subprocess import run
from unittest import main, TestCase

__unittest = True

WD = environ["WD"]


class Tests(TestCase):
    def into_test(self, file, code, expected):
        path = f"{WD}/ex/{file}.tj"
        result = run(
            f"{WD}/bin/main {path}".encode(),
            capture_output=True,
            shell=True,
        )
        self.assertEqual(result.returncode, code)
        if expected is None:
            return
        self.assertEqual(result.stdout.decode(), f"{path}:{expected}\n")

    def test_compose(self):
        self.into_test("compose", 0, None)

    def test_error_constant(self):
        self.into_test(
            "error_constant",
            1,
            "6:5: Expected `X` but received `Y`",
        )

    def test_error_higher_order(self):
        self.into_test(
            "error_higher_order",
            1,
            "9:14: Expected `X` but received `Y`",
        )

    def test_error_var(self):
        self.into_test(
            "error_var",
            1,
            "5:5: Expected `{ a: 'a, b: 'c }` but received `{ a: 'a, b: 'b }`",
        )

    def test_higher_order(self):
        self.into_test("higher_order", 0, None)

    def test_id(self):
        self.into_test("id", 0, None)

    def test_missing_ident(self):
        self.into_test("missing_ident", 1, "3:9: Identifier `x` not defined")

    def test_pipe(self):
        self.into_test("pipe", 0, None)

    def test_var(self):
        self.into_test("var", 0, None)


if __name__ == "__main__":
    main()
