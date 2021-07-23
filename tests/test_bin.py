def test_bin_hy_main():
    output, _ = run_cmd("hy tests/resources/bin/main.hy")
    assert "Hello World" in output


def test_bin_hy_main_args():
    output, _ = run_cmd("hy tests/resources/bin/main.hy test 123 -m -B 9")
    assert "<test|123|-m|-B|9>" in output


def test_bin_hy_main_exitvalue():
    run_cmd("hy tests/resources/bin/main.hy exit1", expect=1)


def test_bin_hy_module_main():
    output, _ = run_cmd("hy -m tests.resources.bin.main")
    assert "Hello World" in output


def test_bin_hy_module_main_args():
    output, _ = run_cmd("hy -m tests.resources.bin.main test 123 -B")
    assert "<test|123|-B>" in output


def test_bin_hy_module_main_exitvalue():
    run_cmd("hy -m tests.resources.bin.main exit1", expect=1)
