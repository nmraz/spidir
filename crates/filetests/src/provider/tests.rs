use expect_test::{expect, Expect};

use super::*;

fn check_lines(input: &str, expected: Expect) {
    let lines = get_non_directive_lines(input).join("\n");
    expected.assert_eq(&lines);
}

#[test]
fn lines_empty() {
    check_lines(
        "
            # check: a


        ",
        expect![[""]],
    );
}

#[test]
fn lines_simple() {
    check_lines(
        r"
# run: verify

# regex: val=%\d+

func @test(ptr, i32) {
    # check: function `test`:
    # nextln: `$val:ctrl, $val:i32, $val:ptr = entry`: bad value kind for output 1, expected one of `ptr`, got `i32`
    %0:ctrl, %2:i32, %1:ptr = entry

    # fsjlkdf
    return %0

}
            ",
        expect![[r#"

                            # run: verify
                            func @test(ptr, i32) {
                                %0:ctrl, %2:i32, %1:ptr = entry

                                # fsjlkdf
                                return %0

                            }"#]],
    );
}

#[test]
fn lines_interleaved_directives() {
    check_lines(
        r"
# run: verify

# regex: val=%\d+

func @test(ptr, i32) {
    # check: function `test`:
    # a
    # nextln: `$val:ctrl, $val:i32, $val:ptr = entry`: bad value kind for output 1, expected one of `ptr`, got `i32`
    # b
    %0:ctrl, %2:i32, %1:ptr = entry

    # fsjlkdf
    return %0
}
            ",
        expect![[r#"

                            # run: verify
                            func @test(ptr, i32) {
                                # a
                                # b
                                %0:ctrl, %2:i32, %1:ptr = entry

                                # fsjlkdf
                                return %0
                            }"#]],
    );
}

#[test]
fn lines_interleaved_whitespace() {
    check_lines(
        r"
# run: verify

# regex: val=%\d+

func @test(ptr, i32) {
    # check: function `test`:


    # nextln: `$val:ctrl, $val:i32, $val:ptr = entry`: bad value kind for output 1, expected one of `ptr`, got `i32`


    # nextln: sdjlkf
    %0:ctrl, %2:i32, %1:ptr = entry

    # fsjlkdf
    return %0

}
        ",
        expect![[r#"

                            # run: verify
                            func @test(ptr, i32) {
                                %0:ctrl, %2:i32, %1:ptr = entry

                                # fsjlkdf
                                return %0

                            }"#]],
    )
}

#[test]
fn lines_leading_trailing_whitespace() {
    check_lines(
        r"
# run: verify

# regex: val=%\d+

func @test(ptr, i32) {


    # nextln: `$val:ctrl, $val:i32, $val:ptr = entry`: bad value kind for output 1, expected one of `ptr`, got `i32`


    %0:ctrl, %2:i32, %1:ptr = entry

    # fsjlkdf
    return %0

}
        ",
        expect![[r#"

                            # run: verify
                            func @test(ptr, i32) {
                                %0:ctrl, %2:i32, %1:ptr = entry

                                # fsjlkdf
                                return %0

                            }"#]],
    )
}
