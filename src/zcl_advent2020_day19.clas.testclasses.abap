*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION LONG
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day19.
    METHODS setup.
    METHODS part1    FOR TESTING.
    METHODS part2    FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1.
    DATA(lv_count) = cut->part1( VALUE #(
      ( `0: 4 1 5` )
      ( `1: 2 3 | 3 2` )
      ( `2: 4 4 | 5 5` )
      ( `3: 4 5 | 5 4` )
      ( `4: "a"` )
      ( `5: "b"` )
      ( `` )
      ( `ababbb` )
      ( `bababa` )
      ( `abbbab` )
      ( `aaabbb` )
      ( `aaaabbb` )
     ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_count
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_rule_01( `4` )
      exp = VALUE stringtab( ( `a` ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_rule_01( `5` )
      exp = VALUE stringtab( ( `b` ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_rule_01( `2` )
      exp = VALUE stringtab( ( `aa` )
                             ( `bb` ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_rule_01( `3` )
      exp = VALUE stringtab( ( `ab` )
                             ( `ba` ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_rule_01( `1` )
      exp = VALUE stringtab(
        ( `aaab` )
        ( `aaba` )
        ( `bbab` )
        ( `bbba` )
        ( `abaa` )
        ( `abbb` )
        ( `baaa` )
        ( `babb` ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_rule_01( `0` )
      exp = VALUE stringtab(
        ( `aaaabb` )
        ( `aaabab` )
        ( `abbabb` )
        ( `abbbab` )
        ( `aabaab` )
        ( `aabbbb` )
        ( `abaaab` )
        ( `ababbb` ) ) ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( it_input = VALUE #(
                          ( `0: 4 1 5` )
                          ( `1: 2 3 | 3 2` )
                          ( `2: 4 4 | 5 5` )
                          ( `3: 4 5 | 5 4` )
                          ( `4: "a"` )
                          ( `5: "b"` )
                          ( `` )
                          ( `ababbb` )
                          ( `bababa` )
                          ( `abbbab` )
                          ( `aaabbb` )
                          ( `aaaabbb` ) )
                       iv_replace = abap_false )
      exp = 2 ).

    DATA(lt_input) = VALUE stringtab(
          ( `42: 9 14 | 10 1` )
          ( `9: 14 27 | 1 26` )
          ( `10: 23 14 | 28 1` )
          ( `1: "a"` )
          ( `11: 42 31` )
          ( `5: 1 14 | 15 1` )
          ( `19: 14 1 | 14 14` )
          ( `12: 24 14 | 19 1` )
          ( `16: 15 1 | 14 14` )
          ( `31: 14 17 | 1 13` )
          ( `6: 14 14 | 1 14` )
          ( `2: 1 24 | 14 4` )
          ( `0: 8 11` )
          ( `13: 14 3 | 1 12` )
          ( `15: 1 | 14` )
          ( `17: 14 2 | 1 7` )
          ( `23: 25 1 | 22 14` )
          ( `28: 16 1` )
          ( `4: 1 1` )
          ( `20: 14 14 | 1 15` )
          ( `3: 5 14 | 16 1` )
          ( `27: 1 6 | 14 18` )
          ( `14: "b"` )
          ( `21: 14 1 | 1 14` )
          ( `25: 1 1 | 1 14` )
          ( `22: 14 14` )
          ( `8: 42` )
          ( `26: 14 22 | 1 20` )
          ( `18: 15 15` )
          ( `7: 14 5 | 1 21` )
          ( `24: 14 1` )
          ( `` )
          ( `abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa` )
          ( `bbabbbbaabaabba` )
          ( `babbbbaabbbbbabbbbbbaabaaabaaa` )
          ( `aaabbbbbbaaaabaababaabababbabaaabbababababaaa` )
          ( `bbbbbbbaaaabbbbaaabbabaaa` )
          ( `bbbababbbbaaaaaaaabbababaaababaabab` )
          ( `ababaaaaaabaaab` )
          ( `ababaaaaabbbaba` )
          ( `baabbaaaabbaaaababbaababb` )
          ( `abbbbabbbbaaaababbbbbbaaaababb` )
          ( `aaaaabbaabaaaaababaa` )
          ( `aaaabbaaaabbaaa` )
          ( `aaaabbaabbaaaaaaabbbabbbaaabbaabaaa` )
          ( `babaaabbbaaabaababbaabababaaab` )
          ( `aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba` ) ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( it_input   = lt_input
                        iv_replace = abap_false )
      exp = 3 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( it_input   = lt_input
                        iv_replace = abap_true )
      exp = 12 ).
  ENDMETHOD.

ENDCLASS.
