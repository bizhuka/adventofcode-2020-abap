*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day18.
    METHODS setup.
    METHODS part1    FOR TESTING.
    METHODS part2    FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( VALUE #( ( |1 + 2 * 3 + 4 * 5 + 6| ) ) )
      exp = 71 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( VALUE #( ( |2 * 3 + (4 * 5)| ) ) )
      exp = 26 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( VALUE #( ( |5 + (8 * 3 + 9 + 3 * 4 * 3)| ) ) )
      exp = 437 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( VALUE #( ( |5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))| ) ) )
      exp = 12240 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( VALUE #( ( |((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2| ) ) )
      exp = 13632 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #( ( |1 + 2 * 3 + 4 * 5 + 6| ) ) )
      exp = 231 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #( ( |1 + (2 * 3) + (4 * (5 + 6))| ) ) )
      exp = 51 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #( ( |2 * 3 + (4 * 5)| ) ) )
      exp = 46 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #( ( |5 + (8 * 3 + 9 + 3 * 4 * 3)| ) ) )
      exp = 1445 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #( ( |5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))| ) ) )
      exp = 669060 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #( ( |((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2| ) ) )
      exp = 23340 ).
  ENDMETHOD.

ENDCLASS.
