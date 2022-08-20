*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day09.
    METHODS setup.
    METHODS part1      FOR TESTING.
    METHODS part2      FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part1(
       iv_preamble = 5
       it_input    = VALUE #(
            ( |35| )
            ( |20| )
            ( |15| )
            ( |25| )
            ( |47| )
            ( |40| )
            ( |62| )
            ( |55| )
            ( |65| )
            ( |95| )
            ( |102| )
            ( |117| )
            ( |150| )
            ( |182| )
            ( |127| )
            ( |219| )
            ( |299| )
            ( |277| )
            ( |309| )
            ( |576| )
      ) )
      exp = 127 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2(
       iv_preamble = 5
       it_input    = VALUE #(
            ( |35| )
            ( |20| )
            ( |15| )
            ( |25| )
            ( |47| )
            ( |40| )
            ( |62| )
            ( |55| )
            ( |65| )
            ( |95| )
            ( |102| )
            ( |117| )
            ( |150| )
            ( |182| )
            ( |127| )
            ( |219| )
            ( |299| )
            ( |277| )
            ( |309| )
            ( |576| )
      ) )
      exp = 62 ).
  ENDMETHOD.

ENDCLASS.
