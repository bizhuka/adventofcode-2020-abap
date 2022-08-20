*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day08.
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
      act = cut->part1( VALUE #(
        ( |nop +0| )
        ( |acc +1| )
        ( |jmp +4| )
        ( |acc +3| )
        ( |jmp -3| )
        ( |acc -99| )
        ( |acc +1| )
        ( |jmp -4| )
        ( |acc +6| )
      ) )
      exp = 5 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #(
        ( |nop +0| )
        ( |acc +1| )
        ( |jmp +4| )
        ( |acc +3| )
        ( |jmp -3| )
        ( |acc -99| )
        ( |acc +1| )
        ( |jmp -4| )
        ( |acc +6| )
      ) )
      exp = 8 ).
  ENDMETHOD.

ENDCLASS.
