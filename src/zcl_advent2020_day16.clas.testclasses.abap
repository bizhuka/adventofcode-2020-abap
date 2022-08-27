*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day16.
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
      act = cut->part1( it_input = VALUE stringtab(
        ( |class: 1-3 or 5-7| )
        ( |row: 6-11 or 33-44| )
        ( |seat: 13-40 or 45-50| )
        ( || )
        ( |your ticket:| )
        ( |7,1,14| )
        ( || )
        ( |nearby tickets:| )
        ( |7,3,47| )
        ( |40,4,50| )
        ( |55,2,20| )
        ( |38,6,12| )
      ) )
      exp = 71 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( it_input = VALUE stringtab( ) )
      exp = 0 ).
  ENDMETHOD.

ENDCLASS.
