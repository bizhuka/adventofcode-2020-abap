*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day13.
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
      ( |939| )
      ( |7,13,x,x,59,x,31,19| )
       ) )
      exp = 295 ).
  ENDMETHOD.

  METHOD part2.

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( iv_input = |3,7,11,5| )
      exp = 867 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( iv_input = |17,x,13,19| )
      exp = 3417 ).
  ENDMETHOD.

ENDCLASS.
