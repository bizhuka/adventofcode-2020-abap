*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION LONG
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day15.
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
      act = cut->part1( iv_input = '0,3,6' )
      exp = 436 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( iv_input = '1,3,2' )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( iv_input = '2,1,3' )
      exp = 10 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( iv_input = '1,2,3' )
      exp = 27 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( iv_input = '2,3,1' )
      exp = 78 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( iv_input = '3,2,1' )
      exp = 438 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( iv_input = '3,1,2' )
      exp = 1836 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( iv_input = '0,3,6' )
      exp = 175594 ).

*    cl_abap_unit_assert=>assert_equals(
*      act = cut->part2( iv_input = '1,3,2' )
*      exp =  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = cut->part2( iv_input = '2,1,3' )
*      exp =  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = cut->part2( iv_input = '1,2,3' )
*      exp =  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = cut->part2( iv_input = '2,3,1' )
*      exp =  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = cut->part2( iv_input = '3,2,1' )
*      exp =  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act = cut->part2( iv_input = '3,1,2' )
*      exp =  ).
  ENDMETHOD.

ENDCLASS.
