*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION LONG
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day22.
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
      act = cut->part1( VALUE #(
          ( |Player 1:| )
          ( |9| )
          ( |2| )
          ( |6| )
          ( |3| )
          ( |1| )
          ( || )
          ( |Player 2:| )
          ( |5| )
          ( |8| )
          ( |4| )
          ( |7| )
          ( |10| )
          ) )
      exp = 306 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #(
          ( |Player 1:| )
          ( |9| )
          ( |2| )
          ( |6| )
          ( |3| )
          ( |1| )
          ( || )
          ( |Player 2:| )
          ( |5| )
          ( |8| )
          ( |4| )
          ( |7| )
          ( |10| )
          ) )
      exp = 291 ).
  ENDMETHOD.

ENDCLASS.
