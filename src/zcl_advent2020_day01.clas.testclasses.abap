*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_advent2020_day01.
    METHODS setup.
    METHODS solve_01 FOR TESTING.
    METHODS solve_02 FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD solve_01.
    cl_abap_unit_assert=>assert_equals(
      act = cut->solve_01( VALUE #( ( 1721 ) ( 979 ) ( 366 ) ( 299 ) ( 675 ) ( 1456 ) ) )
      exp = 514579 ).
  ENDMETHOD.

  METHOD solve_02.
    cl_abap_unit_assert=>assert_equals(
      act = cut->solve_02( VALUE #( ( 1721 ) ( 979 ) ( 366 ) ( 299 ) ( 675 ) ( 1456 ) ) )
      exp = 241861950 ).
  ENDMETHOD.

ENDCLASS.
