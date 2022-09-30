*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day25.
    METHODS setup.
    METHODS part1    FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->get_loop_size( 5764801 )
      exp = 8 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_loop_size( 17807724 )
      exp = 11 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_encryption_key( iv_times = 8  iv_number = 17807724 )
      exp = cut->get_encryption_key( iv_times = 11 iv_number = 5764801  ) ).
  ENDMETHOD.

ENDCLASS.
