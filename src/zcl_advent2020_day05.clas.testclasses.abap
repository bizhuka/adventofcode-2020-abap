*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day05.
    METHODS setup.
    METHODS get_row_1      FOR TESTING.
    METHODS get_column_1   FOR TESTING.
    METHODS get_seat_id_1  FOR TESTING.
    METHODS get_seat_id_2  FOR TESTING.
    METHODS get_seat_id_3  FOR TESTING.
    METHODS get_seat_id_4  FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_row_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->get_row( 'FBFBBFF' )
      exp = 44 ).
  ENDMETHOD.

  METHOD get_column_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->get_column( 'RLR' )
      exp = 5 ).
  ENDMETHOD.

  METHOD get_seat_id_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->get_seat_id( 'FBFBBFFRLR' )
      exp = 357 ).
  ENDMETHOD.

  METHOD get_seat_id_2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->get_seat_id( 'BFFFBBFRRR' )
      exp = 567 ).
  ENDMETHOD.

  METHOD get_seat_id_3.
    cl_abap_unit_assert=>assert_equals(
      act = cut->get_seat_id( 'FFFBBBFRRR' )
      exp = 119 ).
  ENDMETHOD.

  METHOD get_seat_id_4.
    cl_abap_unit_assert=>assert_equals(
      act = cut->get_seat_id( 'BBFFBBFRLL' )
      exp = 820 ).
  ENDMETHOD.

ENDCLASS.
