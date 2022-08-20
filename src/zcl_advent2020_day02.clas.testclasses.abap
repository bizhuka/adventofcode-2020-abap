*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zcl_advent2020_day02.
    METHODS setup.
    METHODS is_valid_1 FOR TESTING.
    METHODS is_valid_2 FOR TESTING.
    METHODS is_valid_3 FOR TESTING.
    METHODS is_valid_pos_1 FOR TESTING.
    METHODS is_valid_pos_2 FOR TESTING.
    METHODS is_valid_pos_3 FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD is_valid_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_valid( iv_password = 'abcde'
                           iv_rule     = '1-3 a' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD is_valid_2.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_valid( iv_password = 'cdefg'
                           iv_rule     = '1-3 b' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD is_valid_3.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_valid( iv_password = 'ccccccccc'
                           iv_rule     = '2-9 c' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD is_valid_pos_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_valid_pos( iv_password = 'abcde'
                               iv_rule     = '1-3 a' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD is_valid_pos_2.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_valid_pos( iv_password = 'cdefg'
                               iv_rule     = '1-3 b' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD is_valid_pos_3.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_valid_pos( iv_password = 'ccccccccc'
                               iv_rule     = '2-9 c' )
      exp = abap_true ).
  ENDMETHOD.

ENDCLASS.
