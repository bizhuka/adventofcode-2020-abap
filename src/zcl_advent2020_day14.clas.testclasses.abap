*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day14.
    METHODS setup.
    METHODS binary_2_int FOR TESTING.
    METHODS mask_2_int   FOR TESTING.
    METHODS mask_part_01 FOR TESTING.
    METHODS part1        FOR TESTING.
    METHODS part2        FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD binary_2_int.
    cut->binary_2_int( EXPORTING iv_binary = '000000000000000000000000000000001011'
                       IMPORTING ev_dec    = DATA(lv_dec) ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_dec
      exp = 11 ).

    cut->binary_2_int( EXPORTING iv_binary = '000000000000000000000000000001100101'
                       IMPORTING ev_dec    = lv_dec ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_dec
      exp = 101 ).
  ENDMETHOD.

  METHOD mask_2_int.
    cut->mask_2_int( EXPORTING iv_binary = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X'
                     IMPORTING ev_or     = DATA(lv_mask_or)
                               ev_and    = DATA(lv_mask_and) ).

    DATA(lv_int_or)  = CONV int8( lv_mask_or ).
    DATA(lv_int_and) = CONV int8( lv_mask_and ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_int_or
      exp = 64 ).

*    cl_abap_unit_assert=>assert_equals(
*      act = lv_int2
*      exp = 2 ).
  ENDMETHOD.

  METHOD mask_part_01.
    DATA(lv_num) = cut->mask_part_01( iv_mask   = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X'
                                      iv_number = '000000000000000000000000000000001011' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_num
      exp = 73 ).

    lv_num = cut->mask_part_01( iv_mask   = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X'
                                iv_number = '000000000000000000000000000001100101' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_num
      exp = 101 ).

    lv_num = cut->mask_part_01( iv_mask   = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X'
                                iv_number = '000000000000000000000000000000000000' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_num
      exp = 64 ).
  ENDMETHOD.

  METHOD part1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( it_input = VALUE #(
        ( |mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X| )
        ( |mem[8] = 11| )
        ( |mem[7] = 101| )
        ( |mem[8] = 0| )
       ) )
      exp = 165 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2(
        it_input = VALUE #(
          ( |mask = 000000000000000000000000000000X1001X| )
          ( |mem[42] = 100| ) ) )
      exp = 400 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2(
        it_input = VALUE #(
          ( |mask = 00000000000000000000000000000000X0XX| )
          ( |mem[26] = 1| ) )
        iv_init = abap_false )
      exp = 208 ).
  ENDMETHOD.

ENDCLASS.
