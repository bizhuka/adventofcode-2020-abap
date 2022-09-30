class ZCL_ADVENT2020_DAY25 definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  methods PART1
    importing
      !IT_INPUT type STRING_TABLE
    returning
      value(RV_COUNT) type DECFLOAT34 .
  methods GET_LOOP_SIZE
    importing
      !IV_VALUE type DECFLOAT34
    returning
      value(RV_RESULT) type DECFLOAT34 .
  methods GET_ENCRYPTION_KEY
    importing
      !IV_TIMES type DECFLOAT34
      !IV_NUMBER type DECFLOAT34
    returning
      value(RV_RESULT) type DECFLOAT34 .
  PROTECTED SECTION.

private section.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY25 IMPLEMENTATION.


  METHOD get_encryption_key.
    rv_result = 1.
    DO iv_times TIMES.
      rv_result *= iv_number.
      rv_result = rv_result MOD 20201227.
    ENDDO.
  ENDMETHOD.


  METHOD get_loop_size.
    DATA(lv_num) = CONV decfloat34( 1 ).
    DO.
      lv_num = ( lv_num * 7 ) MOD 20201227.
      IF lv_num = iv_value.
        rv_result = sy-index.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 }| ).
  ENDMETHOD.


  METHOD part1.
    DATA(lv_public_key) = CONV decfloat34( it_input[ 1 ] ).
    DATA(lv_door_key)   = CONV decfloat34( it_input[ 2 ] ).

    DATA(lv_public_loop_size) = get_loop_size( lv_public_key ).
    DATA(lv_door_loop_size)   = get_loop_size( lv_door_key   ).

    DATA(lv_result1) = get_encryption_key( iv_times  = lv_public_loop_size
                                           iv_number = lv_door_key ).

    DATA(lv_result2) = get_encryption_key( iv_times  = lv_door_loop_size
                                           iv_number = lv_public_key ).

    CHECK lv_result1 = lv_result2.
    rv_count = lv_result1.
  ENDMETHOD.
ENDCLASS.
