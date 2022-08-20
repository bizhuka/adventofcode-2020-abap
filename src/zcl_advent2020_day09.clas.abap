CLASS zcl_advent2020_day09 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES inttab      TYPE STANDARD TABLE OF int8 WITH DEFAULT KEY .
    TYPES inttab_sort TYPE SORTED TABLE OF int8 WITH UNIQUE KEY table_line .

    METHODS part1
      IMPORTING
        it_input        TYPE inttab
        iv_preamble     TYPE i
      RETURNING
        VALUE(rv_count) TYPE int8 .
    METHODS part2
      IMPORTING
        it_input        TYPE inttab
        iv_preamble     TYPE i
      RETURNING
        VALUE(rv_count) TYPE int8 .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_advent2020_day09 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_next_1) = part1( iv_preamble = 25
                             it_input    = lt_input ).
    DATA(lv_next_2) = part2( iv_preamble = 25
                             it_input    = lt_input ).

    CHECK out IS NOT INITIAL.
    out->write( |{ lv_next_1 } - { lv_next_2 }| ).
  ENDMETHOD.

  METHOD part1.
    LOOP AT it_input INTO DATA(lv_check) FROM iv_preamble + 1.
      DATA(lv_from1) = sy-tabix - iv_preamble.
      DATA(lv_found) = abap_false.

      LOOP AT it_input INTO DATA(lv_num1) FROM lv_from1 WHERE table_line < lv_check.
        DATA(lv_second_num) = lv_check - lv_num1.

        LOOP AT it_input INTO DATA(lv_num2) FROM lv_from1 + 1 WHERE table_line = lv_second_num.
          lv_found = abap_true.
          EXIT.
        ENDLOOP.

        IF lv_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_found <> abap_true.
        rv_count = lv_check.
        RETURN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD part2.
    DATA(lv_ok_number) = part1( iv_preamble = iv_preamble
                                it_input    = it_input ).
    CHECK lv_ok_number IS NOT INITIAL.

    LOOP AT it_input INTO DATA(lv_num).
      DATA(lt_range) = VALUE inttab_sort( ( lv_num ) ).

      LOOP AT it_input INTO DATA(lv_num_next) FROM sy-tabix + 1.
        lv_num = lv_num + lv_num_next.
        INSERT lv_num_next INTO TABLE lt_range[].

        IF lv_num = lv_ok_number.
          rv_count = lt_range[ 1 ] + lt_range[ lines( lt_range ) ].
          RETURN.
        ENDIF.

        IF lv_num > lv_ok_number.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
