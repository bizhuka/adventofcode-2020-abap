CLASS zcl_advent2020_day01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES:
      int4_tab TYPE STANDARD TABLE OF int4 WITH DEFAULT KEY.
    CONSTANTS mc_year TYPE i VALUE 2020 ##NO_TEXT.

    METHODS solve_01
      IMPORTING
        it_input         TYPE int4_tab
      RETURNING
        VALUE(rv_result) TYPE i .
    METHODS solve_02
      IMPORTING
        it_input         TYPE int4_tab
      RETURNING
        VALUE(rv_result) TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_advent2020_day01 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input) = NEW lcl_input( )->mt_input.
    out->write( |{ solve_01( lt_input ) } - { solve_02( lt_input ) }| ).
  ENDMETHOD.


  METHOD solve_01.
    DATA lt_prev_number TYPE HASHED TABLE OF int4 WITH UNIQUE KEY table_line.

    LOOP AT it_input INTO DATA(lv_num1) WHERE table_line < mc_year.
      DATA(lv_prev_number) = mc_year - lv_num1.
      ASSIGN lt_prev_number[ table_line = lv_prev_number ] TO FIELD-SYMBOL(<lv_prev_number>).
      IF sy-subrc = 0.
        rv_result = lv_num1 * lv_prev_number.
        RETURN.
      ENDIF.

      INSERT lv_num1 INTO TABLE lt_prev_number.
    ENDLOOP.
  ENDMETHOD.


  METHOD solve_02.
    LOOP AT it_input INTO DATA(lv_num1) WHERE table_line < mc_year.
      LOOP AT it_input INTO DATA(lv_num2) FROM sy-tabix + 1 WHERE table_line < mc_year - lv_num1.
        LOOP AT it_input INTO DATA(lv_num3) FROM sy-tabix + 1 WHERE table_line = mc_year - lv_num1 - lv_num2.
          rv_result = lv_num1 * lv_num2 * lv_num3.
          RETURN.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
