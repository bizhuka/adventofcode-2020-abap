CLASS zcl_advent2020_day05 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
    TYPES char3 TYPE c LENGTH 3.
    TYPES char7 TYPE c LENGTH 7.
    TYPES char10 TYPE c LENGTH 10.

    METHODS part1 IMPORTING it_input      TYPE string_table
                  RETURNING VALUE(rv_max) TYPE i.

    METHODS part2 IMPORTING it_input             TYPE string_table
                  RETURNING VALUE(rv_my_seat_id) TYPE i.

    METHODS get_seat_id IMPORTING iv_raw            TYPE char10
                        RETURNING VALUE(rv_seat_id) TYPE i.
    METHODS get_row IMPORTING iv_raw        TYPE char7
                    RETURNING VALUE(rv_row) TYPE i.
    METHODS get_column IMPORTING iv_raw           TYPE char3
                       RETURNING VALUE(rv_column) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS base_converter
      IMPORTING
        !number         TYPE any
        !from           TYPE i DEFAULT 10
        !to             TYPE i DEFAULT 62
        !alphabet       TYPE csequence OPTIONAL
      RETURNING
        VALUE(rv_value) TYPE string
      RAISING
        cx_sy_move_cast_error .
ENDCLASS.


CLASS zcl_advent2020_day05 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count_1) = part1( lt_input ).
    DATA(lv_count_2) = part2( lt_input ).

    out->write( |{ lv_count_1 } - { lv_count_2 }| ).
  ENDMETHOD.

  METHOD get_row.
    DATA(lv_binary) = iv_raw.
    REPLACE ALL OCCURRENCES OF: 'F' IN lv_binary WITH '0',
                                'B' IN lv_binary WITH '1'.
    rv_row = base_converter( number = lv_binary from = 2 to = 10 ).
  ENDMETHOD.

  METHOD get_column.
    DATA(lv_binary) = iv_raw.
    REPLACE ALL OCCURRENCES OF: 'L' IN lv_binary WITH '0',
                                'R' IN lv_binary WITH '1'.
    rv_column = base_converter( number = lv_binary from = 2 to = 10 ).
  ENDMETHOD.

  METHOD get_seat_id.
    rv_seat_id = get_row( iv_raw(7) ) * 8 + get_column( iv_raw+7 ).
  ENDMETHOD.

  METHOD part1.
    LOOP AT it_input INTO DATA(lv_input).
      DATA(lv_num) = get_seat_id( CONV #( lv_input ) ).
      CHECK rv_max < lv_num.
      rv_max = lv_num.
    ENDLOOP.
  ENDMETHOD.

  METHOD part2.
    DATA lt_seat_id TYPE STANDARD TABLE OF i.
    lt_seat_id = VALUE #( FOR lv_input IN it_input ( get_seat_id( CONV #( lv_input ) ) ) ).

    SORT lt_seat_id DESCENDING.
    DATA lt_my_seat_id LIKE lt_seat_id.
    LOOP AT lt_seat_id INTO DATA(lv_seat_id).
      ASSIGN lt_seat_id[ sy-tabix + 1 ] TO FIELD-SYMBOL(<lv_next_seat_id>).
      CHECK lv_seat_id - <lv_next_seat_id> = 2.

      rv_my_seat_id = <lv_next_seat_id> + 1.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD base_converter.

    DATA: lv_decimal  TYPE p,
          lv_alphabet TYPE string,
          lv_number   TYPE string,
          lv_length   TYPE i,
          lv_last     TYPE i,
          lv_index    TYPE i.
    CONSTANTS mc_alphabet TYPE string VALUE `0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_.~!*{}'();:@&=+$,/?#[]%<>"§|\^`. "#EC NOTEXT

    IF alphabet IS INITIAL.
      lv_alphabet = mc_alphabet.
    ELSE.
      lv_alphabet = alphabet.
    ENDIF.

    IF from GT strlen( lv_alphabet ) OR to LT 2.
      RAISE EXCEPTION TYPE cx_sy_move_cast_error.
    ENDIF.

    IF from NE 10.

      lv_number = number.
      CONDENSE lv_number.

      lv_length = strlen( lv_number ).
      lv_last = lv_length - 1.
      WHILE lv_last GE 0.
        FIND FIRST OCCURRENCE OF lv_number+lv_last(1) IN lv_alphabet RESPECTING CASE MATCH OFFSET lv_index.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE cx_sy_move_cast_error.
        ENDIF.
        lv_decimal = lv_decimal + lv_index * ( from ** ( lv_length - lv_last - 1 ) ).
        lv_last = lv_last - 1.
      ENDWHILE.

    ELSE.
      lv_decimal = number.
    ENDIF.

    IF to NE 10.

      WHILE lv_decimal NE 0.
        lv_index = lv_decimal MOD to.
        CONCATENATE lv_alphabet+lv_index(1) rv_value INTO rv_value.
        lv_decimal = lv_decimal DIV to.
      ENDWHILE.

      IF rv_value IS INITIAL.
        rv_value = '0'.
      ENDIF.

    ELSE.
      rv_value = lv_decimal.
      CONDENSE rv_value.
    ENDIF.

  ENDMETHOD.                    "base_converter

ENDCLASS.
