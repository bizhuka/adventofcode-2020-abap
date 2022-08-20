CLASS zcl_advent2020_day06 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS part1 IMPORTING it_input        TYPE stringtab
                  RETURNING VALUE(rv_count) TYPE i.

    METHODS part2 IMPORTING it_input        TYPE stringtab
                  RETURNING VALUE(rv_count) TYPE i.

    METHODS get_1group_count
      IMPORTING
                iv_string       TYPE string
      RETURNING VALUE(rv_count) TYPE i.

    METHODS get_2group_count
      IMPORTING
                iv_string       TYPE string
      RETURNING VALUE(rv_count) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_advent2020_day06 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count_1) = part1( lt_input ).
    DATA(lv_count_2) = part2( lt_input ).

    out->write( |{ lv_count_1 } - { lv_count_2 }| ).
  ENDMETHOD.

  METHOD part1.
    DATA(lv_all) = concat_lines_of( table = it_input sep = |\n| ).
    SPLIT lv_all AT |\n\n| INTO TABLE DATA(lt_all).
    rv_count = REDUCE #( INIT sum = 0
                         FOR lv_line IN lt_all
                         NEXT sum = sum + get_1group_count( lv_line ) ).
  ENDMETHOD.

  METHOD part2.
    DATA(lv_all) = concat_lines_of( table = it_input sep = |\n| ).
    SPLIT lv_all AT |\n\n| INTO TABLE DATA(lt_all).
    rv_count = REDUCE #( INIT sum = 0
                         FOR lv_line IN lt_all
                         NEXT sum = sum + get_2group_count( lv_line ) ).
  ENDMETHOD.

  METHOD get_1group_count.
    DATA(lv_string) = iv_string.
    REPLACE ALL OCCURRENCES OF |\n| IN lv_string WITH ||.

    DATA lt_char TYPE SORTED TABLE OF char1 WITH UNIQUE KEY table_line.
    DO strlen( lv_string ) TIMES.
      DATA(lv_index) = sy-index - 1.
      INSERT CONV #( lv_string+lv_index(1) ) INTO TABLE lt_char.
    ENDDO.

    rv_count = lines( lt_char ).
  ENDMETHOD.

  METHOD get_2group_count.
    SPLIT iv_string AT |\n| INTO TABLE DATA(lt_answers).

    TYPES: BEGIN OF ts_col_answer,
             char  TYPE char1,
             count TYPE i,
           END OF ts_col_answer.
    DATA lt_col_answer TYPE SORTED TABLE OF ts_col_answer WITH UNIQUE KEY char.

    LOOP AT lt_answers INTO DATA(lv_answer).
      DO strlen( lv_answer ) TIMES.
        DATA(lv_index) = sy-index - 1.
        COLLECT VALUE ts_col_answer( char  = lv_answer+lv_index(1)
                                     count = 1 ) INTO lt_col_answer.
      ENDDO.
    ENDLOOP.

    LOOP AT lt_col_answer TRANSPORTING NO FIELDS WHERE count = lines( lt_answers ).
      rv_count = rv_count + 1.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
