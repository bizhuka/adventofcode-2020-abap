CLASS zcl_advent2020_day00 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS part1
      IMPORTING
        it_input        TYPE string_table
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
    METHODS part2
      IMPORTING
        it_input        TYPE string_table
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_advent2020_day00 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( lt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
  ENDMETHOD.


  METHOD part2.
  ENDMETHOD.
ENDCLASS.
