class ZCL_ADVENT2020_DAY00 definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  methods PART1
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_COUNT) type DECFLOAT34 .
  methods PART2
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_COUNT) type DECFLOAT34 .
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY00 IMPLEMENTATION.


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
