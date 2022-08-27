class ZCL_ADVENT2020_DAY13 definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  methods PART1
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_COUNT) type I .
  methods PART2
    importing
      !IV_INPUT type STRING
    returning
      value(RV_COUNT) type DECFLOAT34 .
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY13 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( it_input = lt_input ).
    DATA(lv_count2) = part2( iv_input = lt_input[ 2 ] ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    DATA(lv_minute_arrival) = CONV i( it_input[ 1 ] ).
    SPLIT it_input[ 2 ] AT ',' INTO TABLE DATA(lt_bus).

    DATA(lv_bus_number)    = 0.
    DATA(lv_nearst_time)   = 2147483647.
    LOOP AT lt_bus INTO DATA(lv_bus) WHERE table_line <> 'x'.
      IF lv_minute_arrival MOD lv_bus = 0.
        lv_bus_number  = lv_bus.
        lv_nearst_time = lv_minute_arrival.
        EXIT.
      ENDIF.

      DATA(lv_nearst) = lv_minute_arrival DIV lv_bus * lv_bus + lv_bus.
      IF lv_nearst_time > lv_nearst.
        lv_nearst_time = lv_nearst.
        lv_bus_number  = lv_bus.
      ENDIF.
    ENDLOOP.

    CHECK lv_bus_number IS NOT INITIAL.
    rv_count = ( lv_nearst_time - lv_minute_arrival ) * lv_bus_number.
  ENDMETHOD.


  METHOD part2.
    SPLIT iv_input AT ',' INTO TABLE DATA(lt_bus_txt).

    " https://github.com/alvin-the-programmer/advent-of-code-2020/blob/main/d13/part2.js
    TYPES int_tab LIKE STANDARD TABLE OF rv_count WITH DEFAULT KEY.
    DATA(lt_bus) = VALUE int_tab( FOR lv_bus_txt IN lt_bus_txt
      ( COND #( WHEN lv_bus_txt = 'x' THEN 1 ELSE lv_bus_txt ) ) ).

    rv_count = 0.
    DATA(stepsize) = lt_bus[ 1 ].
    LOOP AT lt_bus INTO DATA(lv_bus) FROM 2.
      DATA(lv_index) = sy-tabix - 1.
      WHILE ( rv_count + lv_index ) MOD lv_bus <> 0.
        rv_count = rv_count + stepsize.
      ENDWHILE.

      stepsize = stepsize * lv_bus.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
