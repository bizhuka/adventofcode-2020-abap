CLASS zcl_advent2020_day10 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES:
      inttab      TYPE STANDARD TABLE OF int8 WITH DEFAULT KEY .

    METHODS part1
      IMPORTING
        it_input        TYPE inttab
        iv_diff         TYPE i
      RETURNING
        VALUE(rv_count) TYPE int8 .
    METHODS part2
      IMPORTING
        it_input        TYPE inttab
      RETURNING
        VALUE(rv_count) TYPE int8 .
  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS _get_with_bounds
      IMPORTING
        it_input       TYPE inttab
      RETURNING
        VALUE(rt_copy) TYPE inttab .
ENDCLASS.



CLASS zcl_advent2020_day10 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_next_1) = part1( it_input = lt_input
                             iv_diff  = 1 ) *
                      part1( it_input = lt_input
                             iv_diff  = 3 ).

    DATA(lv_next_2) = part2( it_input = lt_input ).

    CHECK out IS NOT INITIAL.
    out->write( |{ lv_next_1 } - { lv_next_2 }| ).
  ENDMETHOD.


  METHOD part1.
    DATA(lt_copy) = _get_with_bounds( it_input ).

    LOOP AT lt_copy INTO DATA(lv_num1) FROM 2.
      DATA(lv_diff) = lt_copy[ sy-tabix - 1 ] - lv_num1.

      CHECK lv_diff = iv_diff.
      ADD 1 TO rv_count.
    ENDLOOP.
  ENDMETHOD.


  METHOD part2.
    DATA(lt_copy)  = _get_with_bounds( it_input ).
    DATA(lt_count) = VALUE inttab( FOR i = 1 UNTIL i > lines( lt_copy ) ( COND #( WHEN i = 1 THEN 1 ELSE 0 ) ) ).

    LOOP AT lt_copy INTO DATA(lv_num1) FROM 2.
      DATA(lv_tabix) = sy-tabix.
      DATA(lv_count) = 0.

      DATA(lv_prev_index) = lv_tabix - 1.
      WHILE lt_copy[ lv_prev_index ] - lv_num1 <= 3.
        lv_prev_index = lv_prev_index - 1.
        ADD 1 TO lv_count.
        IF lv_prev_index = 0.
          EXIT.
        ENDIF.
      ENDWHILE.

      lv_prev_index = lv_tabix.
      DO lv_count TIMES.
        lv_prev_index = lv_prev_index - 1.
        CHECK lv_prev_index >= 1.
        lt_count[ lv_tabix ] = lt_count[ lv_tabix ] + lt_count[ lv_prev_index ].
      ENDDO.

      rv_count = lt_count[ lv_tabix ].
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_with_bounds.
    rt_copy[] = it_input[].
    SORT rt_copy BY table_line DESCENDING.

    INSERT CONV #( 0 )      INTO TABLE rt_copy[].
    INSERT rt_copy[ 1 ] + 3 INTO       rt_copy[] INDEX 1.
  ENDMETHOD.
ENDCLASS.
