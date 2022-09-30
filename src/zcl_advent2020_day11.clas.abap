CLASS zcl_advent2020_day11 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES:
      BEGIN OF ts_shift,
        x TYPE i,
        y TYPE i,
      END OF ts_shift,
      tt_shift   TYPE STANDARD TABLE OF ts_shift WITH DEFAULT KEY,

      char1      TYPE c LENGTH 1,
      int4_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY,

      BEGIN OF ts_2dim,
        x     TYPE i,
        y     TYPE i,
        seat  TYPE char1,
        count TYPE i,
      END OF ts_2dim,
      tt_2dim TYPE HASHED TABLE OF ts_2dim WITH UNIQUE KEY x y.

    CONSTANTS:
      BEGIN OF ms_seat,
        empty    TYPE char1 VALUE 'L',
        occupied TYPE char1 VALUE '#',
        floor    TYPE char1 VALUE '.',
      END OF ms_seat,

      mc_infinite TYPE i VALUE 1000000.

    METHODS constructor .
    METHODS part1
      IMPORTING
        it_input        TYPE string_table
        iv_times        TYPE i
        im_mode         TYPE i DEFAULT 1
        iv_occupied     TYPE i DEFAULT 4
      RETURNING
        VALUE(rv_count) TYPE i.
    METHODS part2
      IMPORTING
                it_input        TYPE string_table
                iv_times        TYPE i
      RETURNING VALUE(rv_count) TYPE i.
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mt_shift TYPE tt_shift.
    DATA mt_2dim TYPE tt_2dim.
    DATA mv_mode TYPE i.

    METHODS _get_2dim
      IMPORTING
        it_input       TYPE string_table
      RETURNING
        VALUE(rt_2dim) TYPE tt_2dim .

    METHODS _calc_occupied_count .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY11 IMPLEMENTATION.


  METHOD constructor.
    DATA(t_shift) = VALUE int4_table( ( -1 ) ( 0 ) ( 1 ) ).
    LOOP AT t_shift INTO DATA(x).
      LOOP AT t_shift INTO DATA(y).
        CHECK x <> 0 OR y <> 0.
        INSERT VALUE #( x = x y = y ) INTO TABLE mt_shift.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.

    DATA(lv_count_1) = part1( it_input = lt_input
                              iv_times = mc_infinite ).
    DATA(lv_count_2) = part2( it_input = lt_input
                              iv_times = mc_infinite ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count_1 } - { lv_count_2 }| ).
  ENDMETHOD.


  METHOD part1.
    mt_2dim = _get_2dim( it_input ).

    DO iv_times TIMES.
      _calc_occupied_count( ).

      DATA(lv_changed) = abap_false.
      LOOP AT mt_2dim ASSIGNING FIELD-SYMBOL(<ls_2dim>) WHERE seat <> ms_seat-floor.
        DATA(lv_new_seat) = <ls_2dim>-seat.
        CASE <ls_2dim>-seat.
          WHEN ms_seat-empty.
            IF <ls_2dim>-count = 0.
              lv_new_seat = ms_seat-occupied.
            ENDIF.

          WHEN ms_seat-occupied.
            IF <ls_2dim>-count >= iv_occupied.
              lv_new_seat = ms_seat-empty.
            ENDIF.
        ENDCASE.

        " For next iteration
        <ls_2dim>-count = 0.

        CHECK <ls_2dim>-seat <> lv_new_seat.
        <ls_2dim>-seat = lv_new_seat.
        lv_changed = abap_true.
      ENDLOOP.

      " Already is Zen
      CHECK lv_changed <> abap_true.
      rv_count = REDUCE #( INIT s = 0
                           FOR <line> IN mt_2dim
                           WHERE ( seat = ms_seat-occupied )
                           NEXT s = s + 1 ).
      RETURN.
    ENDDO.
  ENDMETHOD.


  METHOD part2.
    mv_mode = 2.
    rv_count = part1(
             it_input    = it_input
             iv_times    = iv_times
             iv_occupied = 5 ).
  ENDMETHOD.


  METHOD _calc_occupied_count.
    LOOP AT mt_2dim ASSIGNING FIELD-SYMBOL(<ls_2dim>) WHERE seat = ms_seat-occupied.
      LOOP AT mt_shift ASSIGNING FIELD-SYMBOL(<ls_shift>).
        DATA(x) = <ls_2dim>-x + <ls_shift>-x.
        DATA(y) = <ls_2dim>-y + <ls_shift>-y.

        ASSIGN mt_2dim[ x = x y = y ] TO FIELD-SYMBOL(<ls_result>).
        CHECK sy-subrc = 0.

        WHILE mv_mode = 2 AND <ls_result> IS ASSIGNED AND <ls_result>-seat = ms_seat-floor.
          x += <ls_shift>-x.
          y += <ls_shift>-y.
          UNASSIGN <ls_result>.
          ASSIGN mt_2dim[ x = x y = y ] TO <ls_result>.
        ENDWHILE.

        CHECK <ls_result> IS ASSIGNED.
        <ls_result>-count += 1.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_2dim.
    LOOP AT it_input INTO DATA(lv_input).
      DATA(y) = sy-tabix.
      DO strlen( lv_input ) TIMES.
        DATA(x) = sy-index.

        DATA(lv_off)  = x - 1.
        INSERT VALUE #( x     = x
                        y     = y
                        seat  = lv_input+lv_off(1) ) INTO TABLE rt_2dim[].
      ENDDO.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
