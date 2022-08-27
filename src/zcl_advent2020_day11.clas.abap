CLASS zcl_advent2020_day11 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES:
      BEGIN OF ts_adjacent,
        x       TYPE i,
        shift_x TYPE i,
        y       TYPE i,
        shift_y TYPE i,
        apply   TYPE abap_bool,
      END OF ts_adjacent .
    TYPES:
      tt_adjacent TYPE STANDARD TABLE OF ts_adjacent WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF ms_seat,
        empty    TYPE char1 VALUE 'L',
        occupied TYPE char1 VALUE '#',
        floor    TYPE char1 VALUE '.',
      END OF ms_seat .

    METHODS constructor .
    METHODS part1
      IMPORTING
        it_input       TYPE stringtab
        iv_times       TYPE i OPTIONAL
        im_mode        TYPE i DEFAULT 1
        iv_occupied    TYPE i DEFAULT 4
        rr_occupied    TYPE REF TO i OPTIONAL
      RETURNING
        VALUE(rv_2dim) TYPE string.
    METHODS part2
      IMPORTING
                it_input       TYPE stringtab
                rr_occupied    TYPE REF TO i OPTIONAL
                iv_times       TYPE i OPTIONAL
      RETURNING VALUE(rv_2dim) TYPE string.
  PROTECTED SECTION.

private section.

  data MT_ADJACENT type TT_ADJACENT .
  data MV_2DIM type STRING .
  data MV_ROW_COUNT type I .
  data MV_COL_COUNT type I .

  methods _GET_2DIM
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_2DIM) type STRING .
  methods _GET_OCCUPIED_COUNT
    importing
      !IV_INDEX type I
      !IM_MODE type I
    returning
      value(RV_OCCUPIED_COUNT) type I .
  methods _GET_X_Y
    importing
      !IV_INDEX type I
    exporting
      !X type I
      !Y type I
      !S type CHAR1
    raising
      CX_PARAMETER_INVALID .
  methods _GET_INDEX
    importing
      !_X type I
      !_Y type I
    returning
      value(_RV_INDEX) type I
    raising
      CX_PARAMETER_INVALID .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY11 IMPLEMENTATION.


  METHOD constructor.
    mt_adjacent = VALUE #(
      ( x = -1 y = -1 )
      ( x = -1 y = 0  )
      ( x = -1 y = 1  )
      ( x = 0  y = -1 )
      ( x = 0  y = 1  )
      ( x = 1  y = -1 )
      ( x = 1  y = 0  )
      ( x = 1  y = 1  )
    ).
    LOOP AT mt_adjacent ASSIGNING FIELD-SYMBOL(<ls_adjacent>).
      <ls_adjacent>-shift_x = <ls_adjacent>-x.
      <ls_adjacent>-shift_y = <ls_adjacent>-y.
      <ls_adjacent>-apply   = abap_true.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.

    DATA(lv_count_1) = NEW i( ).
    DATA(lv_count_2) = NEW i( ).
    part1( it_input    = lt_input
           iv_times    = -1
           rr_occupied = lv_count_1 ).
    part2( it_input    = lt_input
           iv_times    = -1
           rr_occupied = lv_count_2 ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count_1->* } - { lv_count_2->* }| ).
  ENDMETHOD.


  METHOD part1.
    rv_2dim = _get_2dim( it_input ).

    DO.
      IF sy-index = iv_times + 1. " 0 for no loop, -1 for infinite loop
        EXIT.
      ENDIF.

      DATA(lv_result)      = rv_2dim.
      DATA(lv_rule_from_l) = xsdbool( sy-index MOD 2 = 1 ).
      DATA(lv_occupied)    = 0.

      DO strlen( rv_2dim ) TIMES.
        DATA(lv_index) = sy-index - 1.
        DATA(lv_seat)  = CONV char1( rv_2dim+lv_index(1) ).
        IF lv_seat <> ms_seat-floor.
          DATA(lv_occupied_count) = _get_occupied_count( iv_index = lv_index + 1
                                                         im_mode  = im_mode ).
          CASE lv_rule_from_l.
            WHEN abap_true.
              IF lv_seat = ms_seat-empty AND lv_occupied_count = 0.
                lv_seat = ms_seat-occupied.
              ENDIF.

            WHEN abap_false.
              IF lv_seat = ms_seat-occupied AND lv_occupied_count >= iv_occupied.
                lv_seat = ms_seat-empty.
              ENDIF.
          ENDCASE.

          IF lv_seat = ms_seat-occupied.
            lv_occupied = lv_occupied + 1.
          ENDIF.
        ENDIF.

        REPLACE SECTION OFFSET lv_index LENGTH 1 OF lv_result WITH lv_seat.
      ENDDO.

      IF rr_occupied IS NOT INITIAL.
        rr_occupied->* = lv_occupied.
      ENDIF.

      " Already is Zen
      IF rv_2dim = lv_result.
        EXIT.
      ENDIF.
      mv_2dim = rv_2dim = lv_result.
    ENDDO.
  ENDMETHOD.


  METHOD part2.
    rv_2dim = part1(
             it_input    = it_input
             iv_times    = iv_times
             im_mode     = 2
             iv_occupied = 5
             rr_occupied = rr_occupied
      ).
  ENDMETHOD.


  METHOD _get_2dim.
    mv_row_count = lines( it_input ).
    mv_col_count = strlen( it_input[ 1 ] ).
    rv_2dim      = mv_2dim = concat_lines_of( table = it_input ).
  ENDMETHOD.


METHOD _get_index.
 _get_index.
ENDMETHOD.


  METHOD _get_occupied_count.
    LOOP AT mt_adjacent ASSIGNING FIELD-SYMBOL(<ls_adjacent>).
      <ls_adjacent>-x     = <ls_adjacent>-shift_x.
      <ls_adjacent>-y     = <ls_adjacent>-shift_y.
      <ls_adjacent>-apply = abap_true.
    ENDLOOP.

***    _get_x_y( EXPORTING iv_index = iv_index
***              IMPORTING x        = DATA(x)
***                        y        = DATA(y) ).
    DATA: x TYPE i, y TYPE i, _rv_index TYPE i.
    _get_x_y.

    DO.
      LOOP AT mt_adjacent ASSIGNING <ls_adjacent> WHERE apply = abap_true.
        DATA(lv_tabix) = sy-tabix.

        TRY.
***            DATA(lv_index) = _get_index( _x = x + <ls_adjacent>-x
***                                         _y = y + <ls_adjacent>-y ) - 1.
            DATA(_x) = x + <ls_adjacent>-x.
            DATA(_y) = y + <ls_adjacent>-y.
            _get_index.
            _rv_index = _rv_index - 1.

          CATCH cx_parameter_invalid.
            CLEAR <ls_adjacent>-apply.
            CONTINUE.
        ENDTRY.

        DATA(lv_seat) = mv_2dim+_rv_index(1).

        IF im_mode = 2 AND lv_seat = ms_seat-floor.
          <ls_adjacent>-x = <ls_adjacent>-x + <ls_adjacent>-shift_x.
          <ls_adjacent>-y = <ls_adjacent>-y + <ls_adjacent>-shift_y.
        ELSE.
          CLEAR <ls_adjacent>-apply.
        ENDIF.

        CHECK lv_seat = ms_seat-occupied.
        rv_occupied_count = rv_occupied_count + 1.
      ENDLOOP.

      IF im_mode = 1 OR sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.


METHOD _get_x_y.
  _get_x_y.

  CHECK s IS REQUESTED.
  DATA(lv_index) = iv_index - 1.
  s = mv_2dim+lv_index(1).
ENDMETHOD.
ENDCLASS.
