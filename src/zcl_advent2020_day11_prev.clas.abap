class ZCL_ADVENT2020_DAY11_PREV definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  types:
    BEGIN OF ts_2dim,
        x    TYPE i,
        y    TYPE i,
        seat TYPE char1,
      END OF ts_2dim .
  types:
    tt_2dim TYPE HASHED TABLE OF ts_2dim WITH UNIQUE KEY x y .
  types:
    BEGIN OF ts_adjacent,
        x       TYPE i,
        shift_x TYPE i,
        y       TYPE i,
        shift_y TYPE i,
        apply   TYPE abap_bool,
      END OF ts_adjacent .
  types:
    tt_adjacent TYPE STANDARD TABLE OF ts_adjacent WITH DEFAULT KEY .

  constants:
    BEGIN OF ms_seat,
        empty    TYPE char1 VALUE 'L',
        occupied TYPE char1 VALUE '#',
        floor    TYPE char1 VALUE '.',
      END OF ms_seat .

  methods CONSTRUCTOR .
  methods PART1
    importing
      !IT_INPUT type STRINGTAB
      !IV_TIMES type I optional
      !IM_MODE type I default 1
      !IV_OCCUPIED type I default 4
      !RR_OCCUPIED type ref to I optional
    returning
      value(RT_2DIM) type TT_2DIM .
  methods PART2
    importing
      !IT_INPUT type STRINGTAB
      !RR_OCCUPIED type ref to I optional
      !IV_TIMES type I optional
    returning
      value(RT_2DIM) type TT_2DIM .
  PROTECTED SECTION.

private section.

  data MT_ADJACENT type TT_ADJACENT .
  data MT_2DIM_CURR type TT_2DIM .

  methods _GET_2DIM
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RT_2DIM) type TT_2DIM .
  methods _GET_OCCUPIED_COUNT
    importing
      !IS_2DIM type TS_2DIM
      !IM_MODE type I
    returning
      value(RV_OCCUPIED_COUNT) type I .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY11_PREV IMPLEMENTATION.


  METHOD CONSTRUCTOR.
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
    ENDLOOP.
  ENDMETHOD.


  METHOD IF_OO_ADT_CLASSRUN~MAIN.
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


  METHOD PART1.
    mt_2dim_curr[] = rt_2dim[] = _get_2dim( it_input ).

    DO.
      IF sy-index = iv_times + 1. " 0 for no loop, -1 for infinite loop
        EXIT.
      ENDIF.

      DATA(lv_rule_from_l) = xsdbool( sy-index MOD 2 = 1 ).
      DATA(lt_result)      = VALUE tt_2dim( ).
      DATA(lv_occupied)    = 0.

      LOOP AT rt_2dim ASSIGNING FIELD-SYMBOL(<ls_2dim>).
        DATA(lv_seat) = <ls_2dim>-seat.
        IF <ls_2dim>-seat <> ms_seat-floor.
          DATA(lv_occupied_count) = _get_occupied_count( is_2dim = <ls_2dim>
                                                         im_mode = im_mode ).
          CASE lv_rule_from_l.
            WHEN abap_true.
              IF <ls_2dim>-seat = ms_seat-empty AND lv_occupied_count = 0.
                lv_seat = ms_seat-occupied.
              ENDIF.

            WHEN abap_false.
              IF <ls_2dim>-seat = ms_seat-occupied AND lv_occupied_count >= iv_occupied.
                lv_seat = ms_seat-empty.
              ENDIF.
          ENDCASE.

          IF lv_seat = ms_seat-occupied.
            lv_occupied = lv_occupied + 1.
          ENDIF.
        ENDIF.
        INSERT VALUE #( x    = <ls_2dim>-x
                        y    = <ls_2dim>-y
                        seat = lv_seat ) INTO TABLE lt_result[].
      ENDLOOP.

      IF rr_occupied IS NOT INITIAL.
        rr_occupied->* = lv_occupied.
      ENDIF.

      " Already is Zen
      IF rt_2dim[] = lt_result[].
        EXIT.
      ENDIF.
      mt_2dim_curr[] =  rt_2dim[] = lt_result[].
    ENDDO.
  ENDMETHOD.


  METHOD PART2.
    rt_2dim = part1(
             it_input    = it_input
             iv_times    = iv_times
             im_mode     = 2
             iv_occupied = 5
             rr_occupied = rr_occupied
      ).
  ENDMETHOD.


  METHOD _GET_2DIM.
    LOOP AT it_input INTO DATA(lv_input).
      DATA(x) = sy-tabix.
      DO strlen( lv_input ) TIMES.
        DATA(y) = sy-index.

        DATA(lv_off)  = y - 1.
        INSERT VALUE #( x     = x
                        y     = y
                        seat  = lv_input+lv_off(1) ) INTO TABLE rt_2dim[].
      ENDDO.
    ENDLOOP.
  ENDMETHOD.


  METHOD _GET_OCCUPIED_COUNT.
    LOOP AT mt_adjacent ASSIGNING FIELD-SYMBOL(<ls_adjacent>).
      <ls_adjacent>-x     = <ls_adjacent>-shift_x.
      <ls_adjacent>-y     = <ls_adjacent>-shift_y.
      <ls_adjacent>-apply = abap_true.
    ENDLOOP.

    DO.
      LOOP AT mt_adjacent ASSIGNING <ls_adjacent> WHERE apply = abap_true.
        DATA(lv_tabix) = sy-tabix.
*
        ASSIGN mt_2dim_curr[ x = is_2dim-x + <ls_adjacent>-x
                             y = is_2dim-y + <ls_adjacent>-y ] TO FIELD-SYMBOL(<ls_2dim_curr>).
        IF sy-subrc <> 0.
          CLEAR <ls_adjacent>-apply.
          CONTINUE.
        ENDIF.

        IF im_mode = 2 AND <ls_2dim_curr>-seat = ms_seat-floor.
          <ls_adjacent>-x = <ls_adjacent>-x + <ls_adjacent>-shift_x.
          <ls_adjacent>-y = <ls_adjacent>-y + <ls_adjacent>-shift_y.
        ELSE.
          CLEAR <ls_adjacent>-apply.
        ENDIF.

        CHECK <ls_2dim_curr>-seat = ms_seat-occupied.
        rv_occupied_count = rv_occupied_count + 1.
      ENDLOOP.

      IF im_mode = 1 OR sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
