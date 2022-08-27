class ZCL_ADVENT2020_DAY17 definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  types:
    BEGIN OF ts_adjacent,
        x TYPE i,
        y TYPE i,
        z TYPE i,
        w TYPE i,
      END OF ts_adjacent .
  types:
    tt_adjacent TYPE STANDARD TABLE OF ts_adjacent WITH DEFAULT KEY .
  types:
    BEGIN OF ts_cube,
        x     TYPE i,
        y     TYPE i,
        z     TYPE i,
        w     TYPE i,
        state TYPE char1,
      END OF ts_cube .
  types:
    tt_cube TYPE HASHED TABLE OF ts_cube WITH UNIQUE KEY x y z w .

  constants:
    BEGIN OF ms_state,
        active   TYPE char1 VALUE '#',
        inactive TYPE char1 VALUE '.',
      END OF ms_state .
  data MT_ADJACENT type TT_ADJACENT .
  data MT_CUBE type TT_CUBE .

    "iv_active    TYPE i DEFAULT 4
  methods PART1
    importing
      !IT_INPUT type STRINGTAB
      !IV_CYCLE type I default 6
      !IM_MODE type I default 1
    returning
      value(RT_CUBE) type TT_CUBE .
  methods PART2
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RT_CUBE) type TT_CUBE .
  methods GET_CUBE
    importing
      !IT_INPUT type STRINGTAB
      !IV_Z type I optional
      !IV_W type I optional
    returning
      value(RT_CUBE) type TT_CUBE .
  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _get_active_count
      IMPORTING
        !is_cube        TYPE ts_cube
        !im_mode        TYPE i
      RETURNING
        VALUE(rv_count) TYPE i .
    METHODS _get_adjacent
      IMPORTING
        !im_mode           TYPE i
      RETURNING
        VALUE(rt_adjacent) TYPE tt_adjacent .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY17 IMPLEMENTATION.


  METHOD get_cube.
    LOOP AT it_input INTO DATA(lv_input).
      DATA(x) = sy-tabix.
      DO strlen( lv_input ) TIMES.
        DATA(y)        = sy-index.
        DATA(lv_index) = sy-index - 1.

        INSERT VALUE #( x     = x
                        y     = y
                        z     = iv_z
                        w     = iv_w
                        state = lv_input+lv_index(1) ) INTO TABLE rt_cube[].
      ENDDO.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)  = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = lines( part1( lt_input ) ).
    DATA(lv_count2) = lines( part2( lt_input ) ).

    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    mt_adjacent = _get_adjacent( im_mode = im_mode ).

    mt_cube[] = rt_cube[] = get_cube( it_input ).

    DO iv_cycle TIMES.
      DATA(lt_new) = VALUE tt_cube( ).
      LOOP AT rt_cube ASSIGNING FIELD-SYMBOL(<ls_cube>).
        LOOP AT mt_adjacent ASSIGNING FIELD-SYMBOL(<ls_adjacent>).
          DATA(x) = <ls_cube>-x + <ls_adjacent>-x.
          DATA(y) = <ls_cube>-y + <ls_adjacent>-y.
          DATA(z) = <ls_cube>-z + <ls_adjacent>-z.
          DATA(w) = <ls_cube>-w + <ls_adjacent>-w.
          ASSIGN rt_cube[ x = x y = y z = z w = w ] TO FIELD-SYMBOL(<ls_cube_curr>).
          CHECK sy-subrc <> 0.
          INSERT VALUE #( x = x y = y z = z w = w state = ms_state-inactive ) INTO TABLE lt_new.
        ENDLOOP.
      ENDLOOP.
      INSERT LINES OF lt_new INTO TABLE rt_cube[].

      DATA(lt_result) = VALUE tt_cube( ).
      DATA(lv_active) = 0.
      LOOP AT rt_cube ASSIGNING <ls_cube>.
        DATA(lv_state) = <ls_cube>-state.
        DATA(lv_active_count) = _get_active_count( is_cube = <ls_cube>
                                                   im_mode = im_mode ).
        CASE lv_state.
          WHEN ms_state-active.
            IF NOT lv_active_count BETWEEN 2 AND 3.
              lv_state = ms_state-inactive.
            ENDIF.

          WHEN ms_state-inactive.
            IF  lv_active_count = 3.
              lv_state = ms_state-active.
            ENDIF.
        ENDCASE.

        IF lv_state = ms_state-active.
          lv_active = lv_active + 1.
        ENDIF.

        INSERT VALUE #( x    = <ls_cube>-x
                        y    = <ls_cube>-y
                        z    = <ls_cube>-z
                        w    = <ls_cube>-w
                        state = lv_state ) INTO TABLE lt_result[].
      ENDLOOP.

      mt_cube[] =  rt_cube[] = lt_result[].
    ENDDO.

    DELETE rt_cube[] WHERE state <> ms_state-active.
  ENDMETHOD.


  METHOD part2.
    rt_cube = part1(
     it_input  = it_input
     im_mode   = 2 ).
  ENDMETHOD.


  METHOD _get_active_count.

    LOOP AT mt_adjacent ASSIGNING FIELD-SYMBOL(<ls_adjacent>).

      ASSIGN mt_cube[ x = is_cube-x + <ls_adjacent>-x
                      y = is_cube-y + <ls_adjacent>-y
                      z = is_cube-z + <ls_adjacent>-z
                      w = is_cube-w + <ls_adjacent>-w ] TO FIELD-SYMBOL(<ls_cube_curr>).
      CHECK sy-subrc = 0.

      CHECK <ls_cube_curr>-state = ms_state-active.
      rv_count = rv_count + 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_adjacent.
    DATA(lt_diff)  = VALUE int4_table( ( -1 ) ( 0 ) ( 1 ) ).
    DATA(lt_dim_w) = COND #( WHEN im_mode = 2 THEN lt_diff ELSE VALUE #( ( 0 ) ) ).

    LOOP AT lt_diff INTO DATA(x).
      LOOP AT lt_diff INTO DATA(y).
        LOOP AT lt_diff INTO DATA(z).
          LOOP AT lt_dim_w INTO DATA(w).
            DATA(ls_adjacent) = VALUE ts_adjacent( x = x
                                                   y = y
                                                   z = z
                                                   w = w ).
            CHECK ls_adjacent IS NOT INITIAL.
            APPEND ls_adjacent TO rt_adjacent.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
