CLASS zcl_advent2020_day17 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    TYPES:
      char1      TYPE c LENGTH 1,
      int4_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ts_cube,
        x   TYPE i,
        y   TYPE i,
        z   TYPE i,
        w   TYPE i,
        cnt TYPE i,
      END OF ts_cube.
    TYPES:
      tt_cube TYPE HASHED TABLE OF ts_cube WITH UNIQUE KEY x y z w.

    CONSTANTS:
      BEGIN OF ms_state,
        active   TYPE char1 VALUE '#',
        inactive TYPE char1 VALUE '.',
      END OF ms_state.
    DATA mt_adjacent TYPE tt_cube.

    "iv_active    TYPE i DEFAULT 4
    METHODS part1
      IMPORTING
        it_input        TYPE string_table
        iv_cycle        TYPE i DEFAULT 6
        im_mode         TYPE i DEFAULT 1
      RETURNING
        VALUE(rv_count) TYPE i.

    METHODS part2
      IMPORTING
        it_input        TYPE string_table
      RETURNING
        VALUE(rv_count) TYPE i.
    METHODS get_active_cube
      IMPORTING
        it_input       TYPE string_table
        iv_z           TYPE i OPTIONAL
        iv_w           TYPE i OPTIONAL
      RETURNING
        VALUE(rt_cube) TYPE tt_cube.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS _get_adjacent
      IMPORTING
        im_mode            TYPE i
      RETURNING
        VALUE(rt_adjacent) TYPE tt_cube.
ENDCLASS.



CLASS zcl_advent2020_day17 IMPLEMENTATION.


  METHOD get_active_cube.
    LOOP AT it_input INTO DATA(lv_input).
      DATA(y) = sy-tabix.
      DO strlen( lv_input ) TIMES.
        DATA(x)        = sy-index.

        DATA(lv_index) = sy-index - 1.
        CHECK lv_input+lv_index(1) = ms_state-active.

        INSERT VALUE #( x     = x
                        y     = y
                        z     = iv_z
                        w     = iv_w ) INTO TABLE rt_cube[].
      ENDDO.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)  = NEW lcl_input(  )->mt_input.
    CHECK out IS NOT INITIAL.
    out->write( |{ part1( lt_input ) } { part2( lt_input ) }| ).
  ENDMETHOD.


  METHOD part1.
    mt_adjacent = _get_adjacent( im_mode = im_mode ).

    DATA(lt_cube) = get_active_cube( it_input ).

    DO iv_cycle TIMES.

      DATA(lt_count) = VALUE tt_cube( ).
      LOOP AT lt_cube ASSIGNING FIELD-SYMBOL(<ls_cube>).
        LOOP AT mt_adjacent ASSIGNING FIELD-SYMBOL(<ls_adjacent>).
          COLLECT VALUE ts_cube( x   = <ls_cube>-x + <ls_adjacent>-x
                                 y   = <ls_cube>-y + <ls_adjacent>-y
                                 z   = <ls_cube>-z + <ls_adjacent>-z
                                 w   = <ls_cube>-w + <ls_adjacent>-w
                                 cnt = 1 ) INTO lt_count.
        ENDLOOP.
      ENDLOOP.

      DATA(lt_new) = VALUE tt_cube( ).
      LOOP AT lt_count ASSIGNING FIELD-SYMBOL(<ls_count>) WHERE cnt = 3.
        ASSIGN lt_cube[ x = <ls_count>-x
                        y = <ls_count>-y
                        z = <ls_count>-z
                        w = <ls_count>-w ] TO <ls_cube>.
        CHECK sy-subrc <> 0.
        INSERT <ls_count> INTO TABLE lt_new.
      ENDLOOP.

      LOOP AT lt_cube ASSIGNING <ls_cube>.
        ASSIGN lt_count[ x = <ls_cube>-x
                         y = <ls_cube>-y
                         z = <ls_cube>-z
                         w = <ls_cube>-w ] TO <ls_count>.
        CHECK sy-subrc = 0 AND <ls_count>-cnt BETWEEN 2 AND 3.
        INSERT <ls_count> INTO TABLE lt_new.
      ENDLOOP.

      lt_cube =  lt_new.
    ENDDO.

    rv_count = lines( lt_new ).
  ENDMETHOD.


  METHOD part2.
    rv_count = part1(
     it_input  = it_input
     im_mode   = 2 ).
  ENDMETHOD.


  METHOD _get_adjacent.
    DATA(lt_diff)  = VALUE int4_table( ( -1 ) ( 0 ) ( 1 ) ).
    DATA(lt_dim_w) = COND #( WHEN im_mode = 2 THEN lt_diff ELSE VALUE #( ( 0 ) ) ).

    LOOP AT lt_diff INTO DATA(x).
      LOOP AT lt_diff INTO DATA(y).
        LOOP AT lt_diff INTO DATA(z).
          LOOP AT lt_dim_w INTO DATA(w).
            DATA(ls_adjacent) = VALUE ts_cube( x = x
                                               y = y
                                               z = z
                                               w = w ).
            CHECK ls_adjacent IS NOT INITIAL.
            INSERT ls_adjacent INTO TABLE rt_adjacent.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
