CLASS zcl_advent2020_day24 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    CONSTANTS:
      BEGIN OF ms_color,
        white TYPE abap_bool VALUE abap_false,
        black TYPE abap_bool VALUE abap_true,
      END OF ms_color .

    TYPES:
      BEGIN OF ts_adjacent,
        dir TYPE char2,
        x   TYPE i,
        y   TYPE i,
        z   TYPE i,
      END OF ts_adjacent .
    TYPES:
      tt_adjacent TYPE HASHED TABLE OF ts_adjacent WITH UNIQUE KEY dir .
    TYPES:
      BEGIN OF ts_cube,
        x     TYPE i,
        y     TYPE i,
        z     TYPE i,
        color TYPE abap_bool,
      END OF ts_cube .
    TYPES:
      tt_cube TYPE HASHED TABLE OF ts_cube WITH UNIQUE KEY x y z .

    DATA mt_adjacent TYPE tt_adjacent .
    DATA mt_cube TYPE tt_cube .

    METHODS constructor .
    METHODS part1
      IMPORTING
        !it_input       TYPE stringtab
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
    METHODS part2
      IMPORTING
        !it_input       TYPE stringtab
        !iv_days        TYPE i
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
  PROTECTED SECTION.

private section.

  methods _GET_BLACK_COUNT
    importing
      !IS_CUBE type TS_CUBE
    returning
      value(RV_COUNT) type I .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY24 IMPLEMENTATION.


  METHOD constructor.
    INSERT VALUE #( dir = 'e'  x = +1 y = +0 z = -1 ) INTO TABLE mt_adjacent.
    INSERT VALUE #( dir = 'se' x = +0 y = +1 z = -1 ) INTO TABLE mt_adjacent.
    INSERT VALUE #( dir = 'sw' x = -1 y = +1 z = +0 ) INTO TABLE mt_adjacent.
    INSERT VALUE #( dir = 'w'  x = -1 y = +0 z = +1 ) INTO TABLE mt_adjacent.
    INSERT VALUE #( dir = 'nw' x = +0 y = -1 z = +1 ) INTO TABLE mt_adjacent.
    INSERT VALUE #( dir = 'ne' x = +1 y = -1 z = +0 ) INTO TABLE mt_adjacent.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( it_input = lt_input
                             iv_days  = 100 ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    mt_cube = VALUE #( ).
    LOOP AT it_input INTO DATA(lv_input).
      DATA(ls_curr_cube) = VALUE ts_cube( color = ms_color-black ).

      FIND ALL OCCURRENCES OF REGEX '(se|sw|nw|ne|e|w)' IN lv_input RESULTS DATA(lt_results).
      DATA(lv_offset) = 0.
      LOOP AT lt_results ASSIGNING FIELD-SYMBOL(<ls_result>).
        " No blanks?
        ASSERT lv_offset = <ls_result>-offset.
        lv_offset = lv_offset + <ls_result>-length.

        DATA(lv_dir) = CONV char2( lv_input+<ls_result>-offset(<ls_result>-length) ).
        ASSIGN mt_adjacent[ dir = lv_dir ] TO FIELD-SYMBOL(<ls_adjacent>).
        ASSERT sy-subrc = 0.

        ADD: <ls_adjacent>-x TO ls_curr_cube-x,
             <ls_adjacent>-y TO ls_curr_cube-y,
             <ls_adjacent>-z TO ls_curr_cube-z.
      ENDLOOP.

      INSERT ls_curr_cube INTO TABLE mt_cube.
      IF sy-subrc <> 0.
        DELETE mt_cube WHERE table_line = ls_curr_cube.
      ENDIF.
    ENDLOOP.

    rv_count = lines( mt_cube ).
  ENDMETHOD.


  METHOD part2.
    part1( it_input ).
    DATA(lt_cube) = mt_cube[].

    DO iv_days TIMES.
      DATA(lt_white) = VALUE tt_cube( ).
      LOOP AT lt_cube ASSIGNING FIELD-SYMBOL(<ls_cube>).
        LOOP AT mt_adjacent ASSIGNING FIELD-SYMBOL(<ls_adjacent>).
          DATA(x) = <ls_cube>-x + <ls_adjacent>-x.
          DATA(y) = <ls_cube>-y + <ls_adjacent>-y.
          DATA(z) = <ls_cube>-z + <ls_adjacent>-z.
          ASSIGN lt_cube[ x = x y = y z = z ] TO FIELD-SYMBOL(<ls_cube_curr>).
          CHECK sy-subrc <> 0.
          INSERT VALUE #( x = x y = y z = z color = ms_color-white ) INTO TABLE lt_white.
        ENDLOOP.
      ENDLOOP.
      INSERT LINES OF lt_white INTO TABLE lt_cube[].
*
      DATA(lt_black) = VALUE tt_cube( ).
      LOOP AT lt_cube ASSIGNING <ls_cube>.
        DATA(lv_color) = <ls_cube>-color.
        DATA(lv_black_count) = _get_black_count( <ls_cube> ).
        CASE lv_color.
          WHEN ms_color-black.
            IF lv_black_count = 0 OR lv_black_count > 2.
              lv_color = ms_color-white.
            ENDIF.

          WHEN ms_color-white.
            IF  lv_black_count = 2.
              lv_color = ms_color-black.
            ENDIF.
        ENDCASE.

        CHECK lv_color = ms_color-black.
        INSERT VALUE #( x    = <ls_cube>-x
                        y    = <ls_cube>-y
                        z    = <ls_cube>-z
                        color = lv_color ) INTO TABLE lt_black[].
      ENDLOOP.

      mt_cube[] = lt_cube[] = lt_black[].
    ENDDO.

    rv_count = lines( mt_cube ).
  ENDMETHOD.


  METHOD _get_black_count.
    LOOP AT mt_adjacent ASSIGNING FIELD-SYMBOL(<ls_adjacent>).

      ASSIGN mt_cube[ x = is_cube-x + <ls_adjacent>-x
                      y = is_cube-y + <ls_adjacent>-y
                      z = is_cube-z + <ls_adjacent>-z ] TO FIELD-SYMBOL(<ls_cube_curr>).
      CHECK sy-subrc = 0.

      CHECK <ls_cube_curr>-color = ms_color-black.
      rv_count = rv_count + 1.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
