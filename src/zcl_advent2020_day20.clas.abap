CLASS zcl_advent2020_day20 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS constructor .
    METHODS part1
      IMPORTING
        !it_input       TYPE string_table
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
    METHODS part2
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
  PROTECTED SECTION.

private section.

  types:
    char1    TYPE c LENGTH 1 .
  types:
    tt_char  TYPE STANDARD TABLE OF char1 WITH DEFAULT KEY .
  types:
    tt_field TYPE STANDARD TABLE OF tt_char WITH DEFAULT KEY .
  types:
    BEGIN OF ts_tile,
        id        TYPE i,
        t_field   TYPE tt_field,
        t_borders TYPE string_table,
      END OF ts_tile .
  types:
    tt_tile TYPE HASHED TABLE OF ts_tile WITH UNIQUE KEY id .
  types:
    BEGIN OF ts_match,
        border_id TYPE string,
        t_tile    TYPE STANDARD TABLE OF REF TO ts_tile WITH DEFAULT KEY,
      END OF ts_match .
  types:
    tt_match TYPE HASHED TABLE OF ts_match WITH UNIQUE KEY border_id .
  types:
    BEGIN OF ts_count,
        id  TYPE i,
        cnt TYPE i,
      END OF ts_count .
  types:
    tt_count TYPE SORTED TABLE OF ts_count WITH UNIQUE KEY id .

  constants MC_COMBINATION_COUNT type I value 8 ##NO_TEXT.
  constants MC_CHAR_COUNT type I value 8 ##NO_TEXT.
  data MT_TILE type TT_TILE .
  data MT_MATCH type TT_MATCH .
  data MT_COUNT type TT_COUNT .
  data MV_SIZE type I .
  data MT_ALL_FIELD type TT_FIELD .
  data MT_MONSTER_MAP type STRING_TABLE .
  data MT_MONSTER_FIELD type TT_FIELD .

  methods READ_TILES
    importing
      !IT_INPUT type STRING_TABLE
    returning
      value(RT_TILE) type TT_TILE .
  methods _CREATE_TILE
    importing
      !IT_ARRAY type STRING_TABLE
    returning
      value(RS_TILE) type TS_TILE .
  methods _FIND_MATCH
    returning
      value(RT_MATCH) type TT_MATCH .
  methods _CALC_COUNT
    returning
      value(RT_COUNT) type TT_COUNT .
  methods _SET_TILE
    importing
      !IV_TILE_ID type I
      !IV_X type I
      !IV_Y type I
      !IV_LEFT type STRING optional
      !IV_TOP type STRING optional .
  methods _SET_FIELD
    importing
      !IR_TILE type ref to TS_TILE
      !IV_X type I
      !IV_Y type I .
  methods _GET_OTHER_TILE_ID
    importing
      !IV_TILE_ID type I
      !IV_BORDER_ID type STRING
    returning
      value(RV_ID) type I .
  methods _GET_AS_FIELD
    importing
      !IT_INPUT type STRING_TABLE
    returning
      value(RT_FIELD) type TT_FIELD .
  methods _HAS_MONSTER
    importing
      !IV_X type I
      !IV_Y type I
    changing
      !CT_FIELD type TT_FIELD
    returning
      value(RV_OK) type ABAP_BOOL .
  methods _COUNT_GRID
    importing
      !IT_FIELD type TT_FIELD
    returning
      value(RV_OK) type I .
  methods _SET_BORDERS
    importing
      !IR_TILE type ref to TS_TILE .
  methods _NEXT_COMBINATION
    importing
      !IV_INDEX type I
    changing
      !CT_FIELD type TT_FIELD .
  methods _ROTATE90
    importing
      !IT_FIELD type TT_FIELD
    returning
      value(RT_FIELD) type TT_FIELD .
  methods _FLIP
    importing
      !IT_FIELD type TT_FIELD
    returning
      value(RT_FIELD) type TT_FIELD .
  methods _GET_BORDER_ID
    importing
      !IV_BORDER type STRING
    returning
      value(RV_ID) type STRING .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY20 IMPLEMENTATION.


  METHOD constructor.
    mt_monster_map = VALUE string_table(
      ( |                  # | )
      ( |#    ##    ##    ###| )
      ( | #  #  #  #  #  #   | ) ).
    mt_monster_field  = _get_as_field( mt_monster_map ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    mt_tile  = read_tiles( it_input ).
    mt_match = _find_match( ).
    mt_count = _calc_count( ).

    rv_count = REDUCE #( INIT m = CONV decfloat34( 1 )
                         FOR <ls_count> IN mt_count
                         WHERE ( cnt = 2 )
                         NEXT m = m * <ls_count>-id ).
  ENDMETHOD.


  METHOD part2.
    mv_size      = sqrt( lines( mt_tile ) ).
    mt_all_field = VALUE #( FOR i = 1 UNTIL i > mc_char_count * mv_size
                 ( VALUE #( FOR j = 1 UNTIL j > mc_char_count * mv_size ( ) ) ) ).

    " Any corner
    DATA(lv_corner_id) = mt_count[ cnt = 2 ]-id.

    DATA(lt_corner) = VALUE string_table( ).
    LOOP AT mt_match REFERENCE INTO DATA(lr_match).
      CHECK lines( lr_match->t_tile ) = 1
        AND lr_match->t_tile[ 1 ]->id = lv_corner_id.
      APPEND lr_match->border_id TO lt_corner[].
    ENDLOOP.

    _set_tile( iv_tile_id = lv_corner_id
               iv_x       = 1
               iv_y       = 1
               iv_left    = lt_corner[ 1 ] " <-- just 1 is enough
               iv_top     = lt_corner[ 2 ] ).


    DATA(lt_original) = mt_all_field[].
    DO mc_combination_count TIMES.
      DATA(lv_index)         = sy-index.
      DATA(lt_copy)          = lt_original[].
      DATA(lv_monster_count) = 0.

      DO mv_size * mc_char_count - lines( mt_monster_map ) TIMES.
        DATA(x) = sy-index.
        DO mv_size * mc_char_count - strlen( mt_monster_map[ 1 ] ) TIMES.
          DATA(y) = sy-index.

          CHECK _has_monster( EXPORTING iv_x     = x
                                        iv_y     = y
                              CHANGING  ct_field = lt_copy ) = abap_true.
          lv_monster_count += 1.
        ENDDO.
      ENDDO.

      IF lv_monster_count <> 0.
        rv_count = _count_grid( lt_copy ).
      ENDIF.

      _next_combination( EXPORTING iv_index = lv_index
                         CHANGING  ct_field = lt_original ).
    ENDDO.
  ENDMETHOD.


  METHOD read_tiles.
    DATA(lv_count) = lines( it_input ) + 1.
    ASSERT lv_count MOD 12 = 0.

    DO lv_count DIV 12 TIMES.
      DATA(lv_index) = sy-index.
      DATA(lv_from)  = ( lv_index - 1 ) * 12 + 1.

      DATA(lt_array) = VALUE string_table( ).
      LOOP AT it_input INTO DATA(lv_row) FROM lv_from TO lv_from + 10.
        APPEND lv_row TO lt_array[].
      ENDLOOP.

      INSERT _create_tile( it_array = lt_array[] ) INTO TABLE rt_tile[].
    ENDDO.
  ENDMETHOD.


  METHOD _calc_count.
    LOOP AT mt_match ASSIGNING FIELD-SYMBOL(<ls_match>).
      CHECK lines( <ls_match>-t_tile ) = 1.
      COLLECT VALUE ts_count( id  = <ls_match>-t_tile[ 1 ]->id
                              cnt = 1 ) INTO rt_count.
    ENDLOOP.
  ENDMETHOD.


  METHOD _count_grid.
    DATA(lv_input) = concat_lines_of( VALUE string_table(
                      FOR <lt_line> IN it_field
                   ( concat_lines_of( <lt_line> ) ) ) ).
    FIND ALL OCCURRENCES OF '#' IN lv_input MATCH COUNT rv_ok.
  ENDMETHOD.


  METHOD _create_tile.
    ASSERT lines( it_array ) = 11.

    ASSIGN it_array[ 1 ] TO FIELD-SYMBOL(<lv_tile>).
    ASSERT <lv_tile> CP 'Tile *:'.
    rs_tile-id   = CONV #( <lv_tile>+5(4) ).

    rs_tile-t_field = _get_as_field( VALUE #( FOR j = 1 UNTIL j > 10
                                              ( it_array[ j + 1 ] ) ) ).
    _set_borders( REF #( rs_tile ) ).
  ENDMETHOD.


  METHOD _find_match.
    LOOP AT mt_tile REFERENCE INTO DATA(lr_tile).
      LOOP AT lr_tile->t_borders ASSIGNING FIELD-SYMBOL(<lv_border>).
        DATA(lv_id) = _get_border_id( <lv_border> ) .
        ASSIGN rt_match[ border_id = lv_id ] TO FIELD-SYMBOL(<ls_match>).
        IF sy-subrc <> 0.
          INSERT VALUE #( border_id = lv_id  ) INTO TABLE rt_match ASSIGNING <ls_match>.
        ENDIF.

        INSERT lr_tile INTO TABLE <ls_match>-t_tile.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD _flip.
    DATA(lv_size) = lines( it_field[] ).
    rt_field = it_field[].

    DO lv_size TIMES.
      DATA(x) = sy-index.
      DO lv_size TIMES.
        DATA(y) = sy-index.

        rt_field[ y ][ x ] = it_field[ x ][ y ].
      ENDDO.
    ENDDO.
  ENDMETHOD.


  METHOD _get_as_field.
    LOOP AT it_input INTO DATA(lv_string).
      APPEND INITIAL LINE TO rt_field ASSIGNING FIELD-SYMBOL(<lt_line>).
      DO strlen( lv_string ) TIMES.
        DATA(lv_index) = sy-index - 1.
        APPEND lv_string+lv_index(1) TO <lt_line>.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.


  METHOD _get_border_id.
    rv_id = cmax( val1 = iv_border val2 = reverse( iv_border )  ).
  ENDMETHOD.


  METHOD _get_other_tile_id.
    ASSIGN mt_match[ border_id = iv_border_id ] TO FIELD-SYMBOL(<ls_match>).
    ASSERT sy-subrc = 0 AND lines( <ls_match>-t_tile ) = 2.

    DATA(lv_other_ind) = SWITCH #( line_index( <ls_match>-t_tile[ table_line->id = iv_tile_id ] ) WHEN 1 THEN 2
                                                                                                  WHEN 2 THEN 1 ).
    ASSIGN <ls_match>-t_tile[ lv_other_ind ] TO FIELD-SYMBOL(<ls_other_tile>).
    ASSERT sy-subrc = 0.

    rv_id = <ls_other_tile>->id.
  ENDMETHOD.


  METHOD _has_monster.
    DO lines( mt_monster_map ) TIMES.
      DATA(x) = sy-index.
      DO strlen( mt_monster_map[ 1 ] ) TIMES.
        DATA(y) = sy-index.

        IF mt_monster_field[ x ][ y ] = '#' AND ct_field[ x + iv_x ][ y + iv_y ] <> '#'.
          RETURN.
        ENDIF.
      ENDDO.
    ENDDO.

    DO lines( mt_monster_map ) TIMES.
      x = sy-index.
      DO strlen( mt_monster_map[ 1 ] ) TIMES.
        y = sy-index.

        CHECK mt_monster_field[ x ][ y ] = '#'.
        ct_field[ x + iv_x ][ y + iv_y ] = 'O'.
      ENDDO.
    ENDDO.

    rv_ok = abap_true.
  ENDMETHOD.


  METHOD _next_combination.
    ct_field = _rotate90( ct_field ).

    IF iv_index MOD 4 = 0.
      ct_field = _flip( ct_field ).
    ENDIF.
  ENDMETHOD.


  METHOD _rotate90.
    DATA(lv_size) = lines( it_field[] ).
    rt_field = it_field[].

    DO lv_size TIMES.
      DATA(x) = sy-index.
      DO lv_size TIMES.
        DATA(y) = sy-index.

        rt_field[ y ][ lv_size - x + 1 ] = it_field[ x ][ y ].
      ENDDO.
    ENDDO.
  ENDMETHOD.


  METHOD _set_borders.
    DATA(border_top)    = concat_lines_of( table = ir_tile->t_field[ 1 ] ).
    DATA(border_bottom) = concat_lines_of( table = ir_tile->t_field[ 10 ] ).
    DATA(border_left)   = ||.
    DATA(border_right)  = ||.
    DO 10 TIMES.
      ASSIGN ir_tile->t_field[ sy-index ] TO FIELD-SYMBOL(<lv_row>).
      border_left  &&= <lv_row>[ 1 ].
      border_right &&= <lv_row>[ 10 ].
    ENDDO.

    ir_tile->t_borders = VALUE #( ( border_left ) ( border_top ) ( border_right ) ( border_bottom )  ).
  ENDMETHOD.


  METHOD _set_field.
    DO mc_char_count TIMES.
      DATA(x) = sy-index.
      DO mc_char_count TIMES.
        DATA(y) = sy-index.
        mt_all_field[ ( iv_x - 1 ) * mc_char_count + x ][ ( iv_y - 1 ) * mc_char_count + y ] = ir_tile->t_field[ x + 1 ][ y + 1 ].
      ENDDO.
    ENDDO.
  ENDMETHOD.


  METHOD _set_tile.
    ASSIGN mt_tile[ id = iv_tile_id ] TO FIELD-SYMBOL(<ls_tile>).
    ASSERT sy-subrc = 0.
    DATA(lr_tile) = REF #( <ls_tile> ).

    DATA lr_ok_pos TYPE REF TO ts_tile.
    DO mc_combination_count TIMES.
      DATA(lv_index) = sy-index.

      DATA(lv_ok) = COND #( WHEN iv_left IS SUPPLIED THEN xsdbool( <ls_tile>-t_borders[ 1 ] = iv_left )
                            WHEN iv_top  IS SUPPLIED THEN xsdbool( <ls_tile>-t_borders[ 2 ] = iv_top ) ).
      IF lv_ok = abap_true.
        lr_ok_pos = lr_tile.
        EXIT.
      ENDIF.

      _next_combination( EXPORTING iv_index = lv_index
                         CHANGING  ct_field = lr_tile->t_field ).
      _set_borders( lr_tile ).
    ENDDO.
    ASSERT lr_ok_pos IS NOT INITIAL.

    _set_field( ir_tile = lr_ok_pos
                iv_x    = iv_x
                iv_y    = iv_y ).

    " Next move to left
    IF mv_size >= iv_y + 1.
      DATA(lv_right_border) = lr_ok_pos->t_borders[ 3 ].
      _set_tile( iv_tile_id = _get_other_tile_id( iv_tile_id   = iv_tile_id
                                                  iv_border_id = _get_border_id( lv_right_border ) )
                 iv_x       = iv_x
                 iv_y       = iv_y + 1
                 iv_left    = lv_right_border ).
    ENDIF.

    " Next move to bottom
    IF iv_y = 1 AND mv_size >= iv_x + 1.
      DATA(lv_bottom_border) = lr_ok_pos->t_borders[ 4 ].
      _set_tile( iv_tile_id = _get_other_tile_id( iv_tile_id   = iv_tile_id
                                                  iv_border_id = _get_border_id( lv_bottom_border ) )
                 iv_x       = iv_x + 1
                 iv_y       = iv_y
                 iv_top     = lv_bottom_border ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
