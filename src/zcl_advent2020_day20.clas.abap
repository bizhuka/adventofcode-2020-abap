class ZCL_ADVENT2020_DAY20 definition
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
  class-methods GET_TOTAL_FIT_COUNT
    importing
      !IV_TILE_COUNT type I
    returning
      value(RV_FIT_COUNT) type I .
  PROTECTED SECTION.

private section.

  data MT_TILE type TT_TILE .

  methods READ_TILES
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RT_TILE) type TT_TILE .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY20 IMPLEMENTATION.


  METHOD get_total_fit_count.
    DATA(lv_zise) = sqrt( iv_tile_count ).

    CHECK iv_tile_count >= 2.
    DATA(lv_used2) = 4.
    rv_fit_count  = 2 * lv_used2.

    CHECK iv_tile_count >= 3.
    DATA(lv_used3) = ( lv_zise  - 3 + 1 ) * 4.
    rv_fit_count = rv_fit_count + 3 * lv_used3.

    rv_fit_count = rv_fit_count +  4 * ( iv_tile_count - lv_used2 -  lv_used3 ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( lt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    mt_tile = read_tiles( it_input ).
    DATA(lv_total_fit_count) = get_total_fit_count( lines( mt_tile ) ).

    " insert blank items
    DATA(lt_match) = VALUE tt_match( FOR i = 0 THEN i + 1 WHILE i < 8 * lines( mt_tile ) ( ) ).

    LOOP AT mt_tile INTO DATA(lo_tile).
      lo_tile->paste_next_combination( CHANGING ct_match = lt_match ).
    ENDLOOP.

    LOOP AT lt_match ASSIGNING FIELD-SYMBOL(<ls_match>) USING KEY _ordered
                     GROUP BY  ( border = <ls_match>-border
                                 size  = GROUP SIZE )
                      REFERENCE INTO DATA(route_group).
      CHECK route_group->size > 1 AND route_group->size MOD 2 = 0.
      LOOP AT GROUP route_group ASSIGNING FIELD-SYMBOL(<ls_match_ok>).
        ADD 1 TO <ls_match_ok>-tile->fit_count.
      ENDLOOP.
    ENDLOOP.

*    WRITE / '-------------'.
*    LOOP AT mt_tile INTO lo_tile.
*      WRITE : / sy-tabix, lo_tile->id, lo_tile->fit_count.
*    ENDLOOP.

    rv_count = REDUCE #( INIT m = CONV decfloat34( 1 )
                         FOR tile IN mt_tile
                         WHERE ( table_line->fit_count = 4 )
                         NEXT m = m * tile->id ).
  ENDMETHOD.


  METHOD part2.
  ENDMETHOD.


  METHOD read_tiles.
    DATA(lv_count) = lines( it_input ) + 1.
    ASSERT lv_count MOD 12 = 0.

    DO lv_count DIV 12 TIMES.
      DATA(lv_index) = sy-index.
      DATA(lv_from)  = ( lv_index - 1 ) * 12 + 1.

      DATA(lt_array) = VALUE stringtab( ).
      LOOP AT it_input INTO DATA(lv_row) FROM lv_from TO lv_from + 10.
        APPEND lv_row TO lt_array[].
      ENDLOOP.

      APPEND NEW lcl_tile( iv_index = lv_index
                           it_array = lt_array[] ) TO rt_tile[].
    ENDDO.
  ENDMETHOD.
ENDCLASS.
