CLASS zcl_advent2020_day03 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS count_tree_1 IMPORTING iv_col_offset   TYPE i DEFAULT 3
                                   iv_row_offset   TYPE i DEFAULT 1
                         RETURNING VALUE(rv_count) TYPE i.

    METHODS set_tree_pos
      IMPORTING it_filed           TYPE stringtab.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_tree_pos,
             col  TYPE i,
             row  TYPE i,
           END OF ts_tree_pos,
           tt_tree_pos TYPE SORTED TABLE OF ts_tree_pos WITH UNIQUE KEY table_line.

   DATA mt_tree_pos  TYPE tt_tree_pos.
   DATA mv_col_count TYPE i.
   DATA mv_row_count TYPE i.

ENDCLASS.



CLASS zcl_advent2020_day03 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    set_tree_pos( NEW lcl_input(  )->mt_input ).

    DATA(lv_total_tree_count_1) = count_tree_1(  ).

    DATA(r2) = count_tree_1(
        iv_col_offset = 1
        iv_row_offset = 1 ).

    DATA(r3) = count_tree_1(
        iv_col_offset = 5
        iv_row_offset = 1 ).

    DATA(r4) = count_tree_1(
         iv_col_offset = 7
         iv_row_offset = 1 ).

    DATA(r5) = count_tree_1(
        iv_col_offset = 1
        iv_row_offset = 2 ).

    out->write( |{ lv_total_tree_count_1 } - { lv_total_tree_count_1 * r2 * r3 * r4 * r5 }| ).
  ENDMETHOD.


  METHOD count_tree_1.
    DATA(lv_curent_col) = 1.
    DATA(lv_curent_row) = 1.

    WHILE mv_row_count >= lv_curent_row.
      lv_curent_col = ( lv_curent_col + iv_col_offset ) MOD mv_col_count.
      IF lv_curent_col = 0.
        lv_curent_col = mv_col_count.
      ENDIF.
      lv_curent_row = lv_curent_row + iv_row_offset.

      ASSIGN mt_tree_pos[ col = lv_curent_col
                          row = lv_curent_row ] TO FIELD-SYMBOL(<ls_tree_pos>).
      CHECK sy-subrc = 0.
      rv_count = rv_count + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD set_tree_pos.
    mt_tree_pos   = VALUE #( ).
    mv_row_count  = lines( it_filed ).
    CHECK mv_row_count > 0.
    mv_col_count  = strlen( it_filed[ mv_row_count ] ).

    LOOP AT it_filed INTO DATA(lv_field).
      DATA(lv_row) = sy-tabix.
      DO strlen( lv_field ) TIMES.
        DATA(lv_index) = sy-index - 1.
        CHECK lv_field+lv_index(1) = '#'.
        INSERT VALUE #( col  = sy-index
                        row  = lv_row ) INTO TABLE mt_tree_pos.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
