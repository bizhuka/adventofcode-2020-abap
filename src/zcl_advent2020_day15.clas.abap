CLASS zcl_advent2020_day15 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS part1
      IMPORTING
        iv_input       TYPE string
        iv_until       TYPE i DEFAULT 2020
      RETURNING
        VALUE(rv_count) TYPE i.
    METHODS part2
      IMPORTING
        iv_input       TYPE string
      RETURNING
        VALUE(rv_count) TYPE i.
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_advent2020_day15 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lv_input) = |1,0,18,10,19,6|.
    DATA(lv_count1) = part1( lv_input ).
    DATA(lv_count2) = part2( lv_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    TYPES:
      BEGIN OF ts_pair,
        key  TYPE i,
        ind1 TYPE i,
        ind2 TYPE i,
      END OF ts_pair,
      tt_pair TYPE HASHED TABLE OF ts_pair WITH UNIQUE KEY key.

    SPLIT iv_input AT ',' INTO TABLE DATA(lt_input).
    DATA(lt_pair) = VALUE tt_pair( FOR lv_num IN lt_input INDEX INTO lv_ind
                                   ( key    = lv_num
                                     ind1   = lv_ind )
                                   ).

    DATA(lv_index) = lines( lt_input ).
    DATA(lv_last)  = CONV i( lt_input[ lv_index ]  ).
    WHILE lv_index < iv_until.
      lv_index = lv_index + 1.

      ASSIGN lt_pair[ key = lv_last ] TO FIELD-SYMBOL(<ls_pair>).
      ASSERT sy-subrc = 0.

      DATA(lv_new_num)  = COND #( WHEN <ls_pair>-ind2 IS INITIAL
                                  THEN 0
                                  ELSE <ls_pair>-ind2 - <ls_pair>-ind1 ).

      ASSIGN lt_pair[ key = lv_new_num ]  TO <ls_pair>.
      IF sy-subrc <> 0.
        INSERT VALUE #( key = lv_new_num ) INTO TABLE lt_pair ASSIGNING <ls_pair>.
      ENDIF.
      lv_last = lv_new_num.

      IF <ls_pair>-ind1 IS INITIAL.
        <ls_pair>-ind1 = lv_index.
      ELSEIF <ls_pair>-ind2 IS INITIAL.
        <ls_pair>-ind2 = lv_index.
      ELSE.
        <ls_pair>-ind1 = <ls_pair>-ind2.
        <ls_pair>-ind2 = lv_index.
      ENDIF.
    ENDWHILE.

    rv_count = lv_last.
  ENDMETHOD.


  METHOD part2.
    rv_count = part1( iv_until = 30000000
                      iv_input = iv_input ).
  ENDMETHOD.
ENDCLASS.
