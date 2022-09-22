CLASS zcl_advent2020_day18 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES: BEGIN OF ts_oper,
             ord  TYPE i,
             oper TYPE char1,
           END OF ts_oper,
           tt_oper TYPE SORTED TABLE OF ts_oper WITH UNIQUE KEY ord,

           tt_sign TYPE STANDARD TABLE OF char1 WITH DEFAULT KEY,

           BEGIN OF ts_queue,
             number TYPE decfloat34,
             oper   TYPE char1,
           END OF ts_queue,
           tt_queue TYPE STANDARD TABLE OF ts_queue WITH DEFAULT KEY.

    DATA mt_queue TYPE tt_queue.
    METHODS part1
      IMPORTING
        !it_input       TYPE stringtab
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
    METHODS part2
      IMPORTING
        !it_input       TYPE stringtab
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_mode TYPE i VALUE 1 ##NO_TEXT.

    METHODS _do_calc
      IMPORTING
        !iv_input        TYPE string
      RETURNING
        VALUE(rv_result) TYPE decfloat34 .
    METHODS _get_last_exp
      IMPORTING
        !iv_input      TYPE string
      EXPORTING
        !ev_expression TYPE string
        !ev_offset     TYPE i
        !ev_length     TYPE i .
    METHODS _get_queue_index
      RETURNING
        VALUE(rv_index) TYPE i .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY18 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( lt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    LOOP AT it_input INTO DATA(lv_input).
      lv_input = replace( val = lv_input sub = ` ` with = `` occ = 0 ).

      DATA(lv_result) = _do_calc( lv_input ).
      rv_count = rv_count + lv_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD part2.
    mv_mode = 2.
    rv_count = part1( it_input ).
  ENDMETHOD.


  METHOD _do_calc.
    DATA(lv_input) = iv_input.

    WHILE lv_input CS `(`.
      _get_last_exp( EXPORTING iv_input      = lv_input
                     IMPORTING ev_expression = DATA(lv_in_parentheses)
                               ev_offset     = DATA(lv_offset)
                               ev_length     = DATA(lv_length) ).
      DATA(lv_sub_calc) = _do_calc( lv_in_parentheses ).
      REPLACE SECTION OFFSET lv_offset LENGTH lv_length OF lv_input WITH |{ lv_sub_calc }|.
    ENDWHILE.

**********************************************************************
**********************************************************************
    DATA(lv_copy) = lv_input.
    REPLACE ALL OCCURRENCES OF REGEX `[\+\*]` IN lv_input WITH `~`
       RESULTS DATA(lt_match).

    DATA(lt_oper) = VALUE tt_oper( FOR <ls_match> IN lt_match
     ( ord  = <ls_match>-offset
       oper = lv_copy+<ls_match>-offset(1)  ) ).

    SPLIT lv_input AT `~` INTO TABLE DATA(lt_numbers).
**********************************************************************
**********************************************************************
    ASSERT lines( lt_numbers ) = lines( lt_oper ) + 1.

    mt_queue = VALUE #( ).
    LOOP AT lt_numbers INTO DATA(lv_number).
      ASSIGN lt_oper[ sy-tabix ] TO FIELD-SYMBOL(<ls_oper>).
      DATA(lv_oper) = COND #( WHEN sy-subrc = 0 THEN <ls_oper>-oper ).

      APPEND VALUE #( number = lv_number
                      oper   = lv_oper ) TO mt_queue[].
    ENDLOOP.

    WHILE lines( mt_queue[] ) > 1.
      DATA(lv_index) = _get_queue_index( ).

      ASSIGN mt_queue[ lv_index ] TO FIELD-SYMBOL(<ls_oper1>).

      lv_index = lv_index + 1.
      ASSIGN mt_queue[ lv_index ] TO FIELD-SYMBOL(<ls_oper2>).

      CASE <ls_oper1>-oper.
        WHEN '*'.
          <ls_oper1>-number = <ls_oper1>-number * <ls_oper2>-number.

        WHEN '+'.
          <ls_oper1>-number = <ls_oper1>-number + <ls_oper2>-number.

        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      <ls_oper1>-oper = <ls_oper2>-oper.
      DELETE mt_queue INDEX lv_index.
    ENDWHILE.

    rv_result = mt_queue[ 1 ]-number.
  ENDMETHOD.


  METHOD _get_last_exp.
    CLEAR: ev_expression,
           ev_offset,
           ev_length.
    FIND ALL OCCURRENCES OF REGEX `[\(\)]` IN iv_input RESULTS DATA(lt_result).

    DATA(lv_ind) = lines( lt_result[] ).
    WHILE lv_ind >= 1.
      lv_ind = lv_ind - 1.
      ASSIGN lt_result[ lv_ind     ] TO FIELD-SYMBOL(<ls_open>).
      ASSIGN lt_result[ lv_ind + 1 ] TO FIELD-SYMBOL(<ls_close>).
      CHECK iv_input+<ls_open>-offset(1)  = '('
        AND iv_input+<ls_close>-offset(1) = ')'.

      ev_offset       = <ls_open>-offset + 1.
      ev_length       = <ls_close>-offset - ev_offset.

      " within ( )
      ev_expression   = iv_input+ev_offset(ev_length).

      " replace entite expression with ()
      ev_offset  = ev_offset - 1.
      ev_length  = ev_length + 2.

      RETURN.
    ENDWHILE.
  ENDMETHOD.


  METHOD _get_queue_index.
    CASE mv_mode.
      WHEN 1.
        rv_index = 1.

      WHEN 2.
        READ TABLE mt_queue TRANSPORTING NO FIELDS
         WITH KEY oper = '+'.
        rv_index = COND #( WHEN sy-subrc = 0 THEN sy-tabix ELSE 1 ).

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
