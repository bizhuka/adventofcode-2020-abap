CLASS zcl_advent2020_day23 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    TYPES:
          char1 TYPE c LENGTH 1,
          char9 TYPE c LENGTH 9,
          num1  TYPE n LENGTH 1,
          int4_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    METHODS part1
      IMPORTING
        !iv_input       TYPE string
        !iv_times       TYPE i
        !iv_final       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_count) TYPE string .
    METHODS part2
      IMPORTING
        !iv_input       TYPE string
        !iv_times       TYPE i
        !iv_move_to     TYPE char1 OPTIONAL
      RETURNING
        VALUE(rv_count) TYPE string .
  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS million TYPE i VALUE 1000000 ##NO_TEXT.
ENDCLASS.



CLASS zcl_advent2020_day23 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lv_input)   = NEW lcl_input(  )->mv_input.
    DATA(lv_count1) = part1( iv_input = lv_input
                             iv_times = 100
                             iv_final = abap_true ).
    DATA(lv_count2) = part2( iv_input = lv_input
                             iv_times = 10 * million  ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    DATA(lv_curr_result) = CONV char9( iv_input ).

    DO iv_times TIMES.
      DATA(lv_pos) = sy-index MOD 9.
      IF lv_pos = 0.
        lv_pos = 9.
      ENDIF.

      DATA(lv_pos_m1) = lv_pos - 1.
      DATA(lv_current_cup) = lv_curr_result+lv_pos_m1(1).

      SHIFT lv_curr_result BY lv_pos PLACES CIRCULAR.
      DATA(lv_move)  = lv_curr_result+0(3).
      lv_curr_result = lv_curr_result+3.

      DATA(lv_number) = CONV num1( lv_current_cup ).
      DO.
        lv_number = lv_number - 1.
        IF lv_number = 0.
          lv_number = 9.
        ENDIF.

        IF lv_move NS lv_number.
          EXIT.
        ENDIF.
      ENDDO.

      ASSERT lv_curr_result CS lv_number.
      DATA(lv_from) = sy-fdpos + 1.

      lv_curr_result = lv_curr_result+0(lv_from) && lv_move && lv_curr_result+lv_from.

      " CIRCULAR SHIFT
      ASSERT lv_curr_result CS lv_current_cup.
      DATA(lv_shift) = sy-fdpos - lv_pos_m1.
      CHECK lv_shift IS NOT INITIAL.
      SHIFT lv_curr_result BY lv_shift PLACES CIRCULAR.
    ENDDO.

    IF iv_final = abap_true.
      SHIFT lv_curr_result UP TO '1' CIRCULAR.
      lv_curr_result = lv_curr_result+1.
    ENDIF.

    rv_count = lv_curr_result.
  ENDMETHOD.


  METHOD part2.
    DATA(lt_node) = VALUE int4_table( ).
    DO strlen( iv_input ) TIMES.
      APPEND INITIAL LINE TO lt_node.
    ENDDO.

    DATA(lv_current_cup) = CONV i( iv_input+0(1) ).
    DO strlen( iv_input ) TIMES.
      DATA(lv_index_m1)   = sy-index - 1.
      DATA(lv_curr_index) = CONV i( iv_input+lv_index_m1(1) ).
      DATA(lv_curr_val)   = COND i( WHEN sy-index = lines( lt_node )
                                    THEN lv_current_cup
                                    ELSE CONV i( iv_input+sy-index(1) ) ).
      lt_node[ lv_curr_index ] = lv_curr_val.
    ENDDO.

    IF iv_times = 10 * million.
      DATA(lv_dx) = lines( lt_node ).
      lt_node[ lv_curr_index ] = lv_dx + 1.
      DO million - lv_dx  TIMES.
        APPEND sy-index + lv_dx + 1 TO lt_node.
      ENDDO.
      lt_node[ lines( lt_node ) ] = lv_current_cup.
    ENDIF.

**********************************************************************
**********************************************************************

    DO iv_times TIMES.
      DATA(lv_splice1) = lt_node[ lv_current_cup ].
      DATA(lv_splice2) = lt_node[ lv_splice1 ].
      DATA(lv_splice3) = lt_node[ lv_splice2 ].

      DATA(lv_next1)    = lt_node[ lv_splice3 ].
      lt_node[ lv_current_cup ] = lv_next1.

      DATA(lv_number) = lv_current_cup.
      DO.
        lv_number = lv_number - 1.
        IF lv_number = 0.
          lv_number = lines( lt_node ).
        ENDIF.

        IF lv_number <> lv_splice1 AND lv_number <> lv_splice2 AND lv_number <> lv_splice3.
          EXIT.
        ENDIF.
      ENDDO.

      lt_node[ lv_splice3 ]  = lt_node[ lv_number  ].
      lt_node[ lv_number  ]  = lv_splice1.

      lv_current_cup = lv_next1.
    ENDDO.

**********************************************************************
**********************************************************************

    IF iv_move_to IS INITIAL.
      DATA(lv_num1) = lt_node[ 1 ].
      rv_count = |{ CONV decfloat34( lv_num1 * lt_node[ lv_num1 ] ) }|.
      RETURN.
    ENDIF.

    DO.
      ASSIGN lt_node[ lv_current_cup ] TO FIELD-SYMBOL(<lv_visited>).
      lv_current_cup = <lv_visited>.

      IF <lv_visited> IS INITIAL.
        EXIT.
      ENDIF.

      rv_count = rv_count && CONV char1( <lv_visited> ).
      <lv_visited> = 0.
    ENDDO.
    SHIFT rv_count UP TO iv_move_to CIRCULAR.
  ENDMETHOD.
ENDCLASS.
