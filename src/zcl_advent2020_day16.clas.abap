CLASS zcl_advent2020_day16 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES:
      int4_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
      BEGIN OF ts_note,
        name     TYPE string,
        diap     TYPE RANGE OF i,
        t_column TYPE int4_table,
        column   TYPE i,
      END OF ts_note .
    TYPES:
      tt_note           TYPE STANDARD TABLE OF ts_note WITH DEFAULT KEY .
    TYPES:
      tt_correct_ticket TYPE STANDARD TABLE OF int4_table .

    METHODS part1
      IMPORTING
        !it_input       TYPE string_table
      RETURNING
        VALUE(rv_count) TYPE i .
    METHODS part2
      IMPORTING
        !it_input       TYPE string_table
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mt_note TYPE tt_note .
    DATA mt_correct_ticket TYPE tt_correct_ticket .
ENDCLASS.



CLASS zcl_advent2020_day16 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( it_input = lt_input ).
    DATA(lv_count2) = part2( it_input = lt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    mt_note = VALUE tt_note( ).
    DATA(lv_mode) = 0.
    LOOP AT it_input INTO DATA(lv_input) WHERE table_line IS NOT INITIAL.
      CASE lv_input.
        WHEN `your ticket:`.
          lv_mode = 1.
          CONTINUE.
        WHEN `nearby tickets:`.
          lv_mode = 2.
          CONTINUE.
      ENDCASE.

      CASE lv_mode.
        WHEN 0.
          SPLIT lv_input AT `: ` INTO DATA(lv_name)
                                      DATA(lv_range).
          APPEND VALUE #( name = lv_name ) TO mt_note ASSIGNING FIELD-SYMBOL(<ls_note>).
          SPLIT lv_range AT ` or ` INTO TABLE DATA(lt_range).
          LOOP AT lt_range INTO lv_range.
            SPLIT lv_range AT `-` INTO DATA(lv_low) DATA(lv_high).
            APPEND VALUE #( sign = 'I' option = 'BT' low = lv_low high = lv_high ) TO <ls_note>-diap[].
          ENDLOOP.

        WHEN 1.
          SPLIT lv_input AT `,` INTO TABLE DATA(lt_number).
          APPEND CORRESPONDING #( lt_number ) TO mt_correct_ticket.

        WHEN 2.
          SPLIT lv_input AT `,` INTO TABLE lt_number.

          DATA(lv_ok) = abap_true.
          LOOP AT lt_number INTO DATA(lv_number).
            DATA(lv_no_count) = 0.

            LOOP AT mt_note ASSIGNING <ls_note>.
              IF lv_number NOT IN <ls_note>-diap[].
                lv_no_count = lv_no_count + 1.
              ENDIF.
            ENDLOOP.

            CHECK lv_no_count = lines( mt_note ).
            rv_count = rv_count + lv_number.
            lv_ok = abap_false.
          ENDLOOP.

          CHECK lv_ok = abap_true.
          APPEND CORRESPONDING #( lt_number ) TO mt_correct_ticket[].
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD part2.
    " part1( it_input ).

    DATA(lv_column_count) = lines( mt_note ).
    LOOP AT mt_note ASSIGNING FIELD-SYMBOL(<ls_note>).
      DO lv_column_count TIMES.
        DATA(lv_column) = sy-index.

        DATA(lv_ok) = abap_true.
        LOOP AT mt_correct_ticket ASSIGNING FIELD-SYMBOL(<lt_number>).
          DATA(lv_number) = <lt_number>[ lv_column ].
          CHECK lv_number NOT IN <ls_note>-diap[].
          lv_ok = abap_false.
          EXIT.
        ENDLOOP.
        CHECK lv_ok = abap_true.

        APPEND lv_column TO <ls_note>-t_column.
      ENDDO.
    ENDLOOP.

**********************************************************************
**********************************************************************

    DATA(lv_changed) = abap_true.
    WHILE lv_changed = abap_true.
      lv_changed = abap_false.

      LOOP AT mt_note ASSIGNING <ls_note> WHERE column IS INITIAL.
        CHECK lines( <ls_note>-t_column[] ) = 1.
        <ls_note>-column = <ls_note>-t_column[ 1 ].

        lv_changed = abap_true.

        LOOP AT mt_note ASSIGNING FIELD-SYMBOL(<ls_note_exclude>) WHERE column IS INITIAL.
          CHECK lines( <ls_note_exclude>-t_column[] ) > 1.

          READ TABLE <ls_note_exclude>-t_column TRANSPORTING NO FIELDS
           WITH KEY table_line = <ls_note>-column.
          CHECK sy-subrc = 0.

          DELETE <ls_note_exclude>-t_column INDEX sy-tabix.
        ENDLOOP.

      ENDLOOP.
    ENDWHILE.

**********************************************************************
**********************************************************************

    ASSIGN mt_correct_ticket[ 1 ] TO FIELD-SYMBOL(<lt_my_ticket>).
    ASSERT sy-subrc = 0.

    rv_count = REDUCE decfloat34( INIT m = CONV decfloat34( 1 )
                                  FOR <ls_dep_note> IN mt_note
                                  WHERE ( name CP 'departure*' )
                                  NEXT m = m * <lt_my_ticket>[  <ls_dep_note>-column ] ).
  ENDMETHOD.
ENDCLASS.
