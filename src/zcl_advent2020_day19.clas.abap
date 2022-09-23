CLASS zcl_advent2020_day19 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    TYPES:
      BEGIN OF ts_rule,
        key   TYPE string,
        val   TYPE string,
        regex TYPE string,
      END OF ts_rule.
    TYPES:
      tt_rule TYPE HASHED TABLE OF ts_rule WITH UNIQUE KEY key.

    METHODS part1
      IMPORTING
        it_input        TYPE string_table
        iv_replace      TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_count) TYPE decfloat34.

    METHODS part2
      IMPORTING
        it_input        TYPE string_table
        iv_replace      TYPE abap_bool
      RETURNING
        VALUE(rv_count) TYPE decfloat34.

    METHODS get_rule
      IMPORTING
        iv_key          TYPE csequence
        iv_depth        TYPE i DEFAULT 20
      RETURNING
        VALUE(rv_regex) TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mt_rule TYPE tt_rule.
    DATA mt_check TYPE string_table.

    METHODS parse
      IMPORTING
        it_input TYPE string_table.
ENDCLASS.



CLASS zcl_advent2020_day19 IMPLEMENTATION.
  METHOD get_rule.
    IF iv_depth = 0.
      rv_regex = ''.
      RETURN.
    ENDIF.

    ASSIGN mt_rule[ key = iv_key ] TO FIELD-SYMBOL(<ls_rule>).
    IF sy-subrc = 0 AND <ls_rule>-regex IS NOT INITIAL.
      rv_regex = <ls_rule>-regex.
      RETURN.
    ENDIF.

    INSERT VALUE #( key = iv_key
                    val = iv_key )  INTO TABLE mt_rule[] ASSIGNING <ls_rule>.

**********************************************************************
**********************************************************************
    IF <ls_rule>-val CP '"*"'.
      <ls_rule>-regex = <ls_rule>-val+1(1).
    ELSEIF <ls_rule>-val CP `* | *`.
      SPLIT <ls_rule>-val AT ` | ` INTO DATA(lv_keys1)
                                        DATA(lv_keys2).

      DATA(lv_part1) = get_rule( iv_key = lv_keys1 iv_depth = iv_depth - 1 ).
      DATA(lv_part2) = get_rule( iv_key = lv_keys2 iv_depth = iv_depth - 1 ).

      <ls_rule>-regex = `((` && lv_part1 && `)|(` && lv_part2 && `))`.
    ELSEIF <ls_rule>-val CS ` `.
      SPLIT <ls_rule>-val AT ` ` INTO TABLE DATA(lt_key).
      LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<lv_key>).
        <lv_key> = get_rule( iv_key = <lv_key> iv_depth = iv_depth - 1 ).
      ENDLOOP.
      <ls_rule>-regex = |{ concat_lines_of( table = lt_key ) }|.
    ELSE.
      <ls_rule>-regex = get_rule( iv_key = <ls_rule>-val iv_depth = iv_depth - 1 ).
    ENDIF.

    rv_regex = <ls_rule>-regex.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( it_input   = lt_input
                             iv_replace = abap_true ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.

  METHOD parse.
    mt_rule  = VALUE #( ).
    mt_check = VALUE #( ).
    LOOP AT it_input INTO DATA(lv_input) WHERE table_line IS NOT INITIAL.
      SPLIT lv_input AT `: ` INTO DATA(lv_key)
                                  DATA(lv_value).
      IF lv_value IS NOT INITIAL.
        INSERT VALUE #( key     = lv_key
                        val     = lv_value ) INTO TABLE mt_rule.
        CONTINUE.
      ENDIF.

      APPEND lv_input TO mt_check.
    ENDLOOP.
  ENDMETHOD.


  METHOD part1.
    parse( it_input ).

    IF iv_replace = abap_true.
*      get_rule( '42' ).
*      get_rule( '31' ).

      mt_rule[ key = `8` ]-val  = `42 | 42 8`.
*      get_rule( '8' ).

      mt_rule[ key = `11` ]-val = `42 31 | 42 11 31`.
*      get_rule( '11' ).
    ENDIF.


    DATA(lv_regex) = |^{ get_rule( `0` ) }$|.

    LOOP AT mt_check INTO DATA(lv_line).
      FIND REGEX lv_regex IN lv_line.
      CHECK sy-subrc = 0.

      rv_count += 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD part2.
    rv_count = part1( it_input   = it_input
                      iv_replace = iv_replace ).
  ENDMETHOD.
ENDCLASS.
