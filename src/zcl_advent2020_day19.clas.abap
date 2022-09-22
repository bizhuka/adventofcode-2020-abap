class ZCL_ADVENT2020_DAY19 definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  types:
    BEGIN OF ts_rule,
        key   TYPE string,
        val   TYPE string,

        cache01 TYPE stringtab,

        regex02 TYPE STRING,
      END OF ts_rule .
  types:
    tt_rule TYPE HASHED TABLE OF ts_rule WITH UNIQUE KEY key .

  methods PART1
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_COUNT) type DECFLOAT34 .
  methods PART2
    importing
      !IT_INPUT type STRINGTAB
      !IV_REPLACE type ABAP_BOOL
    returning
      value(RV_COUNT) type DECFLOAT34 .
  methods GET_RULE_01
    importing
      !IV_KEY type CSEQUENCE
    returning
      value(RT_RULE) type STRINGTAB .
  PROTECTED SECTION.

private section.

  data MT_RULE type TT_RULE .
  data MT_CHECK type STRINGTAB .

  methods PARSE_01
    importing
      !IT_INPUT type STRINGTAB .
  methods GET_RULE_02
    importing
      !IV_KEY type CSEQUENCE
      !IV_DEPTH type I default 20
    returning
      value(RV_REGEX) type STRING .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY19 IMPLEMENTATION.


  METHOD get_rule_01.
    ASSIGN mt_rule[ key = iv_key ] TO FIELD-SYMBOL(<ls_rule>).
    IF sy-subrc = 0 AND <ls_rule>-cache01[] IS NOT INITIAL.
      rt_rule = <ls_rule>-cache01[].
      RETURN.
    ENDIF.

    INSERT VALUE #( key = iv_key )  INTO TABLE mt_rule[] ASSIGNING <ls_rule>.

**********************************************************************
**********************************************************************
    SPLIT iv_key AT ` ` INTO DATA(lv_key1)
                             DATA(lv_key2)
                             DATA(lv_key3).
    IF lv_key2 IS NOT INITIAL.
      DATA(lt_val1) = get_rule_01( lv_key1 ).
      DATA(lt_val2) = get_rule_01( lv_key2 ).
      DATA(lt_val3) = COND #( WHEN lv_key3 IS NOT INITIAL THEN get_rule_01( lv_key3 )
                                                          ELSE VALUE #( ( ) ) ).
      LOOP AT lt_val1 INTO DATA(lv_val1).
        LOOP AT lt_val2 INTO DATA(lv_val2).
          LOOP AT lt_val3 INTO DATA(lv_val3).
            APPEND |{ lv_val1 }{ lv_val2 }{ lv_val3 }| TO rt_rule[].
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.

    ELSEIF <ls_rule>-val CP `* | *`.
      SPLIT <ls_rule>-val AT ` | ` INTO DATA(lv_keys1)
                                        DATA(lv_keys2).

      DATA(lt_vals1) = get_rule_01( lv_keys1 ).
      DATA(lt_vals2) = get_rule_01( lv_keys2 ).

      APPEND LINES OF lt_vals1 TO rt_rule[].
      APPEND LINES OF lt_vals2 TO rt_rule[].
    ELSE.
      rt_rule = get_rule_01( <ls_rule>-val ).
    ENDIF.

    <ls_rule>-cache01[] = rt_rule[].
  ENDMETHOD.


  METHOD get_rule_02.
    IF iv_depth = 0.
      rv_regex = ''.
      RETURN.
    ENDIF.

    ASSIGN mt_rule[ key = iv_key ] TO FIELD-SYMBOL(<ls_rule>).
    IF sy-subrc = 0 AND <ls_rule>-regex02 IS NOT INITIAL.
      rv_regex = <ls_rule>-regex02.
      RETURN.
    ENDIF.

    INSERT VALUE #( key = iv_key )  INTO TABLE mt_rule[] ASSIGNING <ls_rule>.

**********************************************************************
**********************************************************************
    SPLIT iv_key AT ` ` INTO DATA(lv_key1)
                             DATA(lv_key2)
                             DATA(lv_key3).
    IF lv_key2 IS NOT INITIAL.
      DATA(lv_regex1) = get_rule_02( iv_key = lv_key1 iv_depth = iv_depth - 1 ).
      DATA(lv_regex2) = get_rule_02( iv_key = lv_key2 iv_depth = iv_depth - 1 ).
      DATA(lv_regex3) = COND #( WHEN lv_key3 IS NOT INITIAL THEN get_rule_02( iv_key = lv_key3 iv_depth = iv_depth - 1  )
                                                            ELSE || ).
      rv_regex = |{ lv_regex1 }{ lv_regex2 }{ lv_regex3 }|.

    ELSEIF <ls_rule>-val CP `* | *`.
      SPLIT <ls_rule>-val AT ` | ` INTO DATA(lv_keys1)
                                        DATA(lv_keys2).

      DATA(lv_part1) = get_rule_02( iv_key = lv_keys1 iv_depth = iv_depth - 1 ).
      DATA(lv_part2) = get_rule_02( iv_key = lv_keys2 iv_depth = iv_depth - 1 ).

      rv_regex = `((` && lv_part1 && `)|(` && lv_part2 && `))`.
    ELSE.
      rv_regex = get_rule_02( iv_key = <ls_rule>-val iv_depth = iv_depth - 1 ).
    ENDIF.

    <ls_rule>-regex02 = rv_regex.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( it_input   = lt_input
                             iv_replace = abap_true ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD parse_01.
    mt_rule  = VALUE #( ).
    mt_check = VALUE #( ).
    LOOP AT it_input INTO DATA(lv_input) WHERE table_line IS NOT INITIAL.
      SPLIT lv_input AT `: ` INTO DATA(lv_key)
                                  DATA(lv_value).
      IF lv_value IS NOT INITIAL.
        INSERT VALUE #( key     = lv_key
                        val     = lv_value
                        cache01 = COND #( WHEN lv_value CP `"*"` THEN VALUE #( ( |{ lv_value+1(1) }| ) ) )  ) INTO TABLE mt_rule.
        CONTINUE.
      ENDIF.

      APPEND lv_input TO mt_check.
    ENDLOOP.
  ENDMETHOD.


  METHOD part1.
    parse_01( it_input ).

    DATA(lt_all) = get_rule_01( `0` ).
    SORT lt_all BY table_line.

    LOOP AT mt_check INTO DATA(lv_line).
      READ TABLE lt_all TRANSPORTING NO FIELDS BINARY SEARCH
       WITH KEY table_line = lv_line.
      CHECK sy-subrc = 0.

      rv_count = rv_count + 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD part2.
    mt_rule  = VALUE #( ).
    mt_check = VALUE #( ).
    LOOP AT it_input INTO DATA(lv_input) WHERE table_line IS NOT INITIAL.
      SPLIT lv_input AT `: ` INTO DATA(lv_key)
                                  DATA(lv_value).
      IF lv_value IS NOT INITIAL.
        INSERT VALUE #( key     = lv_key
                        val     = lv_value
                        regex02 = COND #( WHEN lv_value CP `"*"` THEN |({ lv_value+1(1) })| ) ) INTO TABLE mt_rule.
        CONTINUE.
      ENDIF.

      APPEND lv_input TO mt_check.
    ENDLOOP.
**********************************************************************
**********************************************************************

    IF iv_replace = abap_true.
      get_rule_02( '42' ).
      get_rule_02( '31' ).


      BREAK-POINT.
      mt_rule[ key = `8` ]-val  = `42 | 42 8`.
      get_rule_02( '8' ).

      mt_rule[ key = `11` ]-val = `42 31 | 42 11 31`.
      get_rule_02( '11' ).
    ENDIF.

**********************************************************************
**********************************************************************
    DATA(lv_regex) = |^{ get_rule_02( `0` ) }$|.

    LOOP AT mt_check INTO DATA(lv_line).
      FIND REGEX lv_regex IN lv_line.
      CHECK sy-subrc = 0.

      rv_count = rv_count + 1.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
