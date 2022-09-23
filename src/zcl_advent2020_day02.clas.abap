CLASS zcl_advent2020_day02 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS:
      is_valid
        IMPORTING
                  iv_password  TYPE string
                  iv_rule      TYPE string
        RETURNING VALUE(rv_ok) TYPE abap_bool,

      is_valid_pos
        IMPORTING
                  iv_password  TYPE string
                  iv_rule      TYPE string
        RETURNING VALUE(rv_ok) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES char1 TYPE c LENGTH 1.
    METHODS _split_rule IMPORTING iv_rule TYPE string
                        EXPORTING ev_char TYPE char1
                                  ev_num1 TYPE i
                                  ev_num2 TYPE i.
ENDCLASS.



CLASS zcl_advent2020_day02 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lv_total_valid)     = 0.
    DATA(lv_total_valid_pos) = 0.
    LOOP AT NEW lcl_input( )->mt_input INTO DATA(lv_pair).
      SPLIT lv_pair AT |: | INTO DATA(lv_rule)
                                 DATA(lv_password).

      IF is_valid( iv_password = lv_password
                   iv_rule     = lv_rule ) = abap_true.
        lv_total_valid = lv_total_valid + 1.
      ENDIF.

      IF is_valid_pos( iv_password = lv_password
                   iv_rule     = lv_rule ) = abap_true.
        lv_total_valid_pos = lv_total_valid_pos + 1.
      ENDIF.
    ENDLOOP.

    out->write( |{ lv_total_valid } - { lv_total_valid_pos }| ).
  ENDMETHOD.

  METHOD is_valid_pos.
    _split_rule( EXPORTING iv_rule = iv_rule
                 IMPORTING ev_char = DATA(lv_char)
                           ev_num1 = DATA(lv_pos1)
                           ev_num2 = DATA(lv_pos2) ).
    DATA(lv_count) = 0.
    lv_pos1 = lv_pos1 - 1.
    IF iv_password+lv_pos1(1) = lv_char.
      lv_count = lv_count + 1.
    ENDIF.

    lv_pos2 = lv_pos2 - 1.
    IF iv_password+lv_pos2(1) = lv_char.
      lv_count = lv_count + 1.
    ENDIF.

    rv_ok = xsdbool( lv_count = 1 ).
  ENDMETHOD.

  METHOD _split_rule.
    SPLIT:
     iv_rule  AT | | INTO DATA(lv_range)
                          ev_char,
     lv_range AT |-| INTO DATA(lv_num1)
                          DATA(lv_num2).
    ev_num1 = lv_num1.
    ev_num2 = lv_num2.
  ENDMETHOD.

  METHOD is_valid.
    TYPES: BEGIN OF ts_cache,
             char  TYPE char1,
             count TYPE i,
           END OF ts_cache,
           tt_cache TYPE SORTED TABLE OF ts_cache WITH UNIQUE KEY char.
    DATA(lt_cache) = VALUE tt_cache( ).

    DO strlen( iv_password ) TIMES.
      DATA(lv_index) = sy-index - 1.
      DATA(lv_char)  = CONV char1( iv_password+lv_index(1) ).

      ASSIGN lt_cache[ char = lv_char ] TO FIELD-SYMBOL(<ls_cache>).
      IF sy-subrc <> 0.
        INSERT VALUE #( char = lv_char ) INTO TABLE lt_cache ASSIGNING <ls_cache>.
      ENDIF.

      <ls_cache>-count = <ls_cache>-count + 1.
    ENDDO.

**********************************************************************
    _split_rule( EXPORTING iv_rule = iv_rule
                 IMPORTING ev_char = lv_char
                           ev_num1 = DATA(lv_from)
                           ev_num2 = DATA(lv_to) ).

    ASSIGN lt_cache[ char = lv_char ] TO <ls_cache>.
    DATA(lv_count) = COND #( WHEN sy-subrc = 0
                             THEN <ls_cache>-count ).

    rv_ok = xsdbool( lv_count BETWEEN lv_from AND lv_to ).
  ENDMETHOD.

ENDCLASS.
