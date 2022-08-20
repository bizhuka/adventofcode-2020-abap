CLASS zcl_advent2020_day08 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS part1
      IMPORTING
        !it_input      TYPE stringtab
      RETURNING
        VALUE(rv_count) TYPE i .
    METHODS part2
      IMPORTING
        !it_input      TYPE stringtab
      RETURNING
        VALUE(rv_count) TYPE i .
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_rule,
        cmd    TYPE char3,
        number TYPE i,
      END OF ts_rule .
    TYPES:
      tt_rule TYPE STANDARD TABLE OF ts_rule WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_state,
        counter TYPE i,
        stack   TYPE SORTED TABLE OF sy-tabix WITH UNIQUE KEY table_line,
        ok_line TYPE i,
      END OF ts_state .

    DATA t_rule TYPE tt_rule .
    DATA s_state TYPE ts_state .

    METHODS _make_rules
      IMPORTING
        !it_input      TYPE stringtab
      RETURNING
        VALUE(rt_rule) TYPE tt_rule .
    METHODS _next
      IMPORTING
        !iv_index        TYPE i
        !iv_repair       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_stoped) TYPE abap_bool .
ENDCLASS.



CLASS zcl_advent2020_day08 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_next_1) = part1( lt_input ).
    DATA(lv_next_2) = part2( lt_input ).

    CHECK out IS NOT INITIAL.
    out->write( |{ lv_next_1 } - { lv_next_2 }| ).
  ENDMETHOD.


  METHOD part1.
    t_rule = _make_rules( it_input ).
    _next( iv_index = 1 ).
    rv_count = s_state-counter.
  ENDMETHOD.


  METHOD part2.
    t_rule = _make_rules( it_input ).

    CLEAR s_state.
    DATA(lv_stoped) = _next( iv_index  = 1
                             iv_repair = abap_true ).
    CHECK lv_stoped <> abap_true.

    ASSIGN t_rule[ s_state-ok_line ] TO FIELD-SYMBOL(<ls_rule>).
    CHECK sy-subrc = 0.

    <ls_rule>-cmd = SWITCH #( <ls_rule>-cmd WHEN 'nop' THEN 'jmp'
                                            WHEN 'jmp' THEN 'nop' ).

    CLEAR s_state.
    lv_stoped = _next( iv_index = 1 ).
    CHECK lv_stoped <> abap_true.
    rv_count = s_state-counter.
  ENDMETHOD.


  METHOD _make_rules.
    LOOP AT it_input INTO DATA(lv_input).
      DATA(lv_sign) = SWITCH #( lv_input+4(1) WHEN '+' THEN 1
                                              WHEN '-' THEN -1
                                              ELSE 0 ).
      APPEND VALUE #( cmd    = lv_input+0(3)
                      number = lv_input+5 * lv_sign ) TO rt_rule.
    ENDLOOP.
  ENDMETHOD.


  METHOD _next.
    ASSIGN t_rule[ iv_index ] TO FIELD-SYMBOL(<ls_rule>).
    CHECK sy-subrc = 0.

    DATA(lv_ok) = abap_false.
    INSERT iv_index INTO TABLE s_state-stack.
    IF sy-subrc <> 0.
      rv_stoped = abap_true.
      RETURN.
    ENDIF.

    CASE <ls_rule>-cmd.
      WHEN 'nop'.
        rv_stoped = _next( iv_index  = iv_index + 1
                           iv_repair = iv_repair ). " just go on

        IF iv_repair = abap_true AND rv_stoped = abap_true.
          rv_stoped = _next( iv_index = iv_index + <ls_rule>-number ).
          lv_ok     = xsdbool( rv_stoped <> abap_true ).
        ENDIF.

      WHEN 'acc'.
        s_state-counter = s_state-counter + <ls_rule>-number.
        rv_stoped = _next( iv_index  = iv_index + 1
                           iv_repair = iv_repair ).

      WHEN 'jmp'.
        rv_stoped = _next( iv_index  = iv_index + <ls_rule>-number
                           iv_repair = iv_repair ).

        IF iv_repair = abap_true AND rv_stoped = abap_true.
          rv_stoped = _next( iv_index  = iv_index + 1 ).
          lv_ok     = xsdbool( rv_stoped <> abap_true ).
        ENDIF.

      WHEN OTHERS.
        MESSAGE 'Oops' TYPE 'X'.
    ENDCASE.

    CHECK lv_ok = abap_true.
    s_state-ok_line = iv_index.
  ENDMETHOD.
ENDCLASS.
