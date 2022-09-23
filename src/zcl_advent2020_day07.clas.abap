CLASS zcl_advent2020_day07 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS part1 IMPORTING it_input        TYPE string_table
                  RETURNING VALUE(rv_count) TYPE i.

    METHODS part2 IMPORTING it_input        TYPE string_table
                  RETURNING VALUE(rv_count) TYPE i.

    TYPES: BEGIN OF ts_sub,
             count TYPE i,
             bag   TYPE string,
           END OF ts_sub,
           tt_sub TYPE STANDARD TABLE OF ts_sub WITH DEFAULT KEY,

           BEGIN OF ts_rule,
             main   TYPE string,
             subs   TYPE string,
             t_subs TYPE tt_sub,
           END OF ts_rule,
           tt_rule   TYPE SORTED TABLE OF ts_rule WITH UNIQUE KEY main,

           tt_unq_id TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_rule TYPE zcl_advent2020_day07=>tt_rule.
    METHODS _init_rules
      IMPORTING
        it_input     TYPE string_table
        iv_count_sub TYPE abap_bool OPTIONAL.

    METHODS _count_01
      IMPORTING
        iv_sub_bag TYPE string
      CHANGING
        ct_unq_id  TYPE tt_unq_id.

    METHODS _count_02
      IMPORTING
                iv_main_bag     TYPE string
      RETURNING VALUE(rv_count) TYPE i.
ENDCLASS.


CLASS zcl_advent2020_day07 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count_1) = part1( lt_input ).
    DATA(lv_count_2) = part2( lt_input ).

    out->write( |{ lv_count_1 } - { lv_count_2 }| ).
  ENDMETHOD.

  METHOD part1.
    _init_rules( it_input ).

    DATA(lt_unq_id) = VALUE tt_unq_id( ).
    _count_01( EXPORTING iv_sub_bag  = |shiny gold|
               CHANGING  ct_unq_id   = lt_unq_id ).
    rv_count = lines( lt_unq_id ).
  ENDMETHOD.

  METHOD part2.
    _init_rules( it_input     = it_input
                 iv_count_sub = abap_true ).
    rv_count = _count_02( iv_main_bag  = |shiny gold| ) - 1.
  ENDMETHOD.

  METHOD _init_rules.
    mt_rule = VALUE tt_rule( ).
    LOOP AT it_input ASSIGNING FIELD-SYMBOL(<lv_input>).
      SPLIT <lv_input> AT | bags contain | INTO DATA(lv_main)
                                                DATA(lv_sub_bags).
      INSERT VALUE #( main = lv_main
                      subs = lv_sub_bags ) INTO TABLE mt_rule ASSIGNING FIELD-SYMBOL(<ls_rule>).

      " for part_2 only
      CHECK iv_count_sub = abap_true.
      REPLACE ALL OCCURRENCES OF: `, `   IN lv_sub_bags WITH ``,
                                  `.`    IN lv_sub_bags WITH ``,
                                  `bags` IN lv_sub_bags WITH `-`,
                                  `bag`  IN lv_sub_bags WITH `-`.
      SPLIT lv_sub_bags AT `-` INTO TABLE DATA(lt_bags).
      LOOP AT lt_bags INTO DATA(lv_bag) WHERE table_line IS NOT INITIAL.
        CHECK lv_bag(1) CO `0123456789`.

        SPLIT lv_bag AT | | INTO DATA(lv_sub_count)
                                 DATA(lv_sub_bag).
        CONDENSE lv_sub_bag.
        APPEND VALUE #( count = lv_sub_count
                        bag   = lv_sub_bag ) TO <ls_rule>-t_subs.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD _count_01.
    LOOP AT mt_rule ASSIGNING FIELD-SYMBOL(<ls_rule>) WHERE subs CS iv_sub_bag.
      INSERT sy-tabix INTO TABLE ct_unq_id.

      _count_01( EXPORTING iv_sub_bag = <ls_rule>-main
                 CHANGING  ct_unq_id  = ct_unq_id ).
    ENDLOOP.
  ENDMETHOD.

  METHOD _count_02.
    rv_count = 1.
    ASSIGN mt_rule[ main = iv_main_bag ] TO FIELD-SYMBOL(<ls_rule>).
    CHECK sy-subrc = 0.

    " TODO check for infinite loops?
    LOOP AT <ls_rule>-t_subs ASSIGNING FIELD-SYMBOL(<ls_sub>).
      rv_count = rv_count + <ls_sub>-count * _count_02( iv_main_bag = <ls_sub>-bag ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
