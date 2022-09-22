CLASS zcl_advent2020_day22 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    TYPES:
      num_table     TYPE STANDARD TABLE OF numc3 WITH EMPTY KEY,
      num_table_ref TYPE REF TO num_table.
    TYPES:
      tt_hash        TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line .

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

    METHODS _read_cards
      IMPORTING
        !it_input   TYPE stringtab
      EXPORTING
        !et_player1 TYPE num_table
        !et_player2 TYPE num_table .
    METHODS _count_score
      IMPORTING
        !it_player1     TYPE num_table
        !it_player2     TYPE num_table
      RETURNING
        VALUE(rv_count) TYPE decfloat34 .
    METHODS _sub_game2
      CHANGING
        !ct_player1     TYPE num_table
        !ct_player2     TYPE num_table
      RETURNING
        VALUE(rv_1_win) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY22 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( lt_input ).
    DATA(lv_count2) = part2( lt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD part1.
    _read_cards( EXPORTING it_input   = it_input
                 IMPORTING et_player1 = DATA(lt_player1)
                           et_player2 = DATA(lt_player2) ).
    " Play
    WHILE lt_player1[] IS NOT INITIAL AND lt_player2[] IS NOT INITIAL.
      DATA(lv_num1) = lt_player1[ 1 ].
      DATA(lv_num2) = lt_player2[ 1 ].

      DELETE: lt_player1 INDEX 1,
              lt_player2 INDEX 1.

      IF lv_num1 > lv_num2.
        APPEND lv_num1 TO lt_player1.
        APPEND lv_num2 TO lt_player1.
      ELSE.
        APPEND lv_num2 TO lt_player2.
        APPEND lv_num1 TO lt_player2.
      ENDIF.
    ENDWHILE.

    rv_count = _count_score( it_player1 = lt_player1
                             it_player2 = lt_player2 ).
  ENDMETHOD.


  METHOD part2.
    _read_cards( EXPORTING it_input   = it_input
                 IMPORTING et_player1 = DATA(lt_player1)
                           et_player2 = DATA(lt_player2) ).

    _sub_game2( CHANGING ct_player1 = lt_player1
                         ct_player2 = lt_player2 ).

    rv_count = _count_score( it_player1 = lt_player1
                             it_player2 = lt_player2 ).
  ENDMETHOD.


  METHOD _count_score.
    DATA(lt_player) = COND num_table_ref( WHEN it_player1[] IS NOT INITIAL
                                           THEN REF #( it_player1 )
                                           ELSE REF #( it_player2 ) ).
    rv_count = REDUCE #( INIT s = 0
                         FOR lv_num IN lt_player->* INDEX INTO lv_index
                         NEXT s = s + lv_num * ( lines( lt_player->* ) - lv_index  + 1 ) ).
  ENDMETHOD.


  METHOD _read_cards.
    CLEAR: et_player1,
           et_player2.

    LOOP AT it_input INTO DATA(lv_input) WHERE table_line IS NOT INITIAL.
      IF lv_input CP 'Player *'.
        DATA(lt_player) = SWITCH num_table_ref( lv_input+7(1) WHEN '1' THEN REF #( et_player1 )
                                                              WHEN '2' THEN REF #( et_player2 ) ).
        CONTINUE.
      ENDIF.

      APPEND lv_input TO lt_player->*.
    ENDLOOP.
  ENDMETHOD.


  METHOD _sub_game2.
    DATA(lt_hash) = VALUE tt_hash( ).

    WHILE ct_player1[] IS NOT INITIAL AND ct_player2[] IS NOT INITIAL.
      " faster for NUMC than to int4
      DATA(lv_hash) = concat_lines_of( table = ct_player1  ) && ` - ` &&
                      concat_lines_of( table = ct_player2  ).
      INSERT lv_hash INTO TABLE lt_hash[].
      IF sy-subrc <> 0.
        rv_1_win = abap_true.
        RETURN.
      ENDIF.

      DATA(lv_num1) = ct_player1[ 1 ].
      DATA(lv_num2) = ct_player2[ 1 ].

      DATA(lv_1_win) = xsdbool( lv_num1 > lv_num2 ).
      IF lines( ct_player1 ) > lv_num1 AND lines( ct_player2 ) > lv_num2.
        DATA(lt_sub1) = VALUE num_table( ).
        DATA(lt_sub2) = VALUE num_table( ).
        APPEND LINES OF: ct_player1 FROM 2 TO lv_num1 + 1 TO lt_sub1,
                         ct_player2 FROM 2 TO lv_num2 + 1 TO lt_sub2.

        lv_1_win = _sub_game2( CHANGING ct_player1 = lt_sub1
                                        ct_player2 = lt_sub2 ).
      ENDIF.

      DELETE: ct_player1 INDEX 1,
              ct_player2 INDEX 1.

      IF lv_1_win = abap_true.
        APPEND lv_num1 TO ct_player1.
        APPEND lv_num2 TO ct_player1.
      ELSE.
        APPEND lv_num2 TO ct_player2.
        APPEND lv_num1 TO ct_player2.
      ENDIF.
    ENDWHILE.

    rv_1_win = xsdbool( ct_player1[] IS NOT INITIAL ).
  ENDMETHOD.
ENDCLASS.
