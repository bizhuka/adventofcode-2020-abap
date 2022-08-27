class ZCL_ADVENT2020_DAY14 definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  types:
    int8x TYPE x LENGTH 8 .
  types:
    BEGIN OF ts_result,
        key TYPE int8,
        val TYPE int8,
      END OF ts_result .
  types:
    tt_result TYPE HASHED TABLE OF ts_result WITH UNIQUE KEY key .

  constants:
    BEGIN OF ms_type,
                 mask   TYPE char4 VALUE 'mask',
                 memory TYPE char4 VALUE 'mem[',
               END OF ms_type .

  methods PART1
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_COUNT) type INT8 .
  methods PART2
    importing
      !IT_INPUT type STRINGTAB
      !IV_INIT type ABAP_BOOL default ABAP_TRUE
    returning
      value(RV_COUNT) type INT8 .
  methods BINARY_2_INT
    importing
      !IV_BINARY type STRING
    exporting
      !EV_DEC type INT8
      !EV_XDEC type INT8X .
  methods MASK_2_INT
    importing
      !IV_BINARY type STRING
    exporting
      !EV_OR type INT8X
      !EV_AND type INT8X .
  methods MASK_PART_01
    importing
      !IV_MASK type STRING
      !IV_NUMBER type STRING
    returning
      value(RV_NUMBER) type INT8 .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mt_result TYPE tt_result .

    METHODS _get_map
      IMPORTING
        !iv_input   TYPE string
      EXPORTING
        !ev_type    TYPE char4
        !ev_mask    TYPE string
        !ev_address TYPE int8
        !ev_value   TYPE int8 .
    METHODS _add_2_result
      IMPORTING
        !is_result TYPE ts_result .
    METHODS _get_result_as_number
      RETURNING
        VALUE(rv_number) TYPE int8 .
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY14 IMPLEMENTATION.


  METHOD binary_2_int.
    CLEAR: ev_dec,
           ev_xdec.

    DATA(lv_len) = strlen( iv_binary ).
    DO lv_len TIMES.
      DATA(lv_index) = sy-index.

      DATA(lv_char_ind) = lv_len - lv_index.
      DATA(lv_char)  = iv_binary+lv_char_ind(1).

      ev_dec = ev_dec + 2 ** ( lv_index - 1 ) * lv_char.
    ENDDO.

    ev_xdec = ev_dec.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count1) = part1( it_input = lt_input ).
    DATA(lv_count2) = part2( it_input = lt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count1 } { lv_count2 }| ).
  ENDMETHOD.


  METHOD mask_2_int.
    CLEAR: ev_or,
           ev_and.

    DATA(lv_mask_or)  = replace( val  = iv_binary
                                 sub  = 'X' with = '0'
                                 occ  = 0 ).
    DATA(lv_mask_and) = replace( val  = iv_binary
                                 sub  = 'X' with = '1'
                                 occ  = 0 ).
    binary_2_int( EXPORTING iv_binary = lv_mask_or
                  IMPORTING ev_xdec   = ev_or ).
    binary_2_int( EXPORTING iv_binary = lv_mask_and
                  IMPORTING ev_xdec   = ev_and ).
  ENDMETHOD.


  METHOD mask_part_01.
    mask_2_int( EXPORTING iv_binary = iv_mask
                IMPORTING ev_and    = DATA(lv_and)
                          ev_or     = DATA(lv_or) ).
    binary_2_int( EXPORTING iv_binary = iv_number
                  IMPORTING ev_xdec   = DATA(lv_number) ).

    lv_number = lv_number BIT-AND lv_and
                          BIT-OR  lv_or.
    rv_number = lv_number.
  ENDMETHOD.


  METHOD part1.
    mt_result = VALUE tt_result( ).

    LOOP AT it_input INTO DATA(lv_input).
      _get_map( EXPORTING iv_input   = lv_input
                IMPORTING ev_type    = DATA(lv_type)
                          ev_mask    = DATA(lv_mask)
                          ev_address = DATA(lv_address)
                          ev_value   = DATA(lv_value) ).
      CASE lv_type.
        WHEN ms_type-mask.
          mask_2_int( EXPORTING iv_binary = lv_mask
                      IMPORTING ev_and    = DATA(lv_and)
                                ev_or     = DATA(lv_or) ).
        WHEN ms_type-memory.
          DATA(lv_valx) = CONV int8x( lv_value ).
          lv_valx = lv_valx BIT-AND lv_and
                            BIT-OR  lv_or.
          DATA(lv_val) = CONV int8( lv_valx ).

          _add_2_result( VALUE #( key = lv_address
                                  val = lv_val ) ).
      ENDCASE.
    ENDLOOP.

    rv_count = _get_result_as_number( ).
  ENDMETHOD.


  METHOD part2.
    IF iv_init = abap_true.
      mt_result = VALUE tt_result( ).
    ENDIF.

    LOOP AT it_input INTO DATA(lv_input).
      _get_map( EXPORTING iv_input   = lv_input
                IMPORTING ev_type    = DATA(lv_type)
                          ev_mask    = DATA(lv_mask)
                          ev_address = DATA(lv_address)
                          ev_value   = DATA(lv_value) ).
      CASE lv_type.
        WHEN ms_type-mask.
          mask_2_int( EXPORTING iv_binary = lv_mask
                      IMPORTING ev_or     = DATA(lv_or) ).

          DATA(lv_mask_and) = replace( val = lv_mask sub  = '0' with = '1' occ  = 0 ).
          REPLACE ALL OCCURRENCES OF 'X' IN lv_mask_and WITH '0' RESULTS DATA(lt_result).

          binary_2_int( EXPORTING iv_binary = lv_mask_and
                        IMPORTING ev_xdec   = DATA(lv_and) ).
          DATA(lv_zeros) = repeat( val = `0` occ = strlen( lv_mask  ) ).
        WHEN ms_type-memory.
          DATA(lv_valx) = CONV int8x( lv_address ).
          lv_valx = lv_valx BIT-AND lv_and
                            BIT-OR  lv_or.
          lv_address = CONV int8( lv_valx ).

          DO 2 ** lines( lt_result[] ) TIMES.
            DATA(lv_index) = sy-index - 1.
            DATA(lv_add_number) = lv_zeros.

            LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
              REPLACE SECTION OFFSET <ls_result>-offset LENGTH 1 OF lv_add_number
                               WITH COND #( WHEN lv_index MOD 2 = 0 THEN '1' ELSE '0' ).
              lv_index = lv_index DIV 2.
            ENDLOOP.

            binary_2_int( EXPORTING iv_binary = lv_add_number
                          IMPORTING ev_dec    = DATA(lv_add) ).
            _add_2_result( VALUE #( key = lv_address + lv_add
                                    val = lv_value ) ).
          ENDDO.

      ENDCASE.
    ENDLOOP.

    rv_count = _get_result_as_number( ).
  ENDMETHOD.


  METHOD _add_2_result.
    DELETE mt_result WHERE key = is_result-key.
    INSERT is_result INTO TABLE mt_result.
  ENDMETHOD.


  METHOD _get_map.
    CLEAR: ev_type,
           ev_mask,
           ev_address,
           ev_value.

    ev_type = iv_input(4).

    CASE ev_type.
      WHEN ms_type-mask.
        ev_mask = iv_input+7.

      WHEN ms_type-memory.
        SPLIT iv_input AT ` = `  INTO DATA(lv_key_txt)
                                      DATA(lv_val_txt).
        DATA(lv_len) = strlen( lv_key_txt ) - 5.

        ev_address = lv_key_txt+4(lv_len).
        ev_value   = lv_val_txt.
    ENDCASE.
  ENDMETHOD.


  METHOD _get_result_as_number.
    rv_number = REDUCE #( INIT s = CONV int8( 0 )
                          FOR <ls_result> IN mt_result
                          NEXT s = s + <ls_result>-val ).
  ENDMETHOD.
ENDCLASS.
