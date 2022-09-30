CLASS zcl_advent2020_day04 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    METHODS constructor.

    METHODS valid_count
      IMPORTING it_input        TYPE string_table
                iv_check_value  TYPE abap_bool OPTIONAL
      RETURNING VALUE(rv_count) TYPE i.

    METHODS is_ok
      IMPORTING
                iv_key       TYPE string
                iv_value     TYPE string
      RETURNING VALUE(rv_ok) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS is_valid
      IMPORTING
                iv_passport    TYPE string
                iv_check_value TYPE abap_bool
      RETURNING VALUE(rv_ok)   TYPE abap_bool.

    TYPES char3 TYPE c LENGTH 3.
    DATA mt_required TYPE SORTED TABLE OF char3 WITH UNIQUE KEY table_line.
*    DATA _out TYPE REF TO if_oo_adt_intrnl_classrun.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY04 IMPLEMENTATION.


  METHOD constructor.
    INSERT 'byr' INTO TABLE mt_required.
    INSERT 'iyr' INTO TABLE mt_required.
    INSERT 'eyr' INTO TABLE mt_required.
    INSERT 'hgt' INTO TABLE mt_required.
    INSERT 'hcl' INTO TABLE mt_required.
    INSERT 'ecl' INTO TABLE mt_required.
    INSERT 'pid' INTO TABLE mt_required.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
*    _out = out.

    DATA(lt_input)   = NEW lcl_input(  )->mt_input.
    DATA(lv_count_1) = valid_count( lt_input ).
    DATA(lv_count_2) = valid_count( it_input       = lt_input
                                    iv_check_value = abap_true ).
    out->write( |{ lv_count_1 } - { lv_count_2 }| ).
  ENDMETHOD.


  METHOD is_ok.
    CASE iv_key.
      WHEN 'byr'.
        FIND REGEX '^\d{4}$' IN iv_value.
        rv_ok = xsdbool( sy-subrc  = 0 AND iv_value BETWEEN |1920| AND |2002| ).

      WHEN 'iyr'.
        FIND REGEX '^\d{4}$' IN iv_value.
        rv_ok = xsdbool( sy-subrc  = 0 AND iv_value BETWEEN |2010| AND |2020| ).

      WHEN 'eyr'.
        FIND REGEX '^\d{4}$' IN iv_value.
        rv_ok = xsdbool( sy-subrc  = 0 AND iv_value BETWEEN |2020| AND |2030| ).

      WHEN 'hgt'.
        FIND REGEX '^(\d+)(cm|in)$' IN iv_value SUBMATCHES DATA(lt_height)
                                                           DATA(lv_unit).
        CHECK sy-subrc  = 0.
        rv_ok = COND #(  WHEN lv_unit = 'cm' THEN xsdbool( lt_height BETWEEN |150| AND |193| )
                         WHEN lv_unit = 'in' THEN xsdbool( lt_height BETWEEN |59|  AND |76| ) ).

      WHEN 'hcl'.
        FIND REGEX '^\#[0-9a-f]{6}$' IN iv_value.
        rv_ok = xsdbool( sy-subrc = 0 ).

      WHEN 'pid'.
        FIND REGEX '^\d{9}$' IN iv_value.
        rv_ok = xsdbool( sy-subrc = 0 ).

      WHEN 'ecl'.
        FIND REGEX '^(amb|\bblu|\bbrn|\bgry|\bgrn|\bhzl|\both)$' IN iv_value.
        rv_ok = xsdbool( sy-subrc = 0 ).

      WHEN OTHERS.
        rv_ok = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD is_valid.
    DATA(lt_required) = mt_required.

    SPLIT iv_passport AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_password).
    LOOP AT lt_password INTO DATA(lv_password).
      SPLIT lv_password  AT | | INTO TABLE DATA(lt_pair).

      LOOP AT lt_pair INTO DATA(lv_pair).
        SPLIT lv_pair AT |:| INTO DATA(lv_key)
                                  DATA(lv_value).
        IF iv_check_value = abap_true.
          CHECK is_ok( iv_key   = lv_key
                       iv_value = lv_value ) = abap_true.
        ENDIF.
        DELETE lt_required WHERE table_line = CONV char3( lv_key ).
      ENDLOOP.
    ENDLOOP.

    rv_ok = xsdbool( lt_required[] IS INITIAL ).
  ENDMETHOD.


  METHOD valid_count.
    DATA(lv_input) = concat_lines_of( table = it_input sep = cl_abap_char_utilities=>newline ).
    SPLIT lv_input AT cl_abap_char_utilities=>newline && cl_abap_char_utilities=>newline INTO TABLE DATA(lt_input).

    LOOP AT lt_input INTO DATA(lv_passport).
*      DATA(lv_tabix) = sy-tabix.
*      DATA(lv_ok)    = abap_undefined.
      IF is_valid( iv_passport    = lv_passport
                      iv_check_value = iv_check_value ) = abap_true.
        rv_count = rv_count + 1.
*         lv_ok    = abap_true.
      ENDIF.

*      _out->write( |{ lv_tabix } = { lv_ok }| ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
