class ZCL_ADVENT2020_DAY12 definition
  public
  final
  create public .

public section.

  interfaces IF_OO_ADT_CLASSRUN .

  data MT_INPUT type STRINGTAB read-only .

  methods CONSTRUCTOR .
  methods PART1
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_COUNT) type I .
  methods PART2
    importing
      !IT_INPUT type STRINGTAB
    returning
      value(RV_COUNT) type I .
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY12 IMPLEMENTATION.


  method CONSTRUCTOR.
    mt_input = NEW lcl_input(  )->mt_input.
  endmethod.


  METHOD if_oo_adt_classrun~main.
    DATA(lv_count_1) = part1( it_input = mt_input ).
    DATA(lv_count_2) = part2( it_input = mt_input ).
    CHECK out IS NOT INITIAL.
    out->write( |{ lv_count_1 } - { lv_count_2 }| ).
  ENDMETHOD.


  METHOD part1.
    DATA(lv_current_x)    = abap_true.
    DATA(lv_current_sign) = 1.

    DATA(x_east)  = 0.
    DATA(y_north) = 0.
    LOOP AT it_input INTO DATA(lv_input).
      DATA(lv_letter) = lv_input(1).
      DATA(lv_number) = CONV i( lv_input+1 ).

      FIELD-SYMBOLS <lv_dest> TYPE i.
      DATA(lv_sign) = 0.
      CASE lv_letter.
        WHEN 'N'.
          ASSIGN y_north TO <lv_dest>.
          lv_sign = 1.
        WHEN 'S'.
          ASSIGN y_north TO <lv_dest>.
          lv_sign = -1.
        WHEN 'E'.
          ASSIGN x_east  TO <lv_dest>.
          lv_sign = 1.
        WHEN 'W'.
          ASSIGN x_east  TO <lv_dest>.
          lv_sign = -1.

        WHEN 'F'.
          IF lv_current_x = abap_true.
            ASSIGN x_east  TO <lv_dest>.
          ELSE.
            ASSIGN y_north TO <lv_dest>.
          ENDIF.
          lv_sign = lv_current_sign.

        WHEN 'R' OR 'L'.
          DATA(lv_cur_angle) = SWITCH i( lv_current_x  WHEN abap_true  THEN COND #( WHEN lv_current_sign = 1 THEN 0  ELSE 180 )
                                                       WHEN abap_false THEN COND #( WHEN lv_current_sign = 1 THEN 90 ELSE 270 ) ).
          lv_sign = COND #( WHEN lv_letter = 'R' THEN -1 ELSE 1 ).
          lv_cur_angle = ( lv_cur_angle + lv_sign * lv_number ) MOD 360.

          CASE lv_cur_angle.
            WHEN 0.
              lv_current_x    = abap_true.
              lv_current_sign = 1.
            WHEN 90.
              lv_current_x    = abap_false.
              lv_current_sign = 1.
            WHEN 180.
              lv_current_x    = abap_true.
              lv_current_sign = -1.
            WHEN 270.
              lv_current_x    = abap_false.
              lv_current_sign = -1.
            WHEN OTHERS.
              MESSAGE 'Oops!' TYPE 'X'.
          ENDCASE.
          CONTINUE.

        WHEN OTHERS.
          MESSAGE 'Oops!' TYPE 'X'.
      ENDCASE.

      <lv_dest> = <lv_dest> + lv_sign * lv_number.
    ENDLOOP.

    rv_count = abs( x_east ) + abs( y_north ).
  ENDMETHOD.


  METHOD part2.
    DATA(x_east)  = 0.
    DATA(y_north) = 0.

    DATA(x_east_way)  = 10.
    DATA(y_north_way) = 1.

    LOOP AT it_input INTO DATA(lv_input).
      DATA(lv_tabix)  = sy-tabix.
      DATA(lv_letter) = lv_input(1).
      DATA(lv_number) = CONV i( lv_input+1 ).

      FIELD-SYMBOLS <lv_dest> TYPE i.
      DATA(lv_sign) = 0.
      CASE lv_letter.
        WHEN 'N'.
          ASSIGN y_north_way TO <lv_dest>.
          lv_sign = 1.
        WHEN 'S'.
          ASSIGN y_north_way TO <lv_dest>.
          lv_sign = -1.
        WHEN 'E'.
          ASSIGN x_east_way  TO <lv_dest>.
          lv_sign = 1.
        WHEN 'W'.
          ASSIGN x_east_way  TO <lv_dest>.
          lv_sign = -1.

        WHEN 'F'.
          x_east  = x_east  + x_east_way  * lv_number.
          y_north = y_north + y_north_way * lv_number.
          CONTINUE.

        WHEN 'R' OR 'L'.
          DATA(lv_sign_1) = COND #( WHEN lv_letter = 'R' THEN -1 ELSE 1 ).
          DATA(lv_sign_2) = COND #( WHEN lv_letter = 'L' THEN -1 ELSE 1 ).
          DO lv_number DIV 90 TIMES.
            DATA(lv_temp) = x_east_way.
            x_east_way    = y_north_way * lv_sign_2.
            y_north_way   = lv_temp     * lv_sign_1.
          ENDDO.
          CONTINUE.

        WHEN OTHERS.
          MESSAGE 'Oops!' TYPE 'X'.
      ENDCASE.

      <lv_dest> = <lv_dest> + lv_sign * lv_number.
    ENDLOOP.

    rv_count = abs( x_east ) + abs( y_north ).
  ENDMETHOD.
ENDCLASS.
