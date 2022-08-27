*"* use this source file for any macro definitions you need
*"* in the implementation part of the class


DEFINE _get_index.
  IF _x < 1 OR _x > mv_row_count OR
     _y < 1 OR _y > mv_col_count.
    RAISE EXCEPTION TYPE cx_parameter_invalid.
  ENDIF.

  _rv_index = ( _x - 1 ) * mv_col_count + _y.
END-OF-DEFINITION.

DEFINE _get_x_y.
  y = iv_index MOD mv_col_count.
  IF y = 0.
    y = mv_col_count.
  ENDIF.

  x = iv_index DIV mv_col_count + COND #( WHEN y = mv_col_count THEN 0 ELSE 1 ).

  IF iv_index <= 0 OR x > mv_row_count.
    RAISE EXCEPTION TYPE cx_parameter_invalid.
  ENDIF.
END-OF-DEFINITION.
