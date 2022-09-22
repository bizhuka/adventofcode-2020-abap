*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_input DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    DATA mv_input TYPE string.
ENDCLASS.


CLASS lcl_input IMPLEMENTATION.
  METHOD constructor.
    mv_input = |167248359|.
  ENDMETHOD.
ENDCLASS.
