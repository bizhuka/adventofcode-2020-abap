*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_input DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    DATA mt_input TYPE stringtab.
ENDCLASS.


CLASS lcl_input IMPLEMENTATION.
  METHOD constructor.
      NEW zcl_xtt_file_smw0( 'ZCL_ADVENT2020_DAY21-TXT'
      )->zif_xtt_file~get_content( IMPORTING ev_as_string = DATA(lv_file) ).

      SPLIT lv_file AT cl_abap_char_utilities=>newline INTO TABLE mt_input.
  ENDMETHOD.
ENDCLASS.
