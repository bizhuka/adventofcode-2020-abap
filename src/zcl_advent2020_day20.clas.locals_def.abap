*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_tile DEFINITION DEFERRED.

TYPES:
  tt_tile TYPE STANDARD TABLE OF REF TO lcl_tile WITH DEFAULT KEY,

  BEGIN OF ts_match,
    border TYPE string,
    tile   TYPE REF TO lcl_tile,
  END OF ts_match,
  tt_match TYPE STANDARD TABLE OF ts_match WITH DEFAULT KEY
                                           WITH NON-UNIQUE SORTED KEY _ordered COMPONENTS border.

CLASS lcl_tile DEFINITION FINAL.
  PUBLIC SECTION.
    DATA:
      id          TYPE numc4,
      _ord        TYPE i,
      t_image     TYPE stringtab,
      t_borders   TYPE stringtab,

      fit_count   TYPE i,
      _flip_count TYPE i.

    METHODS:
      constructor
        IMPORTING
          iv_index TYPE i
          it_array TYPE stringtab,

      _set_borders,

      paste_next_combination
        CHANGING
          ct_match TYPE tt_match,

      flip_border.
*        RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.
