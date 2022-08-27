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
    mt_input = VALUE #(
      ( |##....#.| )
      ( |#.#..#..| )
      ( |...#....| )
      ( |...#.#..| )
      ( |###....#| )
      ( |#.#....#| )
      ( |.#....##| )
      ( |.#.###.#| )
    ).
  ENDMETHOD.
ENDCLASS.
