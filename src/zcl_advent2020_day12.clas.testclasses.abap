*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day12.
    METHODS setup.
    METHODS part1    FOR TESTING.
    METHODS part2    FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1.
    DATA(lt_input) = VALUE stringtab(
      ( |F10| )
      ( |N3| )
      ( |F7| )
      ( |R90| )
      ( |F11| )
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( it_input = lt_input )
      exp = 25 ).
  ENDMETHOD.

  METHOD part2.
    DATA(lt_input) = VALUE stringtab(
      ( |F10| )
      ( |N3| )
      ( |F7| )
      ( |R90| )
      ( |F11| )
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( it_input = lt_input )
      exp = 286 ).
  ENDMETHOD.

ENDCLASS.
