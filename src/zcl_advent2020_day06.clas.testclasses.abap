*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day06.
    METHODS setup.
    METHODS part1      FOR TESTING.
    METHODS part2      FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( value #(
            ( |abc| )
            ( || )
            ( |a| )
            ( |b| )
            ( |c| )
            ( || )
            ( |ab| )
            ( |ac| )
            ( || )
            ( |a| )
            ( |a| )
            ( |a| )
            ( |a| )
            ( || )
            ( |b| )
      ) )
      exp = 11 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( value #(
            ( |abc| )
            ( || )
            ( |a| )
            ( |b| )
            ( |c| )
            ( || )
            ( |ab| )
            ( |ac| )
            ( || )
            ( |a| )
            ( |a| )
            ( |a| )
            ( |a| )
            ( || )
            ( |b| )
      ) )
      exp = 6 ).
  ENDMETHOD.

ENDCLASS.
