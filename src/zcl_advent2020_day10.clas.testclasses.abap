*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day10.
    METHODS setup.
    METHODS part1_1    FOR TESTING.
    METHODS part1_2    FOR TESTING.
    METHODS part2_1    FOR TESTING.
    METHODS part2_2    FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1_1.
    DATA(lt_input) = VALUE zcl_advent2020_day10=>inttab(
            ( |16| )
            ( |10| )
            ( |15| )
            ( |5| )
            ( |1| )
            ( |11| )
            ( |7| )
            ( |19| )
            ( |6| )
            ( |12| )
            ( |4| )
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1(
       it_input  = lt_input
       iv_diff   = 1 )
      exp = 7 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1(
       it_input  = lt_input
       iv_diff   = 3 )
      exp = 5 ).
  ENDMETHOD.

  METHOD part1_2.
    DATA(lt_input) = VALUE zcl_advent2020_day10=>inttab(
            ( |28| )
            ( |33| )
            ( |18| )
            ( |42| )
            ( |31| )
            ( |14| )
            ( |46| )
            ( |20| )
            ( |48| )
            ( |47| )
            ( |24| )
            ( |23| )
            ( |49| )
            ( |45| )
            ( |19| )
            ( |38| )
            ( |39| )
            ( |11| )
            ( |1| )
            ( |32| )
            ( |25| )
            ( |35| )
            ( |8| )
            ( |17| )
            ( |7| )
            ( |9| )
            ( |4| )
            ( |2| )
            ( |34| )
            ( |10| )
            ( |3| )
        ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1(
       it_input  = lt_input
       iv_diff   = 1 )
      exp = 22 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1(
       it_input  = lt_input
       iv_diff   = 3 )
      exp = 10 ).
  ENDMETHOD.

  METHOD part2_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2(
       it_input    = VALUE #(
            ( |16| )
            ( |10| )
            ( |15| )
            ( |5| )
            ( |1| )
            ( |11| )
            ( |7| )
            ( |19| )
            ( |6| )
            ( |12| )
            ( |4| )
      ) )
      exp = 8 ).
  ENDMETHOD.

  METHOD part2_2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2(
       it_input    = VALUE #(
            ( |28| )
            ( |33| )
            ( |18| )
            ( |42| )
            ( |31| )
            ( |14| )
            ( |46| )
            ( |20| )
            ( |48| )
            ( |47| )
            ( |24| )
            ( |23| )
            ( |49| )
            ( |45| )
            ( |19| )
            ( |38| )
            ( |39| )
            ( |11| )
            ( |1| )
            ( |32| )
            ( |25| )
            ( |35| )
            ( |8| )
            ( |17| )
            ( |7| )
            ( |9| )
            ( |4| )
            ( |2| )
            ( |34| )
            ( |10| )
            ( |3| )
      ) )
      exp = 19208 ).
  ENDMETHOD.

ENDCLASS.
