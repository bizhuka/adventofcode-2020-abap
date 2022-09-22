*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION LONG
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day23.
    METHODS setup.
    METHODS part1    FOR TESTING.
    METHODS part2    FOR TESTING.
ENDCLASS.

CLASS zcl_advent2020_day23 DEFINITION LOCAL FRIENDS lcl_test.

CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1.
    DATA(lv_input) = |389125467|.
    DATA(lt_results) = VALUE stringtab(
        ( |328915467| )
        ( |325467891| )
        ( |725891346| )
        ( |325846791| )
        ( |925841367| )
        ( |725841936| )
        ( |836741925| )
        ( |741583926| )
        ( |574183926| )
        ( |583741926| ) ).
    LOOP AT lt_results INTO DATA(lv_result).
      DATA(lv_times) = sy-tabix.
      cl_abap_unit_assert=>assert_equals(
        act = cut->part1( iv_input = lv_input
                          iv_times = lv_times )
        exp = lv_result ).
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( iv_input = lv_input
                        iv_times = 100
                        iv_final = abap_true )
      exp = |67384529| ).
  ENDMETHOD.

  METHOD part2.
    DATA(lv_input) = |389125467|.
    DATA(lt_results) = VALUE stringtab(
        ( |328915467| )
        ( |325467891| )
        ( |725891346| )
        ( |325846791| )
        ( |925841367| )
        ( |725841936| )
        ( |836741925| )
        ( |741583926| )
        ( |574183926| )
        ( |583741926| ) ).
    LOOP AT lt_results INTO DATA(lv_result).
      DATA(lv_times)   = sy-tabix.
      cl_abap_unit_assert=>assert_equals(
        act = cut->part2( iv_input   = lv_input
                          iv_times   = lv_times
                          iv_move_to = CONV char1( lv_result+0(1) ) )
        exp = lv_result ).
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( iv_input = lv_input
                        iv_times = 10 * cut->million )
      exp = |149245887792| ).
  ENDMETHOD.

ENDCLASS.
