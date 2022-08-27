*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION LONG
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day17.
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
        ( |.#.| )
        ( |..#| )
        ( |###| ) ).

    DATA(ra) = cut->get_cube(
       iv_z     = -1
       it_input = VALUE #( ( |#..| )
                           ( |..#| )
                           ( |.#.| ) ) ).
    DATA(rb) = cut->get_cube(
       it_input = VALUE #( ( |#.#| )
                           ( |.##| )
                           ( |.#.| ) ) ).
    DATA(rc) = cut->get_cube(
       iv_z     = 1
       it_input = VALUE #( ( |#..| )
                           ( |..#| )
                           ( |.#.| ) ) ).
    DATA(rabc) = VALUE zcl_advent2020_day17=>tt_cube( ).
    INSERT LINES OF ra INTO TABLE rabc.
    INSERT LINES OF rc INTO TABLE rabc.
    INSERT LINES OF rb INTO TABLE rabc.
    DELETE rabc WHERE state <> cut->ms_state-active.

    cl_abap_unit_assert=>assert_equals(
      act = lines( cut->part1( it_input = lt_input
                               iv_cycle = 1 ) )
      exp = lines( rabc ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( cut->part1( lt_input ) )
      exp = 112 ).
  ENDMETHOD.

  METHOD part2.
    DATA(lt_input) = VALUE stringtab(
        ( |.#.| )
        ( |..#| )
        ( |###| ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( cut->part2( lt_input ) )
      exp = 848 ).
  ENDMETHOD.

ENDCLASS.
