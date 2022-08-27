*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day11.
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
         ( |L.LL.LL.LL| )
         ( |LLLLLLL.LL| )
         ( |L.L.L..L..| )
         ( |LLLL.LL.LL| )
         ( |L.LL.LL.LL| )
         ( |L.LLLLL.LL| )
         ( |..L.L.....| )
         ( |LLLLLLLLLL| )
         ( |L.LLLLLL.L| )
         ( |L.LLLLL.LL| )
    ).
    DATA lt_all TYPE STANDARD TABLE OF stringtab WITH DEFAULT KEY.
    lt_all = VALUE #(
      ( VALUE #(
          ( |#.##.##.##| )
          ( |#######.##| )
          ( |#.#.#..#..| )
          ( |####.##.##| )
          ( |#.##.##.##| )
          ( |#.#####.##| )
          ( |..#.#.....| )
          ( |##########| )
          ( |#.######.#| )
          ( |#.#####.##| ) ) )
      ( VALUE #(
         ( |#.LL.L#.##| )
          ( |#LLLLLL.L#| )
          ( |L.L.L..L..| )
          ( |#LLL.LL.L#| )
          ( |#.LL.LL.LL| )
          ( |#.LLLL#.##| )
          ( |..L.L.....| )
          ( |#LLLLLLLL#| )
          ( |#.LLLLLL.L| )
          ( |#.#LLLL.##| ) ) )
      ( VALUE #(
          ( |#.##.L#.##| )
          ( |#L###LL.L#| )
          ( |L.#.#..#..| )
          ( |#L##.##.L#| )
          ( |#.##.LL.LL| )
          ( |#.###L#.##| )
          ( |..#.#.....| )
          ( |#L######L#| )
          ( |#.LL###L.L| )
          ( |#.#L###.##| ) ) )
      ( VALUE #(
          ( |#.#L.L#.##| )
          ( |#LLL#LL.L#| )
          ( |L.L.L..#..| )
          ( |#LLL.##.L#| )
          ( |#.LL.LL.LL| )
          ( |#.LL#L#.##| )
          ( |..L.L.....| )
          ( |#L#LLLL#L#| )
          ( |#.LLLLLL.L| )
          ( |#.#L#L#.##| ) ) )
      ( VALUE #(
          ( |#.#L.L#.##| )
          ( |#LLL#LL.L#| )
          ( |L.#.L..#..| )
          ( |#L##.##.L#| )
          ( |#.#L.LL.LL| )
          ( |#.#L#L#.##| )
          ( |..L.L.....| )
          ( |#L#L##L#L#| )
          ( |#.LLLLLL.L| )
          ( |#.#L#L#.##| ) ) ) ).

    LOOP AT lt_all ASSIGNING FIELD-SYMBOL(<lt_step>).
      DATA(lv_step) = sy-tabix.
      cl_abap_unit_assert=>assert_equals(
        act = cut->part1(
                it_input  = lt_input
                iv_times  = lv_step )
        exp = cut->part1(
                it_input  = <lt_step> ) ).
    ENDLOOP.

    DATA(lv_occupied) = NEW i( ).
    cut->part1( it_input    = lt_input
                iv_times    = -1
                rr_occupied = lv_occupied ).
      cl_abap_unit_assert=>assert_equals(
        act = lv_occupied->*
        exp = 37 ).
  ENDMETHOD.

  METHOD part2.
    DATA(lt_input) = VALUE stringtab(
( |L.LL.LL.LL| )
( |LLLLLLL.LL| )
( |L.L.L..L..| )
( |LLLL.LL.LL| )
( |L.LL.LL.LL| )
( |L.LLLLL.LL| )
( |..L.L.....| )
( |LLLLLLLLLL| )
( |L.LLLLLL.L| )
( |L.LLLLL.LL| )
    ).

    DATA lt_all TYPE STANDARD TABLE OF stringtab WITH DEFAULT KEY.
    lt_all = VALUE #(
      ( VALUE #(
( |#.##.##.##| )
( |#######.##| )
( |#.#.#..#..| )
( |####.##.##| )
( |#.##.##.##| )
( |#.#####.##| )
( |..#.#.....| )
( |##########| )
( |#.######.#| )
( |#.#####.##| ) ) )
      ( VALUE #(
( |#.LL.LL.L#| )
( |#LLLLLL.LL| )
( |L.L.L..L..| )
( |LLLL.LL.LL| )
( |L.LL.LL.LL| )
( |L.LLLLL.LL| )
( |..L.L.....| )
( |LLLLLLLLL#| )
( |#.LLLLLL.L| )
( |#.LLLLL.L#| ) ) )
      ( VALUE #(
( |#.L#.##.L#| )
( |#L#####.LL| )
( |L.#.#..#..| )
( |##L#.##.##| )
( |#.##.#L.##| )
( |#.#####.#L| )
( |..#.#.....| )
( |LLL####LL#| )
( |#.L#####.L| )
( |#.L####.L#| ) ) )
      ( VALUE #(
( |#.L#.L#.L#| )
( |#LLLLLL.LL| )
( |L.L.L..#..| )
( |##LL.LL.L#| )
( |L.LL.LL.L#| )
( |#.LLLLL.LL| )
( |..L.L.....| )
( |LLLLLLLLL#| )
( |#.LLLLL#.L| )
( |#.L#LL#.L#| ) ) )
      ( VALUE #(
( |#.L#.L#.L#| )
( |#LLLLLL.LL| )
( |L.L.L..#..| )
( |##L#.#L.L#| )
( |L.L#.#L.L#| )
( |#.L####.LL| )
( |..#.#.....| )
( |LLL###LLL#| )
( |#.LLLLL#.L| )
( |#.L#LL#.L#| ) ) )

      ( VALUE #(
( |#.L#.L#.L#| )
( |#LLLLLL.LL| )
( |L.L.L..#..| )
( |##L#.#L.L#| )
( |L.L#.LL.L#| )
( |#.LLLL#.LL| )
( |..#.L.....| )
( |LLL###LLL#| )
( |#.LLLLL#.L| )
( |#.L#LL#.L#| ) ) )
).

    LOOP AT lt_all ASSIGNING FIELD-SYMBOL(<lt_step>) to 2.
      DATA(lv_step) = sy-tabix.
      cl_abap_unit_assert=>assert_equals(
        act = cut->part2(
                it_input  = lt_input
                iv_times  = lv_step )
        exp = cut->part2(
                it_input  = <lt_step> ) ).
    ENDLOOP.

*    DATA(lv_occupied) = NEW i( ).
*    cut->part2( it_input    = lt_input
*                iv_times    = -1
*                rr_occupied = lv_occupied ).
*      cl_abap_unit_assert=>assert_equals(
*        act = lv_occupied->*
*        exp = 26 ).
  ENDMETHOD.

ENDCLASS.
