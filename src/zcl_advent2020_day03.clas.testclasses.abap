*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day03.
    METHODS setup.
    METHODS count_tree_1_1 FOR TESTING.
    METHODS count_tree_1_2 FOR TESTING.
    METHODS count_tree_1_3 FOR TESTING.
    METHODS count_tree_1_4 FOR TESTING.
    METHODS count_tree_1_5 FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
    cut->set_tree_pos( VALUE #(
            ( |..##.........##.........##.........##.........##.........##.......| )
            ( |#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..| )
            ( |.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.| )
            ( |..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#| )
            ( |.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.| )
            ( |..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....| )
            ( |.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#| )
            ( |.#........#.#........#.#........#.#........#.#........#.#........#| )
            ( |#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...| )
            ( |#...##....##...##....##...##....##...##....##...##....##...##....#| )
            ( |.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#| )
        ) ).
  ENDMETHOD.

  METHOD count_tree_1_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->count_tree_1( )
        exp      = 7 ).
  ENDMETHOD.

  METHOD count_tree_1_2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->count_tree_1(
        iv_col_offset = 1
        iv_row_offset = 1 )
      exp = 2 ).
  ENDMETHOD.

  METHOD count_tree_1_3.
    cl_abap_unit_assert=>assert_equals(
      act = cut->count_tree_1(
        iv_col_offset = 5
        iv_row_offset = 1 )
      exp = 3 ).
  ENDMETHOD.

  METHOD count_tree_1_4.
    cl_abap_unit_assert=>assert_equals(
      act = cut->count_tree_1(
        iv_col_offset = 7
        iv_row_offset = 1 )
      exp = 4 ).
  ENDMETHOD.

  METHOD count_tree_1_5.
    cl_abap_unit_assert=>assert_equals(
      act = cut->count_tree_1(
        iv_col_offset = 1
        iv_row_offset = 2 )
      exp = 2 ).
  ENDMETHOD.

ENDCLASS.
