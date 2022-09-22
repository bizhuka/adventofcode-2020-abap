*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day20.
    METHODS setup.
    METHODS get_total_fit_count FOR TESTING.
    METHODS part1               FOR TESTING.
    METHODS part2               FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_total_fit_count.
    cl_abap_unit_assert=>assert_equals(
      act = cut->get_total_fit_count( 2 * 2 )
      exp = 8 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_total_fit_count( 3 * 3 )
      exp = 24 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_total_fit_count( 4 * 4 )
      exp = 48 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_total_fit_count( 5 * 5 )
      exp = 80 ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->get_total_fit_count( 6 * 6 )
      exp = 120 ).
  ENDMETHOD.

  METHOD part1.
    DATA(lt_simple) = VALUE stringtab(
        ( |Tile 2311:| )
        ( |..##.#..#.| )
        ( |##..#.....| )
        ( |#...##..#.| )
        ( |####.#...#| )
        ( |##.##.###.| )
        ( |##...#.###| )
        ( |.#.#.#..##| )
        ( |..#....#..| )
        ( |###...#.#.| )
        ( |..###..###| )
        ( || )
        ( |Tile 1951:| )
        ( |#.##...##.| )
        ( |#.####...#| )
        ( |.....#..##| )
        ( |#...######| )
        ( |.##.#....#| )
        ( |.###.#####| )
        ( |###.##.##.| )
        ( |.###....#.| )
        ( |..#.#..#.#| )
        ( |#...##.#..| )
        ( || )
        ( |Tile 1171:| )
        ( |####...##.| )
        ( |#..##.#..#| )
        ( |##.#..#.#.| )
        ( |.###.####.| )
        ( |..###.####| )
        ( |.##....##.| )
        ( |.#...####.| )
        ( |#.##.####.| )
        ( |####..#...| )
        ( |.....##...| )
        ( || )
        ( |Tile 1427:| )
        ( |###.##.#..| )
        ( |.#..#.##..| )
        ( |.#.##.#..#| )
        ( |#.#.#.##.#| )
        ( |....#...##| )
        ( |...##..##.| )
        ( |...#.#####| )
        ( |.#.####.#.| )
        ( |..#..###.#| )
        ( |..##.#..#.| )
        ( || )
        ( |Tile 1489:| )
        ( |##.#.#....| )
        ( |..##...#..| )
        ( |.##..##...| )
        ( |..#...#...| )
        ( |#####...#.| )
        ( |#..#.#.#.#| )
        ( |...#.#.#..| )
        ( |##.#...##.| )
        ( |..##.##.##| )
        ( |###.##.#..| )
        ( || )
        ( |Tile 2473:| )
        ( |#....####.| )
        ( |#..#.##...| )
        ( |#.##..#...| )
        ( |######.#.#| )
        ( |.#...#.#.#| )
        ( |.#########| )
        ( |.###.#..#.| )
        ( |########.#| )
        ( |##...##.#.| )
        ( |..###.#.#.| )
        ( || )
        ( |Tile 2971:| )
        ( |..#.#....#| )
        ( |#...###...| )
        ( |#.#.###...| )
        ( |##.##..#..| )
        ( |.#####..##| )
        ( |.#..####.#| )
        ( |#..#.#..#.| )
        ( |..####.###| )
        ( |..#.#.###.| )
        ( |...#.#.#.#| )
        ( || )
        ( |Tile 2729:| )
*        ( |...#.#.#.#| )
*        ( |####.#....| )
*        ( |..#.#.....| )
*        ( |....#..#.#| )
*        ( |.##..##.#.| )
*        ( |.#.####...| )
*        ( |####.#.#..| )
*        ( |##.####...| )
*        ( |##..#.##..| )
*        ( |#.##...##.| )
        ( |#.##...##.| )
        ( |##..#.##..| )
        ( |##.####...| )
        ( |####.#.#..| )
        ( |.#.####...| )
        ( |.##..##.#.| )
        ( |....#..#.#| )
        ( |..#.#.....| )
        ( |####.#....| )
        ( |...#.#.#.#| )
        ( || )
        ( |Tile 3079:| )
        ( |#.#.#####.| )
        ( |.#..######| )
        ( |..#.......| )
        ( |######....| )
        ( |####.#..#.| )
        ( |.#...#.##.| )
        ( |#.#####.##| )
        ( |..#.###...| )
        ( |..#.......| )
        ( |..#.###...| )
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( lt_simple[] )
      exp = 20899048083289 ).
  ENDMETHOD.

  METHOD part2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #( ) )
      exp = 0 ).
  ENDMETHOD.

ENDCLASS.
