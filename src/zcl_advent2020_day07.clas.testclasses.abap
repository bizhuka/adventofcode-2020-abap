*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day07.
    METHODS setup.
    METHODS part1      FOR TESTING.
    METHODS part2_1     FOR TESTING.
    METHODS part2_2    FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD part1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part1( VALUE #(
            ( |light red bags contain 1 bright white bag, 2 muted yellow bags.| )
            ( |dark orange bags contain 3 bright white bags, 4 muted yellow bags.| )
            ( |bright white bags contain 1 shiny gold bag.| )
            ( |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.| )
            ( |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.| )
            ( |dark olive bags contain 3 faded blue bags, 4 dotted black bags.| )
            ( |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.| )
            ( |faded blue bags contain no other bags.| )
            ( |dotted black bags contain no other bags.| )
      ) )
      exp = 4 ).
  ENDMETHOD.

  METHOD part2_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #(
            ( |light red bags contain 1 bright white bag, 2 muted yellow bags.| )
            ( |dark orange bags contain 3 bright white bags, 4 muted yellow bags.| )
            ( |bright white bags contain 1 shiny gold bag.| )
            ( |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.| )
            ( |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.| )
            ( |dark olive bags contain 3 faded blue bags, 4 dotted black bags.| )
            ( |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.| )
            ( |faded blue bags contain no other bags.| )
            ( |dotted black bags contain no other bags.| )
      ) )
      exp = 32 ).
  ENDMETHOD.

  METHOD part2_2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->part2( VALUE #(
            ( |shiny gold bags contain 2 dark red bags.| )
            ( |dark red bags contain 2 dark orange bags.| )
            ( |dark orange bags contain 2 dark yellow bags.| )
            ( |dark yellow bags contain 2 dark green bags.| )
            ( |dark green bags contain 2 dark blue bags.| )
            ( |dark blue bags contain 2 dark violet bags.| )
            ( |dark violet bags contain no other bags.| )
      ) )
      exp = 126 ).
  ENDMETHOD.

ENDCLASS.
