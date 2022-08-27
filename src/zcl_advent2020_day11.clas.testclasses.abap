*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day11.
    METHODS setup.
    METHODS index_x_y     FOR TESTING.
    METHODS part1         FOR TESTING.
    METHODS part2         FOR TESTING.
ENDCLASS.


CLASS zcl_advent2020_day11 DEFINITION LOCAL FRIENDS lcl_test.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD index_x_y.
    cut->_get_2dim( VALUE #(
     ( |1234| )
     ( |5678| )
     ( |9ABC| ) ) ).

**********************************************************************
    cut->_get_x_y( EXPORTING iv_index = 1
                   IMPORTING x        = DATA(x)
                             y        = DATA(y)
                             s        = DATA(lv_char) ).
    cl_abap_unit_assert=>assert_equals(
      act = |{ x }-{ y }-{ lv_char }|
      exp = |1-1-1| ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->_get_index( _x = x _y = y )
      exp = 1 ).
**********************************************************************
    cut->_get_x_y( EXPORTING iv_index = 2
                   IMPORTING x        = x
                             y        = y
                             s        = lv_char ).
    cl_abap_unit_assert=>assert_equals(
      act = |{ x }-{ y }-{ lv_char }|
      exp = |1-2-2| ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->_get_index( _x = x _y = y )
      exp = 2 ).
**********************************************************************
    cut->_get_x_y( EXPORTING iv_index = 7
                   IMPORTING x        = x
                             y        = y
                             s        = lv_char ).
    cl_abap_unit_assert=>assert_equals(
      act = |{ x }-{ y }-{ lv_char }|
      exp = |2-3-7| ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->_get_index( _x = x _y = y )
      exp = 7 ).
**********************************************************************
    cut->_get_x_y( EXPORTING iv_index = 10
                   IMPORTING x        = x
                             y        = y
                             s        = lv_char ).
    cl_abap_unit_assert=>assert_equals(
      act = |{ x }-{ y }-{ lv_char }|
      exp = |3-2-A| ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->_get_index( _x = x _y = y )
      exp = 10 ).
**********************************************************************
    cut->_get_x_y( EXPORTING iv_index = 12
                   IMPORTING x        = x
                             y        = y
                             s        = lv_char ).
    cl_abap_unit_assert=>assert_equals(
      act = |{ x }-{ y }-{ lv_char }|
      exp = |3-4-C| ).
    cl_abap_unit_assert=>assert_equals(
      act = cut->_get_index( _x = x _y = y )
      exp = 12 ).
**********************************************************************
    TRY.
        cut->_get_x_y( iv_index = 0 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_parameter_invalid.
    ENDTRY.
    TRY.
        cut->_get_x_y( iv_index = -1 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_parameter_invalid.
    ENDTRY.
    TRY.
        cut->_get_x_y( iv_index = 13 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_parameter_invalid.
    ENDTRY.
**********************************************************************
    TRY.
        cut->_get_index( _x = 0 _y = 2 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_parameter_invalid.
    ENDTRY.
    TRY.
        cut->_get_index( _x = 4 _y = 2 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_parameter_invalid.
    ENDTRY.
    TRY.
        cut->_get_index( _x = 2 _y = 0 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_parameter_invalid.
    ENDTRY.
    TRY.
        cut->_get_index( _x = 2 _y = 5 ).
        cl_abap_unit_assert=>fail( ).
      CATCH cx_parameter_invalid.
    ENDTRY.
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

    LOOP AT lt_all ASSIGNING FIELD-SYMBOL(<lt_step>).
      DATA(lv_step) = sy-tabix.
      cl_abap_unit_assert=>assert_equals(
        act = cut->part2(
                it_input  = lt_input
                iv_times  = lv_step )
        exp = cut->part2(
                it_input  = <lt_step> ) ).
    ENDLOOP.

    DATA(lv_occupied) = NEW i( ).
    cut->part2( it_input    = lt_input
                iv_times    = -1
                rr_occupied = lv_occupied ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_occupied->*
      exp = 26 ).
  ENDMETHOD.

ENDCLASS.
