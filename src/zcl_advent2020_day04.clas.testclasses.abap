*"* use this source file for your ABAP unit test classes


CLASS lcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
                          FINAL.

  PRIVATE SECTION.
    DATA cut      TYPE REF TO zcl_advent2020_day04.
    METHODS setup.
    METHODS valid_count_1 FOR TESTING.
    METHODS valid_all     FOR TESTING.
    METHODS invalid_all   FOR TESTING.
    METHODS byr_valid     FOR TESTING.
    METHODS byr_invalid   FOR TESTING.
    METHODS hgt_valid_1   FOR TESTING.
    METHODS hgt_valid_2   FOR TESTING.
    METHODS hgt_invalid_1 FOR TESTING.
    METHODS hgt_invalid_2 FOR TESTING.
    METHODS hcl_valid     FOR TESTING.
    METHODS hcl_invalid_1 FOR TESTING.
    METHODS hcl_invalid_2 FOR TESTING.
    METHODS ecl_valid     FOR TESTING.
    METHODS ecl_invalid   FOR TESTING.
    METHODS pid_valid     FOR TESTING.
    METHODS pid_invalid   FOR TESTING.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD valid_count_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->valid_count( VALUE #(
        ( |ecl:gry pid:860033327 eyr:2020 hcl:#fffffd| )
        ( |byr:1937 iyr:2017 cid:147 hgt:183cm| )
        ( || )
        ( |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884| )
        ( |hcl:#cfa07d byr:1929| )
        ( || )
        ( |hcl:#ae17e1 iyr:2013| )
        ( |eyr:2024| )
        ( |ecl:brn pid:760753108 byr:1931| )
        ( |hgt:179cm| )
        ( || )
        ( |hcl:#cfa07d eyr:2025 pid:166559648| )
        ( |iyr:2011 ecl:brn hgt:59in| )
      ) )
      exp = 2 ).
  ENDMETHOD.

  METHOD valid_all.
    cl_abap_unit_assert=>assert_equals(
      act = cut->valid_count( it_input =  VALUE #(
            ( |pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980| )
            ( |hcl:#623a2f| )
            ( || )
            ( |eyr:2029 ecl:blu cid:129 byr:1989| )
            ( |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm| )
            ( || )
            ( |hcl:#888785| )
            ( |hgt:164cm byr:2001 iyr:2015 cid:88| )
            ( |pid:545766238 ecl:hzl| )
            ( |eyr:2022| )
            ( || )
            ( |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719| )
      )
                              iv_check_value = abap_true )
      exp = 4 ).
  ENDMETHOD.

  METHOD invalid_all.
    cl_abap_unit_assert=>assert_equals(
      act = cut->valid_count( it_input =  VALUE #(
                ( |eyr:1972 cid:100| )
                ( |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926| )
                ( || )
                ( |iyr:2019| )
                ( |hcl:#602927 eyr:1967 hgt:170cm| )
                ( |ecl:grn pid:012533040 byr:1946| )
                ( || )
                ( |hcl:dab227 iyr:2012| )
                ( |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277| )
                ( || )
                ( |hgt:59cm ecl:zzz| )
                ( |eyr:2038 hcl:74454a iyr:2023| )
                ( |pid:3556412378 byr:2007| )
      )
                              iv_check_value = abap_true )
      exp = 0 ).
  ENDMETHOD.

  METHOD byr_valid.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_ok( iv_key   = 'byr'
                        iv_value = '2002' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD byr_invalid.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_ok( iv_key   = 'byr'
                        iv_value = '2003' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD hgt_valid_1.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_ok( iv_key   = 'hgt'
                        iv_value = '60in' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD hgt_valid_2.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_ok( iv_key   = 'hgt'
                        iv_value = '190cm' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD hgt_invalid_1.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_ok( iv_key   = 'hgt'
                        iv_value = '190in' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD hgt_invalid_2.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_ok( iv_key   = 'hgt'
                        iv_value = '190' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD ecl_valid.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_ok( iv_key   = 'ecl'
                        iv_value = 'brn' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD ecl_invalid.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_ok( iv_key   = 'ecl'
                        iv_value = 'wat' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD hcl_valid.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_ok( iv_key   = 'hcl'
                        iv_value = '#123abc' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD hcl_invalid_1.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_ok( iv_key   = 'hcl'
                        iv_value = '#123abz' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD hcl_invalid_2.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_ok( iv_key   = 'hcl'
                        iv_value = '123abc' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD pid_valid.
    cl_abap_unit_assert=>assert_equals(
      act = cut->is_ok( iv_key   = 'pid'
                        iv_value = '000000001' )
      exp = abap_true ).
  ENDMETHOD.

  METHOD pid_invalid.
    cl_abap_unit_assert=>assert_differs(
      act = cut->is_ok( iv_key   = 'pid'
                        iv_value = '0123456789' )
      exp = abap_true ).
  ENDMETHOD.
ENDCLASS.
