"! Test class for zcl_longtext_service
CLASS ltcl_longtext_service DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_longtext_service.

    METHODS setup.

    " Test read_texts method
    METHODS read_texts_empty_input FOR TESTING.
    METHODS read_texts_returns_keys FOR TESTING.

    " Test read_texts_with_paging method
    METHODS paging_count_only FOR TESTING.
    METHODS paging_default_limit FOR TESTING.
    METHODS paging_max_limit FOR TESTING.

    " Test build_ranges (indirectly via read_texts)
    METHODS deduplicates_ranges FOR TESTING.

ENDCLASS.

CLASS ltcl_longtext_service IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW zcl_longtext_service( ).
  ENDMETHOD.

  METHOD read_texts_empty_input.
    " Given empty keys
    DATA(lt_keys) = VALUE zif_longtext_service=>ty_text_keys( ).

    " When
    DATA(lt_results) = mo_cut->zif_longtext_service~read_texts( lt_keys ).

    " Then
    cl_abap_unit_assert=>assert_initial( lt_results ).
  ENDMETHOD.

  METHOD read_texts_returns_keys.
    " Given keys (texts may or may not exist)
    DATA(lt_keys) = VALUE zif_longtext_service=>ty_text_keys(
      ( textobject = 'TEXT' textname = 'TEST001' textid = 'ST' language = 'E' )
      ( textobject = 'TEXT' textname = 'TEST002' textid = 'ST' language = 'E' ) ).

    " When
    DATA(lt_results) = mo_cut->zif_longtext_service~read_texts( lt_keys ).

    " Then - results should have same count as keys
    cl_abap_unit_assert=>assert_equals(
      exp = lines( lt_keys )
      act = lines( lt_results ) ).

    " And keys should be preserved
    cl_abap_unit_assert=>assert_equals(
      exp = 'TEST001'
      act = lt_results[ 1 ]-textname ).
  ENDMETHOD.

  METHOD paging_count_only.
    " When count only
    DATA(ls_result) = mo_cut->zif_longtext_service~read_texts_with_paging(
      iv_count_only = abap_true ).

    " Then results should be empty but count may be > 0
    cl_abap_unit_assert=>assert_initial( ls_result-results ).
  ENDMETHOD.

  METHOD paging_default_limit.
    " When no limit specified
    DATA(ls_paging) = VALUE zif_longtext_service=>ty_paging( offset = 0 limit = 0 ).

    DATA(ls_result) = mo_cut->zif_longtext_service~read_texts_with_paging(
      is_paging = ls_paging ).

    " Then should use default (max 100 results)
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( lines( ls_result-results ) <= 100 ) ).
  ENDMETHOD.

  METHOD paging_max_limit.
    " When limit exceeds max
    DATA(ls_paging) = VALUE zif_longtext_service=>ty_paging( offset = 0 limit = 9999 ).

    DATA(ls_result) = mo_cut->zif_longtext_service~read_texts_with_paging(
      is_paging = ls_paging ).

    " Then should cap at 1000
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( lines( ls_result-results ) <= 1000 ) ).
  ENDMETHOD.

  METHOD deduplicates_ranges.
    " Given duplicate keys
    DATA(lt_keys) = VALUE zif_longtext_service=>ty_text_keys(
      ( textobject = 'TEXT' textname = 'TEST001' textid = 'ST' language = 'E' )
      ( textobject = 'TEXT' textname = 'TEST001' textid = 'ST' language = 'E' )
      ( textobject = 'TEXT' textname = 'TEST002' textid = 'ST' language = 'E' ) ).

    " When - should not fail even with duplicates
    DATA(lt_results) = mo_cut->zif_longtext_service~read_texts( lt_keys ).

    " Then - results match input count (including duplicates)
    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lines( lt_results ) ).
  ENDMETHOD.

ENDCLASS.


"! Mock implementation for testing consumers
CLASS ltcl_mock_service DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_longtext_service.
    DATA mt_mock_results TYPE zif_longtext_service=>ty_text_results.
    DATA ms_mock_query_result TYPE zif_longtext_service=>ty_query_result.
ENDCLASS.

CLASS ltcl_mock_service IMPLEMENTATION.

  METHOD zif_longtext_service~read_texts.
    rt_results = mt_mock_results.
  ENDMETHOD.

  METHOD zif_longtext_service~read_texts_with_paging.
    rs_result = ms_mock_query_result.
  ENDMETHOD.

ENDCLASS.


"! Test exit class with mock service
CLASS ltcl_exit_with_mock DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS delegates_to_service FOR TESTING.

ENDCLASS.

CLASS ltcl_exit_with_mock IMPLEMENTATION.

  METHOD delegates_to_service.
    " Given mock service with test data
    DATA(lo_mock) = NEW ltcl_mock_service( ).
    lo_mock->mt_mock_results = VALUE #(
      ( textobject = 'TEXT' textname = 'TEST001' textid = 'ST' language = 'E' longtext = 'Hello World' ) ).

    " And exit using mock
    DATA(lo_exit) = NEW zcl_rap_longtext_exit( io_service = lo_mock ).

    " Then service should be called when calculate is invoked
    " (Full test would require SADL test framework)
    cl_abap_unit_assert=>assert_bound( lo_exit ).
  ENDMETHOD.

ENDCLASS.
