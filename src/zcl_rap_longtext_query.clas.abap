CLASS zcl_rap_longtext_query DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

    " Constructor with optional dependency injection
    METHODS constructor
      IMPORTING
        io_service TYPE REF TO zif_longtext_service OPTIONAL.

  PRIVATE SECTION.
    DATA mo_service TYPE REF TO zif_longtext_service.

ENDCLASS.

CLASS zcl_rap_longtext_query IMPLEMENTATION.

  METHOD constructor.
    IF io_service IS BOUND.
      mo_service = io_service.
    ELSE.
      mo_service = NEW zcl_longtext_service( ).
    ENDIF.
  ENDMETHOD.

  METHOD if_rap_query_provider~select.
    " Get filter conditions
    TRY.
        DATA(filters) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
        RETURN.
    ENDTRY.

    " Build filter ranges
    DATA: lr_textobject TYPE zif_longtext_service=>ty_r_textobject,
          lr_textname   TYPE zif_longtext_service=>ty_r_textname,
          lr_textid     TYPE zif_longtext_service=>ty_r_textid,
          lr_language   TYPE zif_longtext_service=>ty_r_language.

    LOOP AT filters ASSIGNING FIELD-SYMBOL(<filter>).
      CASE <filter>-name.
        WHEN 'TEXTOBJECT'. lr_textobject = CORRESPONDING #( <filter>-range ).
        WHEN 'TEXTNAME'.   lr_textname = CORRESPONDING #( <filter>-range ).
        WHEN 'TEXTID'.     lr_textid = CORRESPONDING #( <filter>-range ).
        WHEN 'LANGUAGE'.   lr_language = CORRESPONDING #( <filter>-range ).
      ENDCASE.
    ENDLOOP.

    " Get paging parameters
    DATA(ls_paging) = VALUE zif_longtext_service=>ty_paging(
      offset = io_request->get_paging( )->get_offset( )
      limit  = io_request->get_paging( )->get_page_size( ) ).

    " Check if longtext is requested
    DATA(requested_elements) = io_request->get_requested_elements( ).
    DATA(lv_include_content) = xsdbool( line_exists( requested_elements[ table_line = 'LONGTEXT' ] ) ).

    " Check if only count requested
    DATA(lv_count_only) = xsdbool( io_request->is_total_numb_of_rec_requested( ) = abap_true AND
                                   io_request->is_data_requested( ) = abap_false ).

    " Call service
    DATA(ls_result) = mo_service->read_texts_with_paging(
      ir_textobject      = lr_textobject
      ir_textname        = lr_textname
      ir_textid          = lr_textid
      ir_language        = lr_language
      is_paging          = ls_paging
      iv_include_content = lv_include_content
      iv_count_only      = lv_count_only ).

    " Map to response type
    DATA lt_response TYPE STANDARD TABLE OF zi_rap_longtext WITH EMPTY KEY.

    LOOP AT ls_result-results ASSIGNING FIELD-SYMBOL(<result>).
      APPEND VALUE zi_rap_longtext(
        textobject  = <result>-textobject
        textname    = <result>-textname
        textid      = <result>-textid
        language    = <result>-language
        title       = <result>-title
        createdby   = <result>-createdby
        createddate = <result>-createddate
        changedby   = <result>-changedby
        changeddate = <result>-changeddate
        longtext    = <result>-longtext
      ) TO lt_response.
    ENDLOOP.

    io_response->set_data( lt_response ).
    io_response->set_total_number_of_records( CONV int8( ls_result-total_count ) ).
  ENDMETHOD.

ENDCLASS.
