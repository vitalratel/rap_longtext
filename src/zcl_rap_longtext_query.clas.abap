CLASS zcl_rap_longtext_query DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
ENDCLASS.

CLASS zcl_rap_longtext_query IMPLEMENTATION.

  METHOD if_rap_query_provider~select.
    CONSTANTS: default_page_size TYPE i VALUE 100,
               max_page_size     TYPE i VALUE 1000.

    " Get filter conditions
    TRY.
        DATA(filters) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
        RETURN.
    ENDTRY.

    " Build WHERE clause from filters
    DATA: r_textobject TYPE RANGE OF tdobject,
          r_textname   TYPE RANGE OF tdobname,
          r_textid     TYPE RANGE OF tdid,
          r_language   TYPE RANGE OF spras.

    LOOP AT filters ASSIGNING FIELD-SYMBOL(<filter>).
      CASE <filter>-name.
        WHEN 'TEXTOBJECT'. r_textobject = CORRESPONDING #( <filter>-range ).
        WHEN 'TEXTNAME'.   r_textname = CORRESPONDING #( <filter>-range ).
        WHEN 'TEXTID'.     r_textid = CORRESPONDING #( <filter>-range ).
        WHEN 'LANGUAGE'.   r_language = CORRESPONDING #( <filter>-range ).
      ENDCASE.
    ENDLOOP.

    " Get paging parameters
    DATA(skip) = io_request->get_paging( )->get_offset( ).
    DATA(top) = io_request->get_paging( )->get_page_size( ).
    IF top <= 0.
      top = default_page_size.
    ELSEIF top > max_page_size.
      top = max_page_size.
    ENDIF.

    " Get total count for paging
    SELECT COUNT(*)
      FROM stxh
      WHERE tdobject IN @r_textobject
        AND tdname   IN @r_textname
        AND tdid     IN @r_textid
        AND tdspras  IN @r_language
      INTO @DATA(total_count).

    " Read headers from STXH with paging
    SELECT tdobject, tdname, tdid, tdspras,
           tdtitle, tdfuser, tdfdate, tdluser, tdldate
      FROM stxh
      WHERE tdobject IN @r_textobject
        AND tdname   IN @r_textname
        AND tdid     IN @r_textid
        AND tdspras  IN @r_language
      ORDER BY tdobject, tdname, tdid, tdspras
      INTO TABLE @DATA(headers)
      OFFSET @skip
      UP TO @top ROWS.

    " For each header, read the long text content
    DATA result TYPE STANDARD TABLE OF zi_rap_longtext WITH EMPTY KEY.
    DATA lines  TYPE tline_tab.
    LOOP AT headers ASSIGNING FIELD-SYMBOL(<header>).
      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<row>).
      <row>-textobject  = <header>-tdobject.
      <row>-textname    = <header>-tdname.
      <row>-textid      = <header>-tdid.
      <row>-language    = <header>-tdspras.
      <row>-title       = <header>-tdtitle.
      <row>-createdby   = <header>-tdfuser.
      <row>-createddate = <header>-tdfdate.
      <row>-changedby   = <header>-tdluser.
      <row>-changeddate = <header>-tdldate.

      CLEAR lines.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = <header>-tdid
          language = <header>-tdspras
          name     = <header>-tdname
          object   = <header>-tdobject
        TABLES
          lines    = lines
        EXCEPTIONS
          OTHERS   = 1.

      IF sy-subrc = 0 AND lines IS NOT INITIAL.
        DATA stream_lines TYPE string_table.
        CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
          EXPORTING
            language     = <header>-tdspras
            lf           = abap_true
          IMPORTING
            stream_lines = stream_lines
          TABLES
            itf_text     = lines.

        <row>-longtext = concat_lines_of( table = stream_lines sep = ` ` ).
      ENDIF.
    ENDLOOP.

    io_response->set_data( result ).
    io_response->set_total_number_of_records( total_count ).
  ENDMETHOD.

ENDCLASS.