CLASS zcl_longtext_service DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_longtext_service.

    CONSTANTS: default_page_size TYPE i VALUE 100,
               max_page_size     TYPE i VALUE 1000.

  PRIVATE SECTION.
    " Convert ITF lines to plain text string
    METHODS convert_to_string
      IMPORTING
        it_lines        TYPE tline_tab
        iv_language     TYPE spras
      RETURNING
        VALUE(rv_text)  TYPE string.

    " Build deduplicated ranges from keys
    METHODS build_ranges_from_keys
      IMPORTING
        it_keys      TYPE zif_longtext_service=>ty_text_keys
      EXPORTING
        er_object    TYPE zif_longtext_service=>ty_r_textobject
        er_name      TYPE zif_longtext_service=>ty_r_textname
        er_id        TYPE zif_longtext_service=>ty_r_textid
        er_language  TYPE zif_longtext_service=>ty_r_language.

    " Read text content via FM
    METHODS read_text_content
      IMPORTING
        ir_object        TYPE zif_longtext_service=>ty_r_textobject
        ir_name          TYPE zif_longtext_service=>ty_r_textname
        ir_id            TYPE zif_longtext_service=>ty_r_textid
        ir_language      TYPE zif_longtext_service=>ty_r_language
      RETURNING
        VALUE(rt_texts)  TYPE text_lh.

ENDCLASS.

CLASS zcl_longtext_service IMPLEMENTATION.

  METHOD zif_longtext_service~read_texts.
    CHECK it_keys IS NOT INITIAL.

    " Build deduplicated ranges
    DATA: lr_object   TYPE zif_longtext_service=>ty_r_textobject,
          lr_name     TYPE zif_longtext_service=>ty_r_textname,
          lr_id       TYPE zif_longtext_service=>ty_r_textid,
          lr_language TYPE zif_longtext_service=>ty_r_language.

    build_ranges_from_keys(
      EXPORTING it_keys     = it_keys
      IMPORTING er_object   = lr_object
                er_name     = lr_name
                er_id       = lr_id
                er_language = lr_language ).

    " Read text content if requested
    DATA lt_text_sorted TYPE SORTED TABLE OF itclh
      WITH UNIQUE KEY header-tdobject header-tdname header-tdid header-tdspras.

    IF iv_include_content = abap_true.
      DATA(lt_texts) = read_text_content(
        ir_object   = lr_object
        ir_name     = lr_name
        ir_id       = lr_id
        ir_language = lr_language ).
      lt_text_sorted = lt_texts.
    ENDIF.

    " Build results
    LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<key>).
      APPEND INITIAL LINE TO rt_results ASSIGNING FIELD-SYMBOL(<result>).
      <result>-textobject = <key>-textobject.
      <result>-textname   = <key>-textname.
      <result>-textid     = <key>-textid.
      <result>-language   = <key>-language.

      IF iv_include_content = abap_true.
        READ TABLE lt_text_sorted ASSIGNING FIELD-SYMBOL(<text>)
          WITH KEY header-tdobject = <key>-textobject
                   header-tdname   = <key>-textname
                   header-tdid     = <key>-textid
                   header-tdspras  = <key>-language.

        IF sy-subrc = 0 AND <text>-lines IS NOT INITIAL.
          <result>-longtext = convert_to_string(
            it_lines    = <text>-lines
            iv_language = <key>-language ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_longtext_service~read_texts_with_paging.
    " Determine paging
    DATA(lv_offset) = is_paging-offset.
    DATA(lv_limit) = is_paging-limit.
    IF lv_limit <= 0.
      lv_limit = default_page_size.
    ELSEIF lv_limit > max_page_size.
      lv_limit = max_page_size.
    ENDIF.

    " Get total count
    SELECT COUNT(*)
      FROM stxh
      WHERE tdobject IN @ir_textobject
        AND tdname   IN @ir_textname
        AND tdid     IN @ir_textid
        AND tdspras  IN @ir_language
      INTO @rs_result-total_count.

    " If only count requested, return early
    IF iv_count_only = abap_true.
      RETURN.
    ENDIF.

    " Read headers with paging
    SELECT tdobject, tdname, tdid, tdspras,
           tdtitle, tdfuser, tdfdate, tdluser, tdldate
      FROM stxh
      WHERE tdobject IN @ir_textobject
        AND tdname   IN @ir_textname
        AND tdid     IN @ir_textid
        AND tdspras  IN @ir_language
      ORDER BY tdobject, tdname, tdid, tdspras
      INTO TABLE @DATA(lt_headers)
      OFFSET @lv_offset
      UP TO @lv_limit ROWS.

    " Read text content if requested
    DATA lt_text_sorted TYPE SORTED TABLE OF itclh
      WITH UNIQUE KEY header-tdobject header-tdname header-tdid header-tdspras.

    IF iv_include_content = abap_true AND lt_headers IS NOT INITIAL.
      " Build keys from headers
      DATA(lt_keys) = VALUE zif_longtext_service=>ty_text_keys(
        FOR h IN lt_headers
        ( textobject = h-tdobject
          textname   = h-tdname
          textid     = h-tdid
          language   = h-tdspras ) ).

      " Build deduplicated ranges
      DATA: lr_object   TYPE zif_longtext_service=>ty_r_textobject,
            lr_name     TYPE zif_longtext_service=>ty_r_textname,
            lr_id       TYPE zif_longtext_service=>ty_r_textid,
            lr_language TYPE zif_longtext_service=>ty_r_language.

      build_ranges_from_keys(
        EXPORTING it_keys     = lt_keys
        IMPORTING er_object   = lr_object
                  er_name     = lr_name
                  er_id       = lr_id
                  er_language = lr_language ).

      DATA(lt_texts) = read_text_content(
        ir_object   = lr_object
        ir_name     = lr_name
        ir_id       = lr_id
        ir_language = lr_language ).
      lt_text_sorted = lt_texts.
    ENDIF.

    " Build results
    LOOP AT lt_headers ASSIGNING FIELD-SYMBOL(<header>).
      APPEND INITIAL LINE TO rs_result-results ASSIGNING FIELD-SYMBOL(<result>).
      <result>-textobject  = <header>-tdobject.
      <result>-textname    = <header>-tdname.
      <result>-textid      = <header>-tdid.
      <result>-language    = <header>-tdspras.
      <result>-title       = <header>-tdtitle.
      <result>-createdby   = <header>-tdfuser.
      <result>-createddate = <header>-tdfdate.
      <result>-changedby   = <header>-tdluser.
      <result>-changeddate = <header>-tdldate.

      IF iv_include_content = abap_true.
        READ TABLE lt_text_sorted ASSIGNING FIELD-SYMBOL(<text>)
          WITH KEY header-tdobject = <header>-tdobject
                   header-tdname   = <header>-tdname
                   header-tdid     = <header>-tdid
                   header-tdspras  = <header>-tdspras.

        IF sy-subrc = 0 AND <text>-lines IS NOT INITIAL.
          <result>-longtext = convert_to_string(
            it_lines    = <text>-lines
            iv_language = <header>-tdspras ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_ranges_from_keys.
    " Use hashed tables for O(1) deduplication
    DATA: ht_object   TYPE HASHED TABLE OF tdobject WITH UNIQUE KEY table_line,
          ht_name     TYPE HASHED TABLE OF tdobname WITH UNIQUE KEY table_line,
          ht_id       TYPE HASHED TABLE OF tdid WITH UNIQUE KEY table_line,
          ht_language TYPE HASHED TABLE OF spras WITH UNIQUE KEY table_line.

    LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<key>).
      INSERT <key>-textobject INTO TABLE ht_object.
      INSERT <key>-textname INTO TABLE ht_name.
      INSERT <key>-textid INTO TABLE ht_id.
      INSERT <key>-language INTO TABLE ht_language.
    ENDLOOP.

    " Convert to ranges
    er_object   = VALUE #( FOR o IN ht_object   ( sign = 'I' option = 'EQ' low = o ) ).
    er_name     = VALUE #( FOR n IN ht_name     ( sign = 'I' option = 'EQ' low = n ) ).
    er_id       = VALUE #( FOR i IN ht_id       ( sign = 'I' option = 'EQ' low = i ) ).
    er_language = VALUE #( FOR l IN ht_language ( sign = 'I' option = 'EQ' low = l ) ).
  ENDMETHOD.

  METHOD read_text_content.
    CALL FUNCTION 'READ_MULTIPLE_TEXTS'
      EXPORTING
        object_ranges   = ir_object
        name_ranges     = ir_name
        id_ranges       = ir_id
        language_ranges = ir_language
      IMPORTING
        text_table      = rt_texts.
  ENDMETHOD.

  METHOD convert_to_string.
    DATA lt_lines TYPE tline_tab.
    DATA lt_stream TYPE string_table.

    lt_lines = it_lines.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        language     = iv_language
        lf           = abap_true
      IMPORTING
        stream_lines = lt_stream
      TABLES
        itf_text     = lt_lines.

    rv_text = concat_lines_of( table = lt_stream sep = ` ` ).
  ENDMETHOD.

ENDCLASS.
