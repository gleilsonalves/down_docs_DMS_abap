FORM zf_busca_dados.

  DATA: sl_originals TYPE cv100_radio_buttons.

  CONSTANTS: lc_restrict TYPE i VALUE 1000.

  CLEAR sl_originals.

  PERFORM zf_monta_class_selection.

  sl_originals-all_originals = abap_true.

  CALL FUNCTION 'CV100_DOCUMENT_SEARCH'
    EXPORTING
      max_rows             = lc_restrict
      slang                = sy-langu
      classno              = c_class
      classtype            = c_class_tp
      originals            = sl_originals
    TABLES
      tdraw                = gt_draw
      clsc_class_selection = gt_class_selection
    EXCEPTIONS
      no_result            = 1
      bad_query            = 2
      not_authorized       = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE text-er1 TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF gt_draw[] IS NOT INITIAL.

*   Serão necessário todos os campos
    SELECT * FROM draw
      INTO TABLE gt_draw_all
      FOR ALL ENTRIES IN gt_draw
      WHERE dokar = gt_draw-dokar
        AND doknr = gt_draw-doknr
        AND dokvr = gt_draw-dokvr
        AND doktl = gt_draw-doktl.

    IF sy-subrc IS INITIAL AND gt_draw_all[] IS NOT INITIAL.
      SORT gt_draw_all BY dokar doknr dokvr doktl.

*     Tabela com poucos campos
      SELECT * FROM tdwst
        INTO TABLE gt_tdwst
        FOR ALL ENTRIES IN gt_draw_all
        WHERE cvlang = sy-langu
          AND dokst  = gt_draw_all-dokst.

      IF sy-subrc IS INITIAL.
        SORT gt_tdwst BY cvlang dokst.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.


FORM zf_monta_class_selection.

  DATA: ls_class_selection TYPE comw,
        lv_data            TYPE f,
        lv_exer            TYPE f.

  DATA: ls_cabn     TYPE cabn,
        ls_sel_char TYPE sel_char.

* Serão necessário todos os campos
  SELECT * FROM cabn
    INTO TABLE gt_cabn.

  IF sy-subrc IS INITIAL.
    SORT gt_cabn BY atnam attab atfel.
  ENDIF.

  IF gt_cabn[] IS NOT INITIAL.

    LOOP AT gt_cabn INTO DATA(ls_cod_caract).
      MOVE ls_cod_caract-atinn TO ls_sel_char-atinn.
      APPEND ls_sel_char TO gt_sel_char.
    ENDLOOP.

    "Fornecedor
    IF s_forn[] IS NOT INITIAL.
      LOOP AT s_forn INTO DATA(ls_forn).
        READ TABLE gt_cabn INTO ls_cabn WITH KEY attab = 'LFA1'
                                                 atfel = 'LIFNR'
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_class_selection-atinn = ls_cabn-atinn.
          ls_class_selection-atwrt = ls_forn-low.
          ls_class_selection-atcod = '1'.
          ls_class_selection-statu = 'H'.
          ls_class_selection-atfor = ls_cabn-atfor.
          ls_class_selection-slcod = '1'.
          ls_class_selection-attab = 'LFA1'.
          ls_class_selection-atfel = 'LIFNR'.
          ls_class_selection-catwrt = ls_forn-low.
          APPEND ls_class_selection TO gt_class_selection.
          CLEAR: ls_class_selection, ls_cabn.
        ENDIF.

      ENDLOOP.
    ENDIF.

    "Referência
    IF p_ref IS NOT INITIAL.
      READ TABLE gt_cabn INTO ls_cabn WITH KEY attab = 'BKPF'
                                               atfel = 'XBLNR'
                                               BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_class_selection-atinn = ls_cabn-atinn.
        ls_class_selection-atwrt = p_ref.
        ls_class_selection-atcod = '1'.
        ls_class_selection-statu = 'H'.
        ls_class_selection-atfor = ls_cabn-atfor.
        ls_class_selection-slcod = '1'.
        ls_class_selection-attab = 'BKPF'.
        ls_class_selection-atfel = 'XBLNR'.
        ls_class_selection-catwrt = p_ref.
        APPEND ls_class_selection TO gt_class_selection.
        CLEAR: ls_class_selection, ls_cabn.
      ENDIF.
    ENDIF.

    "Data do Documento
    IF s_dat[] IS NOT INITIAL.
      LOOP AT s_dat INTO DATA(ls_dat).
        READ TABLE gt_cabn INTO ls_cabn WITH KEY attab = 'BKPF'
                                                 atfel = 'BLDAT'
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_data = ls_dat-low.
          ls_class_selection-atinn = ls_cabn-atinn.
          ls_class_selection-atflv = lv_data.
          ls_class_selection-atcod = '1'.
          ls_class_selection-statu = 'H'.
          ls_class_selection-atfor = ls_cabn-atfor.
          ls_class_selection-slflv = lv_data.
          ls_class_selection-slcod = '1'.
          ls_class_selection-attab = 'BKPF'.
          ls_class_selection-atfel = 'BLDAT'.
          APPEND ls_class_selection TO gt_class_selection.
          CLEAR: ls_class_selection, lv_data, ls_cabn.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Vencimento
    IF s_venc[] IS NOT INITIAL.
      LOOP AT s_venc INTO DATA(ls_venc).
        READ TABLE gt_cabn INTO ls_cabn WITH KEY attab = 'BSEG'
                                                 atfel = 'ZFBDT'
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          lv_data = ls_venc-low.
          ls_class_selection-atinn = ls_cabn-atinn.
          ls_class_selection-atflv = lv_data.
          ls_class_selection-atcod = '1'.
          ls_class_selection-statu = 'H'.
          ls_class_selection-atfor = ls_cabn-atfor.
          ls_class_selection-slflv = lv_data.
          ls_class_selection-slcod = '1'.
          ls_class_selection-attab = 'BSEG'.
          ls_class_selection-atfel = 'ZFBDT'.
          APPEND ls_class_selection TO gt_class_selection.
          CLEAR: ls_class_selection, lv_data, ls_cabn.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Empresa
    IF s_emp[] IS NOT INITIAL.
      LOOP AT s_emp INTO DATA(ls_emp).
        READ TABLE gt_cabn INTO ls_cabn WITH KEY attab = 'T001'
                                                 atfel = 'BUKRS'
                                                 BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_class_selection-atinn = ls_cabn-atinn.
          ls_class_selection-atwrt = ls_emp-low.
          ls_class_selection-atcod = '1'.
          ls_class_selection-statu = 'H'.
          ls_class_selection-atfor = ls_cabn-atfor.
          ls_class_selection-slcod = '1'.
          ls_class_selection-attab = 'T001'.
          ls_class_selection-atfel = 'BUKRS'.
          ls_class_selection-catwrt = ls_emp-low.
          APPEND ls_class_selection TO gt_class_selection.
          CLEAR: ls_class_selection, ls_cabn.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "MIRO
    IF p_miro IS NOT INITIAL.
      READ TABLE gt_cabn INTO ls_cabn WITH KEY attab = 'RBKP'
                                               atfel = 'BELNR'
                                               BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_class_selection-atinn = ls_cabn-atinn.
        ls_class_selection-atwrt = p_miro.
        ls_class_selection-atcod = '1'.
        ls_class_selection-statu = 'H'.
        ls_class_selection-atfor = ls_cabn-atfor.
        ls_class_selection-slcod = '1'.
        ls_class_selection-attab = 'RBKP'.
        ls_class_selection-atfel = 'BELNR'.
        ls_class_selection-catwrt = p_miro.
        APPEND ls_class_selection TO gt_class_selection.
        CLEAR: ls_class_selection, ls_cabn.
      ENDIF.
    ENDIF.

    "Exercício
    IF p_exer IS NOT INITIAL.
      READ TABLE gt_cabn INTO ls_cabn WITH KEY attab = 'RBKP'
                                               atfel = 'BELNR'
                                               BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lv_exer = p_exer.
        ls_class_selection-atinn = ls_cabn-atinn.
        ls_class_selection-atflv = lv_exer.
        ls_class_selection-atcod = '1'.
        ls_class_selection-statu = 'H'.
        ls_class_selection-atfor = ls_cabn-atfor.
        ls_class_selection-slflv = lv_exer.
        ls_class_selection-slcod = '1'.
        ls_class_selection-attab = 'BKPF'.
        ls_class_selection-atfel = 'GJAHR'.
        APPEND ls_class_selection TO gt_class_selection.
        CLEAR: ls_class_selection, lv_exer, ls_cabn.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.


FORM zf_monta_saida.

  DATA: lcl_sclass    TYPE TABLE OF sclass,
        lcl_clobjdata TYPE TABLE OF clobjdat.

  DATA: ls_saida     TYPE ty_alv,
        ls_doc_key   TYPE dms_doc_key,
        lv_doknr(25) TYPE c.

  DATA: lcl_objdat    TYPE ausp-objek.

  LOOP AT gt_draw_all INTO DATA(ls_draw).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_draw-doknr
      IMPORTING
        output = lv_doknr.

    ls_saida-dokar = ls_draw-dokar.
    ls_saida-doknr = lv_doknr.
    ls_saida-dokvr = ls_draw-dokvr.
    ls_saida-doktl = ls_draw-doktl.
    ls_saida-dwnam = ls_draw-dwnam.
    ls_saida-dokst = ls_draw-dokst.
    ls_saida-loedk = ls_draw-loedk.

    READ TABLE gt_tdwst INTO DATA(ls_tdwst) WITH KEY cvlang = sy-langu
                                                     dokst = ls_draw-dokst
                                                     BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      ls_saida-dostx = ls_tdwst-dostx.
    ENDIF.

    MOVE: ls_draw-doknr TO ls_doc_key-doknr,
          ls_draw-dokar TO ls_doc_key-dokar,
          ls_draw-doktl TO ls_doc_key-doktl,
          ls_draw-dokvr TO ls_doc_key-dokvr.

    lcl_objdat = ls_doc_key.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
        class                = c_class
        classtype            = c_class_tp
        classtext            = ' '
        features             = 'X'
        language             = syst-langu
        initial_charact      = space
        object               = lcl_objdat
        objecttable          = 'DRAW'
        inherited_char       = space
      TABLES
        t_class              = lcl_sclass
        t_objectdata         = lcl_clobjdata
        i_sel_characteristic = gt_sel_char
      EXCEPTIONS
        no_classification    = 01
        no_classtypes        = 02
        invalid_class_type   = 03.

    IF syst-subrc <> 0.
    ELSE.

      LOOP AT lcl_clobjdata INTO DATA(ls_lcl_clobjdata).
        READ TABLE gt_cabn INTO DATA(ls_cabn_aux) WITH KEY atnam = ls_lcl_clobjdata-atnam
                                                           BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          IF ls_cabn_aux-attab EQ 'LFA1' AND ls_cabn_aux-atfel EQ 'LIFNR'.
            ls_saida-forn = ls_lcl_clobjdata-ausp1.
          ENDIF.

          IF ls_cabn_aux-attab EQ 'BKPF' AND ls_cabn_aux-atfel EQ 'XBLNR'.
            ls_saida-docref = ls_lcl_clobjdata-ausp1.
          ENDIF.

          IF ls_cabn_aux-attab EQ 'BKPF' AND ls_cabn_aux-atfel EQ 'BLDAT'.
            ls_saida-date = ls_lcl_clobjdata-ausp1.
          ENDIF.

          IF ls_cabn_aux-attab EQ 'BSEG' AND ls_cabn_aux-atfel EQ 'ZFBDT'.
            ls_saida-venc = ls_lcl_clobjdata-ausp1.
          ENDIF.

          IF ls_cabn_aux-attab EQ 'T001' AND ls_cabn_aux-atfel EQ 'BUKRS'.
            ls_saida-emp = ls_lcl_clobjdata-ausp1.
          ENDIF.

          IF ls_cabn_aux-attab EQ 'RBKP' AND ls_cabn_aux-atfel EQ 'BELNR'.
            ls_saida-miro = ls_lcl_clobjdata-ausp1.
          ENDIF.

          IF ls_cabn_aux-attab EQ 'BKPF' AND ls_cabn_aux-atfel EQ 'GJAHR'.
            ls_saida-exer = ls_lcl_clobjdata-ausp1.
          ENDIF.

        ENDIF.
      ENDLOOP.

    ENDIF.

    APPEND ls_saida TO gt_alv.

  ENDLOOP.

ENDFORM.


FORM zf_monta_fieldcat.

  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'CHECK'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Download'.
  gs_fieldcat-checkbox   = abap_true.
  gs_fieldcat-edit       = abap_true.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'DOKAR'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Tipo Doc.'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'DOKNR'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Nº Doc.'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'DWNAM'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Responsável'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'DOKST'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Status Doc.'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'DOSTX'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Desc. Status'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'LOEDK'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Marc. Elim.'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'FORN'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Fornecedor'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'DOCREF'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Doc. Ref.'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'DATE'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Data Doc.'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'VENC'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Vencimento'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'EMP'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Empresa'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'MIRO'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'MIRO'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname  = 'EXER'.
  gs_fieldcat-tabname    = 'GT_ALV'.
  gs_fieldcat-seltext_m  = 'Exerc.'.
  gs_fieldcat-just       = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

ENDFORM.


FORM zf_chama_alv.

  PERFORM zf_monta_fieldcat.

  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ZF_PF_STATUS_SET'
      i_callback_user_command  = 'ZF_USER_COMMAND'
      i_callback_top_of_page   = 'ZF_TOP_OF_PAGE'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.


FORM zf_top_of_page.

  DATA: lt_header TYPE slis_t_listheader,
        fs_header TYPE slis_listheader.

  DESCRIBE TABLE gt_alv LINES DATA(lv_lines).

  fs_header-typ  = 'H'.
  fs_header-info = 'Documentos DMS para Download'.
  APPEND fs_header TO lt_header.
  CLEAR fs_header.

  fs_header-typ  = 'S'.
  fs_header-key  = 'Usuário: '.
  fs_header-info = sy-uname.
  APPEND fs_header TO lt_header.
  CLEAR: fs_header.

  fs_header-typ = 'S'.
  fs_header-key = 'Data: '.
  CONCATENATE sy-datum+6(2) '.'
              sy-datum+4(2) '.'
              sy-datum(4)
         INTO fs_header-info.
  APPEND fs_header TO lt_header.
  CLEAR: fs_header.

  fs_header-typ  = 'S'.
  fs_header-key  = 'Total de Registros: '.
  fs_header-info = lv_lines.
  APPEND fs_header TO lt_header.
  CLEAR: fs_header.

  fs_header-typ  = 'S'.
  fs_header-key  = 'Diretório Local: '.
  fs_header-info = p_file.
  APPEND fs_header TO lt_header.
  CLEAR: fs_header.

  fs_header-typ  = 'S'.
  fs_header-key  = space.
  fs_header-info = space.
  APPEND fs_header TO lt_header.
  CLEAR: fs_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.

ENDFORM.


FORM zf_user_command USING ucomm       LIKE sy-ucomm
                           ls_selfield TYPE slis_selfield.

  DATA: ref_grid TYPE REF TO cl_gui_alv_grid.

  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.

  IF NOT ref_grid IS INITIAL.
    CALL METHOD ref_grid->check_changed_data.
  ENDIF.

  CASE ucomm.
    WHEN '&DOWN'. "Download
      PERFORM zf_download_anexos.
    WHEN '&SEL'.  "Selecionar Tudo
      SET SCREEN 0.
      PERFORM zf_marcar_tudo.
    WHEN '&DSEL'. "Desmarcar Tudo
      SET SCREEN 0.
      PERFORM zf_desmarcar_tudo.
    WHEN '&IC1'.
      IF ls_selfield-fieldname = 'DOKNR'.
        READ TABLE gt_alv INTO DATA(ls_aux) INDEX ls_selfield-tabindex.
        IF sy-subrc IS INITIAL.
          SET PARAMETER ID 'CV1' FIELD ls_aux-doknr.
          SET PARAMETER ID 'CV2' FIELD ls_aux-dokar.
          SET PARAMETER ID 'CV3' FIELD ls_aux-dokvr.
          SET PARAMETER ID 'CV4' FIELD ls_aux-doktl.
          CALL TRANSACTION 'CV03N' AND SKIP FIRST SCREEN.
        ENDIF.
      ELSEIF ls_selfield-fieldname = 'MIRO'.
        READ TABLE gt_alv INTO DATA(ls_aux2) INDEX ls_selfield-tabindex.
        IF sy-subrc IS INITIAL.
          SET PARAMETER ID 'RBN' FIELD ls_aux2-miro.
          SET PARAMETER ID 'GJR' FIELD ls_aux2-exer.
          CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    WHEN '&LOG'.
      IF gt_log[] IS NOT INITIAL.
        PERFORM zf_mostrar_log.
      ELSE.
        MESSAGE text-er3 TYPE 'I'.
      ENDIF.
  ENDCASE.

ENDFORM.


FORM zf_pf_status_set USING pf_tab TYPE slis_t_extab.

  SET PF-STATUS 'ZPF_STATUS' EXCLUDING pf_tab.

ENDFORM.


FORM zf_marcar_tudo.

  LOOP AT gt_alv INTO DATA(ls_alv).
    ls_alv-check = abap_true.
    MODIFY gt_alv FROM ls_alv.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ZF_PF_STATUS_SET'
      i_callback_user_command  = 'ZF_USER_COMMAND'
      i_callback_top_of_page   = 'ZF_TOP_OF_PAGE'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.


FORM zf_desmarcar_tudo.

  LOOP AT gt_alv INTO DATA(ls_alv).
    ls_alv-check = space.
    MODIFY gt_alv FROM ls_alv.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'ZF_PF_STATUS_SET'
      i_callback_user_command  = 'ZF_USER_COMMAND'
      i_callback_top_of_page   = 'ZF_TOP_OF_PAGE'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.


FORM zf_download_anexos.

  DATA: lt_documentdescriptions TYPE TABLE OF bapi_doc_drat,
        lt_documentfiles        TYPE TABLE OF bapi_doc_files2,
        lt_characteristicvalues TYPE TABLE OF bapi_characteristic_values,
        lt_classallocations     TYPE TABLE OF bapi_class_allocation,
        lt_co_documentfiles     TYPE TABLE OF bapi_doc_files2,
        lt_access               TYPE TABLE OF scms_acinf,
        lt_sdokcntbin           TYPE TABLE OF sdokcntbin.

  DATA: documentdata LIKE bapi_doc_draw2,
        return       LIKE bapiret2,
        documentfile TYPE bapi_doc_files2,
        v_storage    TYPE sdokstca-stor_cat.

  DATA: lv_doknr     TYPE draw-doknr,
        lv_full_path TYPE string,
        l_cont       TYPE c,
        l_offset     TYPE i,
        lv_path      TYPE bapi_doc_aux-filename.

  DATA ls_log TYPE ty_log.

  DATA(lt_alv_aux) = gt_alv[].
  DELETE lt_alv_aux WHERE check <> abap_true.

  CLEAR: gt_log[].

  IF lt_alv_aux[] IS NOT INITIAL.

    LOOP AT lt_alv_aux INTO DATA(ls_alv).

      CLEAR: documentdata, return, v_storage, lt_documentdescriptions[], lt_documentfiles[],
             lt_characteristicvalues[], ls_log, lt_classallocations[], lt_co_documentfiles[],
             lt_access[], lt_sdokcntbin[], l_cont.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_alv-doknr
        IMPORTING
          output = lv_doknr.

      READ TABLE gt_draw_all INTO DATA(ls_draw)
                              WITH KEY dokar = ls_alv-dokar
                                       doknr = lv_doknr
                                       dokvr = ls_alv-dokvr
                                       doktl = ls_alv-doktl
                                       BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
          EXPORTING
            documenttype         = ls_draw-dokar
            documentnumber       = ls_draw-doknr
            documentpart         = ls_draw-doktl
            documentversion      = ls_draw-dokvr
            getcomponents        = abap_true
            getdocdescriptions   = abap_true
            getdocfiles          = abap_true
            getclassification    = abap_true
          IMPORTING
            documentdata         = documentdata
            return               = return
          TABLES
            documentdescriptions = lt_documentdescriptions
            documentfiles        = lt_documentfiles
            characteristicvalues = lt_characteristicvalues
            classallocations     = lt_classallocations.

        IF lt_documentfiles[] IS NOT INITIAL AND return IS INITIAL.

          LOOP AT lt_documentfiles INTO DATA(ls_documentfile).

            ADD 1 TO l_cont.

            documentfile-documenttype    = ls_draw-dokar.
            documentfile-documentnumber  = ls_draw-doknr.
            documentfile-documentpart    = ls_draw-doktl.
            documentfile-documentversion = ls_draw-dokvr.
            documentfile-wsapplication   = ls_documentfile-wsapplication.

            lv_path = p_file.

            FIND ALL OCCURRENCES OF '\' IN ls_documentfile-docfile MATCH OFFSET l_offset.
            l_offset = l_offset + 1.

            DATA(lv_filename) = ls_documentfile-docfile+l_offset(20).

            CALL FUNCTION 'BAPI_DOCUMENT_CHECKOUTVIEW2'
              EXPORTING
                documenttype    = ls_draw-dokar
                documentnumber  = ls_draw-doknr
                documentpart    = ls_draw-doktl
                documentversion = ls_draw-dokvr
                documentfile    = documentfile
              IMPORTING
                return          = return
              TABLES
                documentfiles   = lt_co_documentfiles.

            IF return IS INITIAL AND lt_co_documentfiles[] IS NOT INITIAL.
              READ TABLE lt_co_documentfiles INTO DATA(ls_co_documentfiles) INDEX 1.
              IF sy-subrc IS INITIAL.
                v_storage = ls_co_documentfiles-storagecategory.

                CALL FUNCTION 'SCMS_DOC_READ'
                  EXPORTING
                    mandt       = sy-mandt
                    stor_cat    = v_storage
                    doc_id      = ls_co_documentfiles-file_id
                  TABLES
                    access_info = lt_access
                    content_bin = lt_sdokcntbin.

 ##FM_SUBRC_OK  IF sy-subrc IS INITIAL.

                  CLEAR lv_full_path.
                  CONCATENATE p_file '\' 'DOC_DMS_' ls_alv-doknr '-' ls_draw-dokar '_' l_cont '.PDF'
                         INTO lv_full_path.

                  CALL FUNCTION 'GUI_DOWNLOAD'
                    EXPORTING
                      filename                = lv_full_path
                      filetype                = 'BIN'
                      confirm_overwrite       = 'X'
                    TABLES
                      data_tab                = lt_sdokcntbin
                    EXCEPTIONS
                      file_write_error        = 1
                      no_batch                = 2
                      gui_refuse_filetransfer = 3
                      invalid_type            = 4
                      no_authority            = 5
                      unknown_error           = 6
                      header_not_allowed      = 7
                      separator_not_allowed   = 8
                      filesize_not_allowed    = 9
                      header_too_long         = 10
                      dp_error_create         = 11
                      dp_error_send           = 12
                      dp_error_write          = 13
                      unknown_dp_error        = 14
                      access_denied           = 15
                      dp_out_of_memory        = 16
                      disk_full               = 17
                      dp_timeout              = 18
                      file_not_found          = 19
                      dataprovider_exception  = 20
                      control_flush_error     = 21
                      OTHERS                  = 22.

                  IF sy-subrc <> 0.
                    ls_log-okcode = space.
                    ls_log-doknr = ls_alv-doknr.
                    ls_log-dokar = ls_alv-dokar.
                    ls_log-dokvr = ls_alv-dokvr.
                    ls_log-doktl = ls_alv-doktl.
                    ls_log-status = 'Erro no download!'.
                    APPEND ls_log TO gt_log.
                  ELSE.
                    ls_log-okcode = abap_true.
                    ls_log-doknr = ls_alv-doknr.
                    ls_log-dokar = ls_alv-dokar.
                    ls_log-dokvr = ls_alv-dokvr.
                    ls_log-doktl = ls_alv-doktl.
                    ls_log-status = 'Download efetuado com sucesso!'.
                    APPEND ls_log TO gt_log.
                  ENDIF.

                ENDIF.
              ENDIF.

            ELSE.
              ls_log-okcode = space.
              ls_log-doknr  = ls_alv-doknr.
              ls_log-dokar  = ls_alv-dokar.
              ls_log-dokvr  = ls_alv-dokvr.
              ls_log-doktl  = ls_alv-doktl.
              ls_log-status = 'Erro ao criar o arquivo.'.
              APPEND ls_log TO gt_log.
            ENDIF.

          ENDLOOP.

        ELSE.
          ls_log-okcode = space.
          ls_log-doknr  = ls_alv-doknr.
          ls_log-dokar  = ls_alv-dokar.
          ls_log-dokvr  = ls_alv-dokvr.
          ls_log-doktl  = ls_alv-doktl.
          ls_log-status = 'Não existe anexo!'.
          APPEND ls_log TO gt_log.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ELSE.
    MESSAGE text-er2 TYPE 'I'.
  ENDIF.

  IF gt_log[] IS NOT INITIAL.
    MESSAGE text-s01 TYPE 'I' DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.


FORM zf_mostrar_log.

  TYPES: BEGIN OF ty_itab_log,
           value(200) TYPE c,
         END OF ty_itab_log.

  DATA: lt_itab_log TYPE TABLE OF ty_itab_log,
        ls_itab_log TYPE ty_itab_log.

  DATA: l_lines TYPE i.

  CLEAR: l_lines, ls_itab_log.

  LOOP AT gt_log INTO DATA(ls_show_log).

    IF ls_show_log-okcode EQ abap_true.
      CONCATENATE 'Documento DMS:'
                  ls_show_log-doknr
                  'Tipo:'
                  ls_show_log-dokar
                  'Status:'
                  ls_show_log-status
                  INTO ls_itab_log-value
                  SEPARATED BY space.

      APPEND ls_itab_log TO lt_itab_log.
      CLEAR ls_itab_log.
    ELSE.
      CONCATENATE 'Documento DMS'
                  ls_show_log-doknr
                  'do tipo'
                  ls_show_log-dokar
                  'Status:'
                  ls_show_log-status
                  INTO ls_itab_log-value
                  SEPARATED BY space.

      APPEND ls_itab_log TO lt_itab_log.
      CLEAR ls_itab_log.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE lt_itab_log LINES l_lines.

  l_lines = l_lines + 1.

  CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
    EXPORTING
      endpos_col   = 100
      endpos_row   = l_lines
      startpos_col = 1
      startpos_row = 1
      titletext    = 'Log de Processamento'
    TABLES
      valuetab     = lt_itab_log
    EXCEPTIONS
      break_off    = 1
      OTHERS       = 2.

ENDFORM.
