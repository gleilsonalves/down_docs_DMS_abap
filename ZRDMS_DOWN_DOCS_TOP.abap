TABLES: lfa1, bkpf, bseg, t001, rbkp.

*tipos
TYPES: BEGIN OF ty_alv,
         check    TYPE c,
         dokar    TYPE draw-dokar, "Tipo Doc.
         doknr    TYPE draw-doknr, "Nº Doc.
         dokvr    TYPE draw-dokvr, "Versao Doc.
         doktl    TYPE draw-doktl, "Doc. Parcial
         dwnam    TYPE draw-dwnam, "Responsável
         dokst    TYPE draw-dokst, "Status Doc.
         dostx    TYPE tdwst-dostx, "Descrição Status do Doc.
         forn     TYPE lfa1-lifnr, "Fornecedor
         docref   TYPE bkpf-xblnr, "Referência
         date(10) TYPE c,          "Data documento
         venc(10) TYPE c,           "Vencimento
         emp      TYPE t001-bukrs, "Empresa
         miro     TYPE rbkp-belnr, "MIRO
         exer     TYPE bkpf-gjahr, "Exercício
         loedk    TYPE draw-loedk, "Marc. Eliminação
       END OF ty_alv,

       BEGIN OF ty_log,
         okcode      TYPE c,
         doknr       TYPE draw-doknr,
         dokar       TYPE draw-dokar,
         dokvr       TYPE draw-dokvr,
         doktl       TYPE draw-doktl,
         status(255) TYPE c,
       END OF ty_log.

* Tabelas internas
DATA: gt_alv             TYPE TABLE OF ty_alv,
      gt_log             TYPE TABLE OF ty_log,
      gt_class_selection TYPE TABLE OF comw,
      gt_draw            TYPE TABLE OF draw,
      gt_draw_all        TYPE TABLE OF draw,
      gt_tdwst           TYPE TABLE OF tdwst,
      gt_sel_char        TYPE TABLE OF sel_char,
      gt_cabn            TYPE TABLE OF cabn.

*Variáveis
DATA: gv_path_ini TYPE string,
      gv_path_sel TYPE string.

*Estruturas para ALV
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv.

*Constantes
CONSTANTS: c_class    TYPE bapi1003_key-classnum  VALUE 'ACC_PAY_001',
           c_class_tp TYPE bapi1003_key-classtype VALUE '017'.

* Tela de Seleção
SELECTION-SCREEN BEGIN OF BLOCK blc1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS: s_forn FOR lfa1-lifnr NO INTERVALS.
PARAMETER:      p_ref  TYPE bkpf-xblnr.
SELECT-OPTIONS: s_dat  FOR bkpf-bldat NO INTERVALS,
                s_venc FOR bseg-zfbdt NO INTERVALS,
                s_emp  FOR t001-bukrs NO INTERVALS.
PARAMETER:      p_miro TYPE rbkp-belnr,
                p_exer TYPE bkpf-gjahr.

SELECTION-SCREEN SKIP.

PARAMETER: p_file  TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blc1.


*Seleção da pasta para download dos arquivos
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      initial_folder  = gv_path_ini
    CHANGING
      selected_folder = gv_path_sel
    EXCEPTIONS
      cntl_error      = 1
      error_no_gui    = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    CALL METHOD cl_gui_cfw=>flush( ).
    IF NOT gv_path_sel IS INITIAL.
      gv_path_ini = p_file = gv_path_sel.
    ENDIF.
  ENDIF.
