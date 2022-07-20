REPORT zrdms_down_docs.

INCLUDE zrdms_down_docs_top.
INCLUDE zrdms_down_docs_f01.

START-OF-SELECTION.

  PERFORM zf_busca_dados.

  IF gt_draw[] IS NOT INITIAL.
    PERFORM zf_monta_saida.
    IF gt_alv[] IS NOT INITIAL.
      PERFORM zf_chama_alv.
    ENDIF.
  ENDIF.
